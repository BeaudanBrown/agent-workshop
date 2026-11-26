{-# LANGUAGE OverloadedStrings #-}

{- | Audio and image work done with ffmpeg, ffprobe and curl.

Every external program comes from the Nix wrapper as an absolute path.
Outputs are written to a partial file and renamed, so a failed run never
leaves a file at the target path.
-}
module Workshop.Abrec.Media (
    Tools (..),
    loadTools,
    Encoded (..),
    presetId,
    probeAudio,
    encodeChapter,
    cachedEncode,
    normaliseCover,
    titleCard,
    wrapTitle,
    fetchHttps,
    trimWindow,
    SilenceEvent (..),
) where

import Control.Exception (IOException, try)
import Control.Monad (when)
import Data.Aeson (FromJSON (..), ToJSON (..), decodeStrict', eitherDecodeFileStrict', encodeFile, object, withObject, (.:), (.=))
import Data.ByteString.Char8 qualified as BC
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Numeric (showFFloat)
import System.Directory (createDirectoryIfMissing, doesFileExist, getFileSize, removeFile, renameFile)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..), die)
import System.FilePath (isAbsolute, (<.>), (</>))
import System.Process (readProcessWithExitCode)
import Text.Read (readMaybe)
import Workshop.Cli.Env (configuredExecutable)

data Tools = Tools
    { toolFfmpeg :: FilePath
    , toolFfprobe :: FilePath
    , toolCurl :: FilePath
    , toolFont :: FilePath
    }

-- | Read the programs and title-card font the Nix wrapper provides.
loadTools :: IO Tools
loadTools = do
    ffmpeg <- configuredExecutable "abrec" "ABREC_FFMPEG" "ffmpeg"
    ffprobe <- configuredExecutable "abrec" "ABREC_FFPROBE" "ffprobe"
    curl <- configuredExecutable "abrec" "ABREC_CURL" "curl"
    font <- lookupEnv "ABREC_FONT"
    case font of
        Just path | isAbsolute path -> do
            present <- doesFileExist path
            if present then pure (Tools ffmpeg ffprobe curl path) else die ("abrec: ABREC_FONT does not exist: " ++ path)
        _ -> die "abrec: configure ABREC_FONT with an absolute path to a font file."

-- | An encoded chapter's facts for feeds.
data Encoded = Encoded
    { encodedBytes :: Integer
    , encodedSeconds :: Double
    }
    deriving (Eq, Show)

instance ToJSON Encoded where
    toJSON encoded = object ["bytes" .= encodedBytes encoded, "seconds" .= encodedSeconds encoded]

instance FromJSON Encoded where
    parseJSON = withObject "Encoded" $ \obj -> Encoded <$> obj .: "bytes" <*> obj .: "seconds"

{- | Identifies the encoding settings below. Change it whenever they change,
so cached encodes made with older settings are not reused.
-}
presetId :: Text
presetId = "mp3-64k-mono-44k-lufs16-tp15-trim05-v1"

-- | The duration of a file with at least one audio stream.
probeAudio :: Tools -> FilePath -> IO (Either Text Double)
probeAudio tools path = do
    result <- run (toolFfprobe tools) ["-v", "error", "-show_entries", "stream=codec_type:format=duration", "-of", "default=noprint_wrappers=1", path]
    pure $ do
        out <- either (const (Left "This file is not audio that ffmpeg can read.")) Right result
        let fields = mapMaybe keyValue (lines out)
        if ("codec_type", "audio") `notElem` fields
            then Left "This file has no audio."
            else case lookup "duration" fields >>= readMaybe of
                Just seconds | seconds > 0 -> Right seconds
                _ -> Left "This file's duration cannot be read."

-- | Silence boundaries reported by ffmpeg's silencedetect filter.
data SilenceEvent = SilenceStart Double | SilenceEnd Double
    deriving (Eq, Show)

{- | The part of a recording to keep: leading and trailing silence removed,
leaving half a second of it at each end. Silence in the middle stays.
-}
trimWindow :: Double -> [SilenceEvent] -> (Double, Double)
trimWindow duration events
    | end - start < 1 = (0, duration)
    | otherwise = (start, end)
  where
    start = case events of
        SilenceStart s : SilenceEnd e : _ | s <= 0.1 -> max 0 (e - 0.5)
        _ -> 0
    end = case reverse events of
        SilenceStart s : _ -> min duration (s + 0.5)
        SilenceEnd e : SilenceStart s : _ | e >= duration - 0.1 && s > 0.1 -> min duration (s + 0.5)
        _ -> duration

{- | Encode a master to the feed format: trimmed, loudness normalised in two
passes to −16 LUFS integrated and −1.5 dBTP, mono 44.1 kHz MP3 at 64 kb/s.
-}
encodeChapter :: Tools -> FilePath -> Double -> FilePath -> IO (Either Text Encoded)
encodeChapter tools master duration target = do
    analysis <-
        readProcessWithExitCode
            (toolFfmpeg tools)
            ["-nostdin", "-hide_banner", "-nostats", "-i", master, "-vn", "-af", "silencedetect=noise=-50dB:d=1," <> loudnorm <> ":print_format=json", "-f", "null", "-"]
            ""
    case analysis of
        (ExitFailure _, _, err) -> pure (Left ("ffmpeg could not analyse the audio: " <> lastLines err))
        (ExitSuccess, _, err) -> do
            let (start, end) = trimWindow duration (silenceEvents err)
                partial = target <.> "partial"
                filters = maybe loudnorm (\measured -> loudnorm <> ":" <> measured <> ":linear=true") (measurements err)
            encoded <-
                run
                    (toolFfmpeg tools)
                    [ "-nostdin"
                    , "-hide_banner"
                    , "-loglevel"
                    , "error"
                    , "-y"
                    , "-ss"
                    , showSeconds start
                    , "-t"
                    , showSeconds (end - start)
                    , "-i"
                    , master
                    , "-vn"
                    , "-map_metadata"
                    , "-1"
                    , "-af"
                    , filters <> ":print_format=none,aresample=44100"
                    , "-ac"
                    , "1"
                    , "-c:a"
                    , "libmp3lame"
                    , "-b:a"
                    , "64k"
                    , "-f"
                    , "mp3"
                    , partial
                    ]
            case encoded of
                Left err' -> do
                    removeIfPresent partial
                    pure (Left ("ffmpeg could not encode the audio: " <> err'))
                Right _ -> do
                    probed <- probeAudio tools partial
                    size <- getFileSize partial
                    case probed of
                        Left err' -> removeIfPresent partial >> pure (Left err')
                        Right length' -> do
                            renameFile partial target
                            pure (Right (Encoded size length'))
  where
    loudnorm = "loudnorm=I=-16:TP=-1.5:LRA=11"

{- | Encode through a cache keyed by the master's hash and 'presetId'. Returns
the cached MP3 path.
-}
cachedEncode :: Tools -> FilePath -> Text -> FilePath -> Double -> IO (Either Text (FilePath, Encoded))
cachedEncode tools cache digest master duration = do
    let base = cache </> T.unpack (digest <> "-" <> presetId)
        audio = base <.> "mp3"
        facts = base <.> "json"
    createDirectoryIfMissing True cache
    present <- (&&) <$> doesFileExist audio <*> doesFileExist facts
    cached <- if present then either (const Nothing) Just <$> eitherDecodeFileStrict' facts else pure Nothing
    case cached of
        Just encoded -> pure (Right (audio, encoded))
        Nothing -> do
            result <- encodeChapter tools master duration audio
            case result of
                Left err -> pure (Left err)
                Right encoded -> do
                    encodeFile (facts <.> "partial") encoded
                    renameFile (facts <.> "partial") facts
                    pure (Right (audio, encoded))

silenceEvents :: String -> [SilenceEvent]
silenceEvents = mapMaybe event . lines
  where
    event line = case words (dropWhile (/= ']') line) of
        "]" : "silence_start:" : value : _ -> SilenceStart <$> readMaybe value
        "]" : "silence_end:" : value : _ -> SilenceEnd <$> readMaybe value
        _ -> Nothing

-- | The first-pass loudnorm measurements as filter options, if all are finite.
measurements :: String -> Maybe String
measurements err = do
    let afterBrace = reverse (takeWhile (/= '{') (reverse err))
        block = '{' : takeWhile (/= '}') afterBrace ++ "}"
    fields <- decodeStrict' (BC.pack block) :: Maybe (Map.Map String String)
    values <- mapM (\key -> Map.lookup key fields >>= finite) ["input_i", "input_tp", "input_lra", "input_thresh", "target_offset"]
    case values of
        [i, tp, lra, thresh, offset] ->
            Just ("measured_I=" <> i <> ":measured_TP=" <> tp <> ":measured_LRA=" <> lra <> ":measured_thresh=" <> thresh <> ":offset=" <> offset)
        _ -> Nothing
  where
    finite value = case readMaybe value :: Maybe Double of
        Just number | not (isInfinite number || isNaN number) -> Just value
        _ -> Nothing

-- | Convert an image to a square 1400 px JPEG, cropping to the centre.
normaliseCover :: Tools -> FilePath -> FilePath -> IO (Either Text ())
normaliseCover tools source target = do
    codec <- run (toolFfprobe tools) ["-v", "error", "-select_streams", "v:0", "-show_entries", "stream=codec_name", "-of", "default=noprint_wrappers=1:nokey=1", source]
    case fmap (filter (not . null) . lines) codec of
        Right [name] | name `elem` imageCodecs -> writeJpeg tools ["-i", source, "-frames:v", "1", "-vf", "scale=1400:1400:force_original_aspect_ratio=increase,crop=1400:1400,format=yuvj420p"] target
        _ -> pure (Left "This file is not an image.")
  where
    imageCodecs = ["mjpeg", "png", "webp", "gif", "bmp", "tiff"]

-- | A plain 1400 px card with a title and optional author.
titleCard :: Tools -> Text -> Maybe Text -> FilePath -> IO (Either Text ())
titleCard tools title author target = do
    let (size, lines') = wrapTitle title
        titleFile = target <.> "title.txt"
        authorFile = target <.> "author.txt"
        font = "fontfile=" <> toolFont tools <> ":expansion=none:x=(w-text_w)/2"
        titleFilter = "drawtext=" <> font <> ":textfile=" <> titleFile <> ":fontcolor=white:fontsize=" <> show size <> ":line_spacing=24:text_align=center:y=(h-text_h)/2-80"
        authorFilter = "drawtext=" <> font <> ":textfile=" <> authorFile <> ":fontcolor=0xcad2c5:fontsize=64:y=h-300"
    TIO.writeFile titleFile (T.intercalate "\n" lines')
    mapM_ (TIO.writeFile authorFile . T.take 40) author
    result <-
        writeJpeg
            tools
            ["-f", "lavfi", "-i", "color=c=0x2f3e46:s=1400x1400", "-frames:v", "1", "-vf", titleFilter <> maybe "" (const ("," <> authorFilter)) author]
            target
    mapM_ removeIfPresent [titleFile, authorFile]
    pure result

-- | Font size and lines for a title card: shorter titles get larger text.
wrapTitle :: Text -> (Int, [Text])
wrapTitle title
    | length large <= 5 = (110, large)
    | otherwise = (80, take 8 (wrap 26))
  where
    large = wrap 18
    wrap width = foldr place [] (T.words title)
      where
        place word (line : rest) | T.length word + 1 + T.length line <= width = (word <> " " <> line) : rest
        place word acc = word : acc

writeJpeg :: Tools -> [String] -> FilePath -> IO (Either Text ())
writeJpeg tools arguments target = do
    let partial = target <.> "partial"
    result <- run (toolFfmpeg tools) (["-nostdin", "-hide_banner", "-loglevel", "error", "-y"] ++ arguments ++ ["-c:v", "mjpeg", "-q:v", "2", "-f", "image2", "-update", "1", partial])
    case result of
        Left err -> removeIfPresent partial >> pure (Left ("ffmpeg could not write the image: " <> err))
        Right _ -> Right <$> renameFile partial target

-- | Download an HTTPS URL, refusing other schemes, redirects to them and large files.
fetchHttps :: Tools -> Text -> FilePath -> IO (Either Text ())
fetchHttps tools url target
    | not ("https://" `T.isPrefixOf` url) = pure (Left "Only https:// addresses can be fetched.")
    | otherwise = do
        let partial = target <.> "partial"
        result <-
            run
                (toolCurl tools)
                ["--fail", "--silent", "--show-error", "--location", "--proto", "=https", "--proto-redir", "=https", "--max-time", "30", "--max-filesize", "20000000", "--output", partial, "--", T.unpack url]
        case result of
            Left err -> removeIfPresent partial >> pure (Left ("The address could not be fetched: " <> err))
            Right _ -> Right <$> renameFile partial target

-- Helpers --------------------------------------------------------------

-- | Run a program, returning stdout or the last lines of stderr.
run :: FilePath -> [String] -> IO (Either Text String)
run program arguments = do
    result <- try (readProcessWithExitCode program arguments "")
    pure $ case result of
        Left (err :: IOException) -> Left (T.pack (show err))
        Right (ExitSuccess, out, _) -> Right out
        Right (ExitFailure _, _, err) -> Left (lastLines err)

lastLines :: String -> Text
lastLines output = case filter (any (/= ' ')) (lines output) of
    [] -> "no diagnostic output"
    diagnostic -> T.pack (unwords (reverse (take 3 (reverse diagnostic))))

keyValue :: String -> Maybe (String, String)
keyValue line = case break (== '=') line of
    (key, '=' : value) -> Just (key, value)
    _ -> Nothing

showSeconds :: Double -> String
showSeconds value = showFFloat (Just 3) value ""

removeIfPresent :: FilePath -> IO ()
removeIfPresent path = do
    present <- doesFileExist path
    when present (removeFile path)
