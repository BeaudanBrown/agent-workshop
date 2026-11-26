{-# LANGUAGE OverloadedStrings #-}

{- | Normalise media with ffmpeg so every page embeds the same formats.

Each preset has an identifier that is part of its cache key; changing a
preset's arguments requires a new identifier so stale cache entries are never
reused.
-}
module Workshop.MediaPage.Media (
    Tools (..),
    SourceKind (..),
    Encoded (..),
    Embedded (..),
    sourceKind,
    supportedExtensions,
    encodeMedia,
) where

import Control.Exception (IOException, try)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BC
import Data.Char (toLower)
import Data.Maybe (mapMaybe)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile, renameFile)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, takeExtension, takeFileName, (</>))
import System.Process (getCurrentPid, readProcessWithExitCode)
import Text.Read (readMaybe)

data Tools = Tools
    { toolFfmpeg :: FilePath
    , toolFfprobe :: FilePath
    }

-- | What a source file is, judged from its extension.
data SourceKind = StillImage | Gif | Clip
    deriving (Eq, Show)

-- | How the encoded media is embedded in the page.
data Embedded = EmbeddedImage | EmbeddedVideo {videoLoops :: Bool}
    deriving (Eq, Show)

data Encoded = Encoded
    { encodedAs :: Embedded
    , encodedMime :: String
    , encodedBytes :: ByteString
    , encodedWidth :: Int
    , encodedHeight :: Int
    }
    deriving (Eq, Show)

supportedExtensions :: [(String, SourceKind)]
supportedExtensions =
    [ ("png", StillImage)
    , ("jpg", StillImage)
    , ("jpeg", StillImage)
    , ("webp", StillImage)
    , ("gif", Gif)
    , ("webm", Clip)
    , ("mp4", Clip)
    , ("mov", Clip)
    ]

sourceKind :: FilePath -> Maybe SourceKind
sourceKind path = lookup (map toLower (drop 1 (takeExtension path))) supportedExtensions

data Preset = Preset
    { presetId :: String
    , presetExtension :: String
    , presetMime :: String
    , presetEmbedded :: Embedded
    , presetArguments :: Bool -> [String]
    -- ^ ffmpeg output options, given whether the source has audio
    }

imagePreset :: Preset
imagePreset =
    Preset "image-webp-q90-1920-v1" ".webp" "image/webp" EmbeddedImage $
        const
            [ "-frames:v"
            , "1"
            , "-vf"
            , "scale='min(1920,iw)':'min(1920,ih)':force_original_aspect_ratio=decrease:flags=lanczos"
            , "-c:v"
            , "libwebp"
            , "-quality"
            , "90"
            ]

videoArguments :: Bool -> [String]
videoArguments audio =
    ["-map", "0:v:0"]
        ++ (if audio then ["-map", "0:a:0", "-c:a", "aac", "-b:a", "96k"] else ["-an"])
        ++ [ "-vf"
           , "fps=15,scale='trunc(min(1280,iw)/2)*2':-2:flags=lanczos,format=yuv420p"
           , "-c:v"
           , "libx264"
           , "-preset"
           , "slow"
           , "-tune"
           , "stillimage"
           , "-crf"
           , "28"
           , "-movflags"
           , "+faststart"
           ]

videoPreset :: Preset
videoPreset = Preset "video-h264-crf28-15fps-1280-v1" ".mp4" "video/mp4" (EmbeddedVideo False) videoArguments

loopPreset :: Preset
loopPreset = Preset "loop-h264-crf28-15fps-1280-v1" ".mp4" "video/mp4" (EmbeddedVideo True) (const (videoArguments False))

{- | Encode a source file, reusing the cached result for the same source bytes
and preset. Failures are messages suitable for a FILE:LINE prefix.
-}
encodeMedia :: Tools -> FilePath -> FilePath -> SourceKind -> IO (Either String Encoded)
encodeMedia tools cacheDir source kind = do
    readResult <- try (BS.readFile source) :: IO (Either IOException ByteString)
    case readResult of
        Left err -> pure (Left ("cannot read " ++ source ++ ": " ++ show err))
        Right sourceBytes -> do
            sourceProbe <- probe tools source (kind == Gif)
            case sourceProbe of
                Left err -> pure (Left err)
                Right info -> do
                    let preset = case kind of
                            StillImage -> imagePreset
                            Clip -> videoPreset
                            Gif
                                | maybe False (> 1) (probeFrames info) -> loopPreset
                                | otherwise -> imagePreset
                        key = BC.unpack (Base16.encode (SHA256.hash sourceBytes)) ++ "-" ++ presetId preset
                        cached = cacheDir </> key ++ presetExtension preset
                    if probeHasVideo info
                        then do
                            createDirectoryIfMissing True cacheDir
                            present <- doesFileExist cached
                            encoded <- if present then pure (Right ()) else encodeTo tools preset (probeHasAudio info) source cached
                            either (pure . Left) (const (finish preset cached)) encoded
                        else pure (Left (source ++ " contains no image or video stream"))
  where
    finish preset cached = do
        outputProbe <- probe tools cached False
        bytes <- BS.readFile cached
        pure $ do
            info <- outputProbe
            case (probeWidth info, probeHeight info) of
                (Just width, Just height) -> Right (Encoded (presetEmbedded preset) (presetMime preset) bytes width height)
                _ -> Left ("ffmpeg produced no dimensions for " ++ source)

encodeTo :: Tools -> Preset -> Bool -> FilePath -> FilePath -> IO (Either String ())
encodeTo tools preset audio source target = do
    pid <- getCurrentPid
    -- A named partial file, not openTempFile: its private mode would survive the
    -- rename and lock other users out of the shared cache.
    let partial = takeDirectory target </> (".partial-" ++ show pid ++ "-" ++ takeFileName target)
    let arguments =
            ["-nostdin", "-hide_banner", "-loglevel", "error", "-y", "-i", source]
                ++ presetArguments preset audio
                ++ [partial]
    (code, _, err) <- readProcessWithExitCode (toolFfmpeg tools) arguments ""
    case code of
        ExitSuccess -> Right <$> renameFile partial target
        ExitFailure _ -> do
            removeFile partial
            pure (Left ("ffmpeg could not encode " ++ source ++ ": " ++ lastLines err))

data Probe = Probe
    { probeHasVideo :: Bool
    , probeHasAudio :: Bool
    , probeWidth :: Maybe Int
    , probeHeight :: Maybe Int
    , probeFrames :: Maybe Int
    }

-- | Read stream facts; counting frames needs a full demux, so only GIFs ask for it.
probe :: Tools -> FilePath -> Bool -> IO (Either String Probe)
probe tools path countFrames = do
    let arguments =
            ["-v", "error"]
                ++ ["-count_packets" | countFrames]
                ++ ["-show_entries", "stream=codec_type,width,height,nb_read_packets", "-of", "compact=p=0", path]
    (code, out, err) <- readProcessWithExitCode (toolFfprobe tools) arguments ""
    pure $ case code of
        ExitFailure _ -> Left ("ffprobe could not read " ++ path ++ ": " ++ lastLines err)
        ExitSuccess ->
            let streams = map fields (filter (not . null) (lines out))
                videos = filter ((== Just "video") . lookup "codec_type") streams
                firstVideo = case videos of
                    stream : _ -> stream
                    [] -> []
                number key = lookup key firstVideo >>= readMaybe
             in Right
                    Probe
                        { probeHasVideo = not (null videos)
                        , probeHasAudio = any ((== Just "audio") . lookup "codec_type") streams
                        , probeWidth = number "width"
                        , probeHeight = number "height"
                        , probeFrames = number "nb_read_packets"
                        }
  where
    fields line = mapMaybe field (splitOn '|' line)
    field item = case break (== '=') item of
        (key, '=' : value) -> Just (key, value)
        _ -> Nothing

splitOn :: Char -> String -> [String]
splitOn separator text = case break (== separator) text of
    (chunk, []) -> [chunk]
    (chunk, _ : rest) -> chunk : splitOn separator rest

-- | The last few diagnostic lines of a failed ffmpeg or ffprobe run.
lastLines :: String -> String
lastLines output = case filter (any (/= ' ')) (lines output) of
    [] -> "no diagnostic output"
    diagnostic -> unwords (reverse (take 3 (reverse diagnostic)))
