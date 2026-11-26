{-# LANGUAGE OverloadedStrings #-}

-- | Command-line entry point: arguments, environment and help text.
module Workshop.MediaPage.Cli (main) where

import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Version (showVersion)
import Paths_media_page (version)
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode (..), die, exitWith)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)
import Workshop.Cli.Env (configuredExecutable)
import Workshop.MediaPage.Build (Config (..), buildPage, defaultMaxBytes)
import Workshop.MediaPage.Media (Tools (..))

-- | Version of the source format accepted by this build.
formatVersion :: Int
formatVersion = 1

data Command = Help ExitCode | Version | Build FilePath (Maybe FilePath)

main :: IO ()
main = getArgs >>= either usageError run . parseArguments

parseArguments :: [String] -> Either String Command
parseArguments arguments = case arguments of
    [] -> Right (Help (ExitFailure 2))
    ["--help"] -> Right (Help ExitSuccess)
    ["-h"] -> Right (Help ExitSuccess)
    ["--version"] -> Right Version
    [input] | not (isFlag input) -> Right (Build input Nothing)
    [input, "-o", output] | not (isFlag input) -> Right (Build input (Just output))
    ["-o", output, input] | not (isFlag input) -> Right (Build input (Just output))
    _ -> Left ("unexpected arguments: " ++ unwords arguments)
  where
    isFlag argument = take 1 argument == "-"

usageError :: String -> IO ()
usageError message = do
    hPutStrLn stderr ("media-page: " ++ message)
    hPutStrLn stderr "Usage: media-page FILE [-o OUTPUT.html]   (media-page --help explains FILE)"
    exitWith (ExitFailure 2)

run :: Command -> IO ()
run command = case command of
    Help code -> putStr helpText >> exitWith code
    Version -> putStrLn footer
    Build input output -> do
        ffmpeg <- configuredExecutable "media-page" "MEDIA_PAGE_FFMPEG" "ffmpeg"
        ffprobe <- configuredExecutable "media-page" "MEDIA_PAGE_FFPROBE" "ffprobe"
        outDir <- fromMaybe "/tmp/media-page" . nonEmpty <$> lookupEnv "MEDIA_PAGE_OUT_DIR"
        maxBytes <- lookupEnv "MEDIA_PAGE_MAX_BYTES" >>= maybe (pure defaultMaxBytes) positive . nonEmpty
        let config = Config (Tools ffmpeg ffprobe) outDir maxBytes output (T.pack footer)
        result <- buildPage config input
        case result of
            Left errs -> mapM_ (hPutStrLn stderr) errs >> exitWith (ExitFailure 1)
            Right path -> putStrLn path
  where
    nonEmpty value = case value of
        Just "" -> Nothing
        _ -> value
    positive text = case readMaybe text of
        Just bytes | bytes > 0 -> pure bytes
        _ -> die "media-page: MEDIA_PAGE_MAX_BYTES must be a positive number of bytes."

footer :: String
footer = "media-page " ++ showVersion version ++ " · format " ++ show formatVersion

helpText :: String
helpText =
    unlines
        [ "Usage: media-page FILE [-o OUTPUT.html]"
        , ""
        , "Builds one self-contained HTML page from FILE, embedding every image and"
        , "video, and prints the page's path. Problems are reported as FILE:LINE."
        , ""
        , "Example FILE:"
        , ""
        , "---"
        , "title: Unavailability changes"
        , "meta: Staging f5bd4287 to 7f90ccdf"
        , "date: 2026-11-26"
        , "---"
        , "# Requesting time off"
        , "Staff see one confirmation when a request hits both a blackout and the"
        , "daily maximum. Consecutive lines join into one paragraph."
        , ""
        , "![One confirmation covers the blackout and the daily maximum](demo.webm)"
        , ""
        , "# Install Bepis"
        , "::: tabs"
        , "## iPhone"
        , "![Add to Home Screen in Safari](ios.png)"
        , "## Android"
        , "![Install from the browser menu](android.png)"
        , ":::"
        , ""
        , "Rules:"
        , "- The header sits between '---' lines as 'key: value'. title is required;"
        , "  meta and date are optional."
        , "- '# Heading' starts a section. Every paragraph, media line and tab group"
        , "  belongs to a section."
        , "- Paragraphs are plain text separated by blank lines. Markdown markup such as"
        , "  bold, links, lists, code, quotes and tables is rejected."
        , "- '![caption](file)' on its own line adds media; the caption is required."
        , "  Paths are relative to FILE. Images: png, jpg, jpeg, webp. Video: webm,"
        , "  mp4, mov. An animated gif plays as a looping video."
        , "- '::: tabs' opens a tab group and ':::' closes it. Each tab starts with"
        , "  '## label' followed by its paragraphs and media. Tab groups do not nest."
        , ""
        , "Media is re-encoded and cached: video to H.264 MP4 at 15 fps, at most"
        , "1280 px wide; images to WebP, at most 1920 px on the longest side."
        , ""
        , "Environment:"
        , "  MEDIA_PAGE_OUT_DIR    directory for pages (default /tmp/media-page); -o overrides"
        , "  MEDIA_PAGE_MAX_BYTES  largest page allowed, in bytes (default 67108864)"
        ]
