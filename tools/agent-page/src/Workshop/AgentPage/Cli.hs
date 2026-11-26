{-# LANGUAGE OverloadedStrings #-}

-- | Command-line entry point: arguments, environment and help text.
module Workshop.AgentPage.Cli (main) where

import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Version (showVersion)
import Paths_agent_page (version)
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode (..), die, exitWith)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)
import Workshop.AgentPage.Build (Config (..), buildPage, defaultMaxBytes)
import Workshop.AgentPage.Media (Tools (..))
import Workshop.Cli.Env (configuredExecutable)

-- | Version of the source format accepted by this build.
formatVersion :: Int
formatVersion = 2

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
    hPutStrLn stderr ("agent-page: " ++ message)
    hPutStrLn stderr "Usage: agent-page FILE [-o OUTPUT.html]   (agent-page --help explains FILE)"
    exitWith (ExitFailure 2)

run :: Command -> IO ()
run command = case command of
    Help code -> putStr helpText >> exitWith code
    Version -> putStrLn footer
    Build input output -> do
        ffmpeg <- configuredExecutable "agent-page" "AGENT_PAGE_FFMPEG" "ffmpeg"
        ffprobe <- configuredExecutable "agent-page" "AGENT_PAGE_FFPROBE" "ffprobe"
        outDir <- fromMaybe "/tmp/agent-page" . nonEmpty <$> lookupEnv "AGENT_PAGE_OUT_DIR"
        maxBytes <- lookupEnv "AGENT_PAGE_MAX_BYTES" >>= maybe (pure defaultMaxBytes) positive . nonEmpty
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
        _ -> die "agent-page: AGENT_PAGE_MAX_BYTES must be a positive number of bytes."

footer :: String
footer = "agent-page " ++ showVersion version ++ " · format " ++ show formatVersion

helpText :: String
helpText =
    unlines
        [ "Usage: agent-page FILE [-o OUTPUT.html]"
        , ""
        , "Builds one self-contained HTML page from FILE and prints the page's path."
        , "A page presents images and video, asks the reader questions, or both."
        , "Every image and video is embedded. Problems are reported as FILE:LINE."
        , ""
        , "Example FILE:"
        , ""
        , "---"
        , "title: Unavailability changes"
        , "meta: Staging f5bd4287 to 7f90ccdf"
        , "date: 2026-11-26"
        , "reply: on"
        , "---"
        , "Text before the first heading opens the page."
        , ""
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
        , "# Decisions"
        , "::: choice"
        , "Which confirmation layout should ship?"
        , ""
        , "![Both layouts side by side](layouts.png)"
        , ""
        , "- Stacked (recommended)"
        , "- Side by side"
        , ":::"
        , ""
        , "::: multi"
        , "Which devices should the install guide cover first?"
        , "- iPhone"
        , "- Android"
        , "- Windows"
        , ":::"
        , ""
        , "::: text"
        , "What should the confirmation say instead?"
        , ":::"
        , ""
        , "Rules:"
        , "- The header sits between '---' lines as 'key: value'. title is required;"
        , "  meta, date and reply are optional."
        , "- '# Heading' starts a section. Content before the first heading has none."
        , "- Paragraphs are plain text separated by blank lines. Markdown markup such as"
        , "  bold, links, lists, code, quotes and tables is rejected."
        , "- '![caption](file)' on its own line adds media; the caption is required."
        , "  Paths are relative to FILE. Images: png, jpg, jpeg, webp. Video: webm,"
        , "  mp4, mov. An animated gif plays as a looping video."
        , "- '::: tabs' opens a tab group and ':::' closes it. Each tab starts with"
        , "  '## label' followed by its paragraphs, media and questions. Tab groups do"
        , "  not nest."
        , "- '::: choice' asks for one option, '::: multi' for any number and"
        , "  '::: text' for free text; ':::' closes the question. Its first paragraph is"
        , "  the question. Paragraphs and media may follow as context. Options come"
        , "  last, one '- label' line each: at least two for choice and multi, none for"
        , "  text. ' (recommended)' after an option marks it without selecting it; a"
        , "  choice recommends at most one. Questions are numbered Q1, Q2... in order."
        , "  Choice and multi questions always offer Other and an optional note."
        , "- 'reply: on' adds a reply box under every image, video and section."
        , "- A page with questions or 'reply: on' ends with an overall reply box and a"
        , "  Copy reply button. The copied text names each answer's FILE:LINE and quotes"
        , "  its question or caption; the reader pastes it back into the chat."
        , "  Unanswered questions are listed as skipped. Nothing is saved, and replying"
        , "  needs scripts: previews that strip them show a notice instead."
        , ""
        , "Media is re-encoded and cached: video to H.264 MP4 at 15 fps, at most"
        , "1280 px wide; images to WebP, at most 1920 px on the longest side."
        , ""
        , "Environment:"
        , "  AGENT_PAGE_OUT_DIR    directory for pages (default /tmp/agent-page); -o overrides"
        , "  AGENT_PAGE_MAX_BYTES  largest page allowed, in bytes (default 67108864)"
        ]
