{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM_, unless)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy qualified as BL
import Data.List (isInfixOf, isPrefixOf, sort)
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (doesDirectoryExist, getDirectoryContents, getModificationTime, listDirectory, removeDirectoryRecursive)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..), die)
import System.FilePath (takeDirectory, takeExtension, takeFileName, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (fileMode, getFileStatus, setFileCreationMask)
import System.Process (readProcessWithExitCode)
import Workshop.AgentPage.Build (Config (..), buildPage, defaultMaxBytes)
import Workshop.AgentPage.Document
import Workshop.AgentPage.Media (Embedded (..), Encoded (..), Tools (..))
import Workshop.AgentPage.Parse (ParseError (..), parseDocument)
import Workshop.AgentPage.Render (Placed (..), renderPage)
import Workshop.Test (assert, equal)

main :: IO ()
main = do
    parserTests
    goldenTests
    pipelineTests

-- Parser ---------------------------------------------------------------

sample :: Text
sample =
    T.unlines
        [ "---"
        , "title: Demo page"
        , "meta: Synthetic fixture"
        , "date: 2026-11-26"
        , "---"
        , "# First section"
        , "Plain text that"
        , "continues here."
        , ""
        , "![A synthetic clip](clip.webm)"
        , ""
        , "# Second section"
        , "::: tabs"
        , "## One"
        , "![First still](one.png)"
        , "## Two"
        , "Second panel text."
        , ":::"
        ]

questionsSample :: Text
questionsSample =
    T.unlines
        [ "---"
        , "title: Questions"
        , "reply: on"
        , "---"
        , "Lead text."
        , ""
        , "::: choice"
        , "Which layout?"
        , ""
        , "Context paragraph."
        , "![Both layouts](both.png)"
        , "- Compact (recommended)"
        , "- Spacious"
        , ":::"
        , "# Section"
        , "::: tabs"
        , "## Tab"
        , "::: multi"
        , "Pick any"
        , "- A"
        , "- B"
        , ":::"
        , ":::"
        , "::: text"
        , "Anything else?"
        , ":::"
        ]

parserTests :: IO ()
parserTests = do
    putStrLn "agent-page: parser"
    equal
        "sample document"
        ( Right
            ( Document
                "Demo page"
                (Just "Synthetic fixture")
                (Just "2026-11-26")
                False
                [ Section (Just "First section") 6 [Paragraph "Plain text that continues here.", Media (MediaRef "A synthetic clip" "clip.webm" 10)]
                , Section
                    (Just "Second section")
                    12
                    [Tabs [Tab "One" [Media (MediaRef "First still" "one.png" 15)], Tab "Two" [Paragraph "Second panel text."]]]
                ]
            )
        )
        (parseDocument sample)
    equal
        "questions document"
        ( Right
            ( Document
                "Questions"
                Nothing
                Nothing
                True
                [ Section
                    Nothing
                    5
                    [ Paragraph "Lead text."
                    , Ask
                        ( Question
                            SingleChoice
                            8
                            "Which layout?"
                            [Paragraph "Context paragraph.", Media (MediaRef "Both layouts" "both.png" 11)]
                            [Choice "Compact" True, Choice "Spacious" False]
                        )
                    ]
                , Section
                    (Just "Section")
                    15
                    [ Tabs [Tab "Tab" [Ask (Question MultipleChoice 19 "Pick any" [] [Choice "A" False, Choice "B" False])]]
                    , Ask (Question FreeText 25 "Anything else?" [] [])
                    ]
                ]
            )
        )
        (parseDocument questionsSample)
    equal
        "lists, code and tables"
        ( Right
            ( Document
                "T"
                Nothing
                Nothing
                False
                [ Section
                    (Just "S")
                    4
                    [ List ["one", "two"]
                    , Paragraph "Para"
                    , Code (Just "diff") ["-a", "+b"]
                    , Table ["h1", "h,2"] [["x", "say \"hi\""]]
                    , Ask (Question FreeText 19 "Q?" [Code Nothing ["  code"]] [])
                    ]
                ]
            )
        )
        ( parseDocument
            ( withHeader
                [ "# S"
                , "- one"
                , "- two"
                , "Para"
                , ""
                , "```diff"
                , "-a"
                , "+b"
                , "```"
                , "```csv"
                , "h1, \"h,2\""
                , ""
                , "x,\"say \"\"hi\"\"\""
                , "```"
                , "::: text"
                , "Q?"
                , "```"
                , "  code"
                , "```"
                , ":::"
                ]
            )
        )
    forM_ rejections $ \(source, expected) -> case parseDocument source of
        Right _ -> die ("accepted invalid source: " ++ show source)
        Left errs -> do
            equal ("error lines for " ++ show source) (map fst expected) (map errorLine errs)
            forM_ (zip expected errs) $ \((_, fragment), err) ->
                assert (fragment `T.isInfixOf` errorMessage err) ("expected " ++ show fragment ++ " in " ++ show (errorMessage err))

-- | Body lines start at line 4, after a three-line header.
withHeader :: [Text] -> Text
withHeader body = T.unlines (["---", "title: T", "---"] ++ body)

rejections :: [(Text, [(Int, Text)])]
rejections =
    [ (withHeader ["# S", "> quote"], [(5, "quotes are not supported")])
    , (withHeader ["# S", "See [docs](https://example.test)"], [(5, "links are not supported")])
    , (withHeader ["# S", "**bold**"], [(5, "bold and italic")])
    , (withHeader ["# S", "Run `make`"], [(5, "inline code")])
    , (withHeader ["# S", "![](a.png)"], [(5, "needs a caption")])
    , (withHeader ["# S", "Look ![c](a.png)"], [(5, "its own line")])
    , (withHeader ["# S", "::: tabs", "## A", "![c](a.png)"], [(5, "never closed")])
    , (withHeader ["# S", "::: grid", ":::"], [(5, "unknown container 'grid'")])
    , (withHeader ["# S", "::: tabs", "![c](a.png)", ":::"], [(5, "needs at least one"), (6, "start each tab")])
    , (withHeader ["# S", "## Heading"], [(5, "only used for tab labels")])
    , (withHeader ["# S", "```csv"], [(5, "never closed")])
    , (withHeader ["# S", "::: tabs", "## A", ":::"], [(6, "has no content")])
    , (withHeader ["# S", "::: tabs", "## A", "x", "::: tabs"], [(5, "never closed"), (8, "cannot be nested")])
    , (withHeader ["# S", "- a", "**b**"], [(6, "bold")])
    , (withHeader ["# S", "- **a**"], [(5, "bold")])
    , (withHeader ["# S", "-"], [(5, "a list item needs text")])
    , (withHeader ["# S", "1. one"], [(5, "numbered lists")])
    , (withHeader ["# S", "* a"], [(5, "write list items as '- item'")])
    , (withHeader ["# S", "| a |"], [(5, "write tables as a ```csv block")])
    , (withHeader ["# S", "~~~"], [(5, "not ~~~")])
    , (withHeader ["# S", "```", "", "```"], [(5, "is empty")])
    , (withHeader ["# S", "```c sharp", "x", "```"], [(5, "one word")])
    , (withHeader ["# S", "```csv", "a,b", "1", "```"], [(7, "this row has 1 cells; the header has 2")])
    , (withHeader ["# S", "```csv", "a,b", "```"], [(6, "at least one data row")])
    , (withHeader ["# S", "```csv", "a,b", "\"x,1", "```"], [(7, "never closed")])
    , (withHeader ["# S", "```csv", "a,b", "\"x\"y,1", "```"], [(7, "comma straight after")])
    , (withHeader ["::: text", "```", "x", "```", "Q?", ":::"], [(5, "start the question with its text")])
    , (withHeader [], [(3, "add some content")])
    , (withHeader ["::: choice", "- a", "- b", ":::"], [(4, "needs its question"), (5, "before its options"), (6, "before its options")])
    , (withHeader ["::: choice", "Q?", "- only", ":::"], [(4, "at least two")])
    , (withHeader ["::: text", "Q?", "- a", ":::"], [(6, "take no options")])
    , (withHeader ["::: choice", "Q?", "- a", "- b", "More text", ":::"], [(8, "options come last")])
    , (withHeader ["::: choice", "Q?", "- a", "- a", "- b", ":::"], [(7, "listed twice")])
    , (withHeader ["::: choice", "Q?", "- a (recommended)", "- b (Recommended)", ":::"], [(4, "recommend only one")])
    , (withHeader ["::: choice", "Q?", "::: tabs", ":::"], [(4, "at least two"), (6, "cannot contain containers")])
    , (withHeader ["::: choice", "Q?", "## T", "- a", "- b", ":::"], [(6, "'##' tab labels cannot go inside '::: choice'")])
    , (withHeader ["::: choice", "Q?", "- a", "- b"], [(4, "'::: choice' is never closed")])
    , (withHeader ["::: choice", "Q?", "- a", "- b", "# S"], [(8, "close '::: choice' with ':::'")])
    , (withHeader ["::: choice", "Q?", "* a", "- b", ":::"], [(4, "at least two"), (6, "write each option as '- label'")])
    , (withHeader ["::: choice", "Q?", "- See [x](y)", "- b", ":::"], [(4, "at least two"), (6, "links are not supported")])
    , (withHeader ["::: choice", "![c](a.png)", "Q?", "- a", "- b", ":::"], [(5, "start the question with its text")])
    , (withHeader ["::: multi now", "Q?", "- a", "- b", ":::"], [(4, "'multi' takes no options")])
    , (withHeader ["::: choice", "Q?", "-", "- b", ":::"], [(4, "at least two"), (6, "an option needs text")])
    , (T.unlines ["---", "title: T", "reply: yes", "---", "x"], [(3, "'reply' is 'on' or 'off'")])
    , (T.unlines ["# S", "text"], [(1, "start the file with a '---' header")])
    , (T.unlines ["---", "meta: m", "---", "# S"], [(1, "needs 'title")])
    , (T.unlines ["---", "title: T", "colour: red", "---", "# S"], [(3, "unknown header key 'colour'")])
    ]

-- Golden rendering -------------------------------------------------------

goldenTests :: IO ()
goldenTests = do
    putStrLn "agent-page: golden rendering"
    update <- (== Just "1") <$> lookupEnv "AGENT_PAGE_UPDATE_GOLDEN"
    forM_ goldens $ \(name, doc) -> do
        let path = "test" </> "golden" </> name
            actual = BL.toStrict (toLazyByteString (renderPage "agent-page test · format 3" "/work/notes/page.md" "0123abcd" doc))
        if update
            then BS.writeFile path actual
            else do
                expected <- BS.readFile path
                unless (expected == actual) $
                    die (path ++ " differs from the rendered page; rerun with AGENT_PAGE_UPDATE_GOLDEN=1 after an intended change")

goldens :: [(FilePath, Document Placed)]
goldens =
    [
        ( "basic.html"
        , Document
            "Escaping <&> \"quotes\" and 'apostrophes'"
            (Just "Synthetic fixture")
            (Just "2026-11-26")
            False
            [ Section
                (Just "Media")
                5
                [ Paragraph "A paragraph with <tags> & ampersands."
                , Media (Placed "A still image" 7 (fake EmbeddedImage "image/webp" 640 360))
                , Media (Placed "A clip" 8 (fake (EmbeddedVideo False) "video/mp4" 1280 800))
                , Media (Placed "A looping animation" 9 (fake (EmbeddedVideo True) "video/mp4" 320 240))
                ]
            ]
        )
    ,
        ( "tabs.html"
        , Document
            "Tabs"
            Nothing
            Nothing
            False
            [ Section (Just "Two tabs") 4 [Tabs [Tab "One" [Paragraph "First."], Tab "Two" [Media (Placed "Second" 8 (fake EmbeddedImage "image/webp" 10 10))]]]
            , Section (Just "Three tabs") 10 [Tabs [Tab "A" [Paragraph "a"], Tab "B" [Paragraph "b"], Tab "C" [Paragraph "c"]]]
            ]
        )
    ,
        ( "blocks.html"
        , Document
            "Blocks"
            Nothing
            Nothing
            False
            [ Section
                (Just "Text blocks")
                4
                [ List ["First <item>", "Second & last"]
                , Code (Just "diff") ["--- a/x", "+++ b/x", "@@ -1 +1 @@", "-old", "+new <tag>", " same"]
                , Code (Just "haskell") ["main :: IO ()", "main = pure ()"]
                , Code Nothing ["  indented", "", "after a blank"]
                , Table ["Name", "Count"] [["A <b>", "1"], ["B", "2, two"]]
                ]
            ]
        )
    ,
        ( "questions.html"
        , Document
            "Review <round> 1"
            Nothing
            Nothing
            True
            [ Section
                Nothing
                4
                [ Paragraph "Lead text."
                , Ask
                    ( Question
                        SingleChoice
                        7
                        "Which \"layout\"?"
                        [Paragraph "Context.", Media (Placed "Both layouts" 9 (fake EmbeddedImage "image/webp" 10 10))]
                        [Choice "Compact" True, Choice "Spacious & airy" False]
                    )
                ]
            , Section
                (Just "Payroll")
                14
                [ Tabs
                    [ Tab "Export" [Media (Placed "Export review" 17 (fake EmbeddedImage "image/webp" 10 10))]
                    , Tab "Xero" [Ask (Question MultipleChoice 19 "Pick any" [] [Choice "A" False, Choice "B" False])]
                    ]
                , Ask (Question FreeText 25 "Anything else?" [] [])
                , Media (Placed "A clip" 28 (fake (EmbeddedVideo False) "video/mp4" 10 10))
                ]
            ]
        )
    ]
  where
    fake embedded mime = Encoded embedded mime "BYTES"

-- Pipeline (real ffmpeg) -------------------------------------------------

pipelineTests :: IO ()
pipelineTests = do
    putStrLn "agent-page: pipeline"
    ffmpeg <- requireEnv "AGENT_PAGE_FFMPEG"
    ffprobe <- requireEnv "AGENT_PAGE_FFPROBE"
    let tools = Tools ffmpeg ffprobe
    withSystemTempDirectory "agent-page-test" $ \tmp -> do
        let media = tmp </> "media"
            out = tmp </> "pages"
            config = Config tools out defaultMaxBytes Nothing "agent-page test · format 3"
            generate name arguments = run ffmpeg (["-v", "error", "-y"] ++ arguments ++ [media </> name])
        run "mkdir" ["-p", media]
        generate "clip.mp4" ["-f", "lavfi", "-i", "testsrc2=size=1600x900:rate=30:duration=2", "-f", "lavfi", "-i", "sine=frequency=440:duration=2", "-c:v", "libx264", "-c:a", "aac", "-shortest"]
        generate "silent.mov" ["-f", "lavfi", "-i", "testsrc=size=320x240:rate=25:duration=1", "-c:v", "libx264"]
        generate "large.png" ["-f", "lavfi", "-i", "testsrc2=size=2400x1200", "-frames:v", "1"]
        generate "small.png" ["-f", "lavfi", "-i", "testsrc2=size=300x200", "-frames:v", "1"]
        generate "anim.gif" ["-f", "lavfi", "-i", "testsrc=size=160x120:rate=10:duration=0.8"]
        generate "still.gif" ["-f", "lavfi", "-i", "testsrc=size=160x120", "-frames:v", "1"]
        let source = media </> "doc.md"
        writeFile source $
            unlines
                [ "---"
                , "title: Synthetic pipeline"
                , "---"
                , "# Video"
                , "![Clip with sound](clip.mp4)"
                , "![Silent clip](silent.mov)"
                , "# Images"
                , "::: tabs"
                , "## Large"
                , "![Large still](large.png)"
                , "## Small"
                , "![Small still](small.png)"
                , "## Gifs"
                , "![Animated gif](anim.gif)"
                , "![Single-frame gif](still.gif)"
                , ":::"
                ]
        first <- buildPage config source >>= either (die . unlines) pure
        equal "page directory" out (takeDirectory first)
        assert ("synthetic-pipeline-" `isPrefixOf` takeFileName first && takeExtension first == ".html") ("unexpected page name " ++ first)
        page <- readFile first
        equal "embedded videos" 3 (count "<source type=\"video/mp4\" src=\"data:video/mp4;base64," page)
        equal "embedded images" 3 (count "src=\"data:image/webp;base64," page)
        equal "looping videos" 1 (count " loop autoplay" page)

        cache <- sort . filter (not . ("." `isPrefixOf`)) <$> listDirectory (out </> ".cache")
        equal "one cache entry per source" 6 (length cache)
        assert (not (any ("partial" `isInfixOf`) cache)) "partial encode left in the cache"
        let cached suffix = [out </> ".cache" </> name | name <- cache, suffix `isInfixOf` name]
        forM_ (cached "-video-") $ \path -> do
            facts <- probeFacts ffprobe path
            assert ("codec_name=h264" `elem` facts) ("video not H.264: " ++ show facts)
            assert ("r_frame_rate=15/1" `elem` facts) ("video not 15 fps: " ++ show facts)
        widths <- mapM (fmap (filter ("width=" `isPrefixOf`)) . probeFacts ffprobe) (cached "-video-")
        equal "video widths" [["width=1280"], ["width=320"]] (sort widths)
        audio <- mapM (fmap (elem "codec_type=audio") . probeFacts ffprobe) (cached "-video-")
        equal "audio kept only where present" [False, True] (sort audio)
        images <- mapM (fmap (filter (\f -> "width=" `isPrefixOf` f || "height=" `isPrefixOf` f)) . probeFacts ffprobe) (cached "-image-")
        equal "image sizes" [["width=160", "height=120"], ["width=1920", "height=960"], ["width=300", "height=200"]] (sort images)
        equal "looping gif entries" 1 (length (cached "-loop-"))

        -- Another user shares the cache and reads the pages, so both must get
        -- the ordinary creation mode, not a private temporary-file mode. A
        -- permissive umask makes the difference observable.
        _ <- setFileCreationMask 0o022
        removeDirectoryRecursive out
        rebuilt <- buildPage config source >>= either (die . unlines) pure
        modes <- mapM (fmap fileMode . getFileStatus) (rebuilt : cached "")
        equal "page and cache file modes" (map (const 0o100644) modes) modes

        stamps <- mapM getModificationTime (cached "")
        second <- buildPage config source >>= either (die . unlines) pure
        equal "same page for same input" first second
        stampsAgain <- mapM getModificationTime (cached "")
        equal "cache reused" stamps stampsAgain

        let failing name body expected = do
                let path = media </> name
                    strict = config{configOutDir = tmp </> name}
                writeFile path (unlines (["---", "title: Failure", "---", "# S"] ++ body))
                result <- buildPage strict{configMaxBytes = if name == "big.md" then 1000 else defaultMaxBytes} path
                case result of
                    Right page' -> die ("expected failure, built " ++ page')
                    Left errs -> assert (any (expected `isInfixOf`) errs) ("expected " ++ show expected ++ " in " ++ show errs)
                written <- filter ((== ".html") . takeExtension) <$> getDirectoryContents' (tmp </> name)
                equal ("no page written for " ++ name) [] written
        failing "missing.md" ["![Missing](absent.png)"] "missing.md:5: no such file"
        failing "type.md" ["![Bitmap](picture.bmp)"] "type.md:5: unsupported media type '.bmp'"
        failing "big.md" ["![Large still](large.png)"] "over the 0.0 MiB limit"

count :: String -> String -> Int
count needle haystack = length (filter (needle `isPrefixOf`) (tailsOf haystack))
  where
    tailsOf text = case text of
        [] -> []
        _ : rest -> text : tailsOf rest

requireEnv :: String -> IO String
requireEnv name = lookupEnv name >>= maybe (die ("agent-page tests need " ++ name)) pure

run :: FilePath -> [String] -> IO ()
run program arguments = do
    (code, out, err) <- readProcessWithExitCode program arguments ""
    unless (code == ExitSuccess) (die (unwords (program : arguments) ++ " failed: " ++ out ++ err))

probeFacts :: FilePath -> FilePath -> IO [String]
probeFacts ffprobe path = do
    (code, out, err) <- readProcessWithExitCode ffprobe ["-v", "error", "-show_entries", "stream=codec_type,codec_name,width,height,r_frame_rate", "-of", "default=nw=1", path] ""
    unless (code == ExitSuccess) (die ("ffprobe failed on " ++ path ++ ": " ++ err))
    pure (lines out)

getDirectoryContents' :: FilePath -> IO [FilePath]
getDirectoryContents' dir = do
    exists <- doesDirectoryExist dir
    if exists then filter (`notElem` [".", ".."]) <$> getDirectoryContents dir else pure []
