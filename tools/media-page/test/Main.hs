{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM_, unless)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy qualified as BL
import Data.List (isInfixOf, isPrefixOf, sort)
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (doesDirectoryExist, getDirectoryContents, getModificationTime, listDirectory)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..), die)
import System.FilePath (takeDirectory, takeExtension, takeFileName, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Workshop.MediaPage.Build (Config (..), buildPage, defaultMaxBytes)
import Workshop.MediaPage.Document
import Workshop.MediaPage.Media (Embedded (..), Encoded (..), Tools (..))
import Workshop.MediaPage.Parse (ParseError (..), parseDocument)
import Workshop.MediaPage.Render (Placed (..), renderPage)
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

parserTests :: IO ()
parserTests = do
    putStrLn "media-page: parser"
    equal
        "sample document"
        ( Right
            ( Document
                "Demo page"
                (Just "Synthetic fixture")
                (Just "2026-11-26")
                [ Section "First section" [Paragraph "Plain text that continues here.", Media (MediaRef "A synthetic clip" "clip.webm" 10)]
                , Section
                    "Second section"
                    [Tabs [Tab "One" [Media (MediaRef "First still" "one.png" 15)], Tab "Two" [Paragraph "Second panel text."]]]
                ]
            )
        )
        (parseDocument sample)
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
    [ (withHeader ["# S", "- item"], [(5, "lists, quotes and tables")])
    , (withHeader ["# S", "See [docs](https://example.test)"], [(5, "links are not supported")])
    , (withHeader ["# S", "**bold**"], [(5, "bold and italic")])
    , (withHeader ["# S", "Run `make`"], [(5, "inline code")])
    , (withHeader ["# S", "![](a.png)"], [(5, "needs a caption")])
    , (withHeader ["# S", "Look ![c](a.png)"], [(5, "its own line")])
    , (withHeader ["# S", "::: tabs", "## A", "![c](a.png)"], [(5, "never closed")])
    , (withHeader ["# S", "::: grid", ":::"], [(5, "unknown container 'grid'")])
    , (withHeader ["# S", "::: tabs", "![c](a.png)", ":::"], [(5, "needs at least one"), (6, "start each tab")])
    , (withHeader ["# S", "## Heading"], [(5, "only used for tab labels")])
    , (withHeader ["Text first"], [(4, "must follow a '# Section'")])
    , (withHeader ["# S", "```csv"], [(5, "fenced blocks")])
    , (withHeader ["# S", "::: tabs", "## A", ":::"], [(6, "has no content")])
    , (withHeader ["# S", "::: tabs", "## A", "x", "::: tabs"], [(5, "never closed"), (8, "cannot be nested")])
    , (withHeader ["# S", "- a", "**b**"], [(5, "lists"), (6, "bold")])
    , (withHeader [], [(3, "at least one '# Section'")])
    , (T.unlines ["# S", "text"], [(1, "start the file with a '---' header")])
    , (T.unlines ["---", "meta: m", "---", "# S"], [(1, "needs 'title")])
    , (T.unlines ["---", "title: T", "colour: red", "---", "# S"], [(3, "unknown header key 'colour'")])
    ]

-- Golden rendering -------------------------------------------------------

goldenTests :: IO ()
goldenTests = do
    putStrLn "media-page: golden rendering"
    update <- (== Just "1") <$> lookupEnv "MEDIA_PAGE_UPDATE_GOLDEN"
    forM_ goldens $ \(name, doc) -> do
        let path = "test" </> "golden" </> name
            actual = BL.toStrict (toLazyByteString (renderPage "media-page test · format 1" doc))
        if update
            then BS.writeFile path actual
            else do
                expected <- BS.readFile path
                unless (expected == actual) $
                    die (path ++ " differs from the rendered page; rerun with MEDIA_PAGE_UPDATE_GOLDEN=1 after an intended change")

goldens :: [(FilePath, Document Placed)]
goldens =
    [
        ( "basic.html"
        , Document
            "Escaping <&> \"quotes\" and 'apostrophes'"
            (Just "Synthetic fixture")
            (Just "2026-11-26")
            [ Section
                "Media"
                [ Paragraph "A paragraph with <tags> & ampersands."
                , Media (Placed "A still image" (fake EmbeddedImage "image/webp" 640 360))
                , Media (Placed "A clip" (fake (EmbeddedVideo False) "video/mp4" 1280 800))
                , Media (Placed "A looping animation" (fake (EmbeddedVideo True) "video/mp4" 320 240))
                ]
            ]
        )
    ,
        ( "tabs.html"
        , Document
            "Tabs"
            Nothing
            Nothing
            [ Section "Two tabs" [Tabs [Tab "One" [Paragraph "First."], Tab "Two" [Media (Placed "Second" (fake EmbeddedImage "image/webp" 10 10))]]]
            , Section "Three tabs" [Tabs [Tab "A" [Paragraph "a"], Tab "B" [Paragraph "b"], Tab "C" [Paragraph "c"]]]
            ]
        )
    ]
  where
    fake embedded mime = Encoded embedded mime "BYTES"

-- Pipeline (real ffmpeg) -------------------------------------------------

pipelineTests :: IO ()
pipelineTests = do
    putStrLn "media-page: pipeline"
    ffmpeg <- requireEnv "MEDIA_PAGE_FFMPEG"
    ffprobe <- requireEnv "MEDIA_PAGE_FFPROBE"
    let tools = Tools ffmpeg ffprobe
    withSystemTempDirectory "media-page-test" $ \tmp -> do
        let media = tmp </> "media"
            out = tmp </> "pages"
            config = Config tools out defaultMaxBytes Nothing "media-page test · format 1"
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
requireEnv name = lookupEnv name >>= maybe (die ("media-page tests need " ++ name)) pure

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
