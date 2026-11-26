{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Either (isLeft)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian)
import System.Directory (doesFileExist, getModificationTime, getPermissions, listDirectory, setOwnerExecutable, setPermissions)
import System.Environment (getEnv, setEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess, readProcess)
import Workshop.Abrec.Library
import Workshop.Abrec.Media
import Workshop.Test (assert, equal)

main :: IO ()
main = do
    episodeTitleTests
    slugTests
    libraryTests
    trimTests
    mediaTests

-- Derived values -------------------------------------------------------

sampleChapter :: Chapter
sampleChapter =
    Chapter
        { chapterId = "00"
        , chapterNumber = 13
        , chapterTitle = Just "Fire and Water"
        , chapterEpisodeTitle = Nothing
        , chapterHidden = False
        , chapterRecorded = UTCTime (fromGregorian 2026 11 26) 0
        , chapterSource = "13.wav"
        , chapterMaster = Master "x.wav" "x" 1 1
        }

episodeTitleTests :: IO ()
episodeTitleTests = do
    putStrLn "abrec: episode titles"
    equal "with a chapter title" "13. Fire and Water" (episodeTitle sampleChapter)
    equal "without a chapter title" "Chapter 13" (episodeTitle sampleChapter{chapterTitle = Nothing})
    equal "free text" "Prologue" (episodeTitle sampleChapter{chapterEpisodeTitle = Just "Prologue"})
    equal "follows renumbering" "14. Fire and Water" (episodeTitle sampleChapter{chapterNumber = 14})

slugTests :: IO ()
slugTests = do
    putStrLn "abrec: slugs"
    equal "plain title" "the-hobbit" (slugify "The Hobbit")
    equal "punctuation collapses" "a-b-c" (slugify "  A, b -- C! ")
    equal "nothing usable" "book" (slugify "¿¡")
    equal "length limit" 60 (T.length (slugify (T.replicate 100 "a")))

-- Library --------------------------------------------------------------

libraryTests :: IO ()
libraryTests = withSystemTempDirectory "abrec-library" $ \root -> do
    putStrLn "abrec: library"
    store <- openStore root
    secret <- readSecret store
    equal "secret length" 32 (T.length secret)
    rotateSecret store
    rotated <- readSecret store
    assert (rotated /= secret) "rotating changes the secret"

    expectLeft "blank titles are rejected" =<< createBook store (NewBook "  " Nothing Nothing)
    hobbit <- expectRight =<< createBook store (NewBook "The Hobbit" (Just "J. R. R. Tolkien") (Just " "))
    second <- expectRight =<< createBook store (NewBook "The Hobbit" Nothing Nothing)
    piranesi <- expectRight =<< createBook store (NewBook "Piranesi" Nothing Nothing)
    equal "slug" "the-hobbit" hobbit
    equal "duplicate titles get distinct slugs" "the-hobbit-2" second

    library <- loadLibrary store
    equal "books in season order" ["the-hobbit", "the-hobbit-2", "piranesi"] (map entrySlug (libraryEntries library))
    equal "seasons" [1, 2, 3] (map (bookSeason . entryBook) (libraryEntries library))
    equal "blank optional fields are absent" Nothing (bookDescription . entryBook =<< findEntry library hobbit)

    expectRight =<< updateBook store hobbit (NewBook "The Hobbit, or There and Back Again" Nothing Nothing)
    expectRight =<< deleteBook store second
    fourth <- expectRight =<< createBook store (NewBook "Fourth" Nothing Nothing)
    renamed <- loadLibrary store
    equal "renaming keeps the slug" (Just "The Hobbit, or There and Back Again") (bookTitle . entryBook <$> findEntry renamed hobbit)
    equal "seasons are never reused" (Just 4) (bookSeason . entryBook <$> findEntry renamed fourth)
    expectLeft "path-like slugs are rejected" =<< updateBook store "../escape" (NewBook "x" Nothing Nothing)

    first <- stage store root "first.WAV" "one"
    one <- expectRight =<< addChapter store hobbit (ChapterFields 1 (Just "An Unexpected Party") Nothing) False first
    equal "staged extension is lowercased" "wav" (stagedExtension first)
    three <- stage store root "03 Short Rest.mp3" "three"
    threeId <- expectRight =<< addChapter store hobbit (ChapterFields 3 Nothing Nothing) False three
    duplicate <- stage store root "again.wav" "again"
    expectLeft "taken numbers need replace" =<< addChapter store hobbit (ChapterFields 1 Nothing Nothing) False duplicate
    expectLeft "chapter 0 is rejected" =<< addChapter store hobbit (ChapterFields 0 Nothing Nothing) False duplicate

    withChapters <- entryFor store hobbit
    equal "next chapter" 4 (nextChapterNumber withChapters)
    equal "missing numbers" [2] (missingNumbers withChapters)
    let oldMaster = maybe "" (masterFile . chapterMaster) (findChapter withChapters one)
    assert (oldMaster == stagedSha256 first <> ".wav") "masters are named by hash"

    replaced <- expectRight =<< addChapter store hobbit (ChapterFields 1 Nothing Nothing) True duplicate
    equal "replacing keeps the identifier" one replaced
    afterReplace <- entryFor store hobbit
    equal "replacing keeps the title" (Just "An Unexpected Party") (chapterTitle =<< findChapter afterReplace one)
    oldGone <- doesFileExist (root </> "books" </> "the-hobbit" </> "masters" </> T.unpack oldMaster)
    assert (not oldGone) "the replaced master is removed"

    expectLeft "renumbering onto a taken number fails" =<< updateChapter store hobbit threeId (ChapterFields 1 Nothing Nothing)
    expectRight =<< updateChapter store hobbit threeId (ChapterFields 2 (Just "Roast Mutton") (Just "Free text"))
    expectRight =<< setChapterHidden store hobbit threeId True
    expectRight =<< setBookHidden store piranesi True
    edited <- entryFor store hobbit
    equal "renumbered" (Just 2) (chapterNumber <$> findChapter edited threeId)
    equal "episode title override" (Just "Free text") (episodeTitle <$> findChapter edited threeId)
    equal "hidden chapter" (Just True) (chapterHidden <$> findChapter edited threeId)
    hiddenBook <- entryFor store piranesi
    assert (bookHidden (entryBook hiddenBook)) "hidden book"

    expectRight =<< deleteChapter store hobbit threeId
    afterDelete <- entryFor store hobbit
    equal "deleted chapter" [1] (map chapterNumber (entryChapters afterDelete))
    masters <- listDirectory (root </> "books" </> "the-hobbit" </> "masters")
    equal "only the remaining master stays" 1 (length masters)

    upload <- mapM (uncurry (stage store root)) [("a.wav", "a"), ("b.wav", "b")]
    ident <- createUpload store upload
    loaded <- loadUpload store ident
    equal "upload round trip" (Just upload) loaded
    unknown <- loadUpload store "../x"
    equal "path-like upload identifiers are rejected" Nothing unknown
    discardUpload store ident
    remaining <- listDirectory (root </> "incoming")
    equal "discarding removes staged files" [] remaining
  where
    entryFor store slug = fromMaybe (error ("missing " ++ T.unpack slug)) . (`findEntry` slug) <$> loadLibrary store

stage :: Store -> FilePath -> Text -> Text -> IO Staged
stage store root name body = do
    let path = root </> "tmp" </> "upload"
    writeFile path (T.unpack body)
    stageFile store path name 1.5

expectRight :: Either Text a -> IO a
expectRight = either (\err -> fail ("unexpected failure: " ++ T.unpack err)) pure

expectLeft :: String -> Either Text a -> IO ()
expectLeft message result = assert (isLeft result) message

-- Media ----------------------------------------------------------------

trimTests :: IO ()
trimTests = do
    putStrLn "abrec: trimming"
    equal "no silence" (0, 60) (trimWindow 60 [])
    equal "leading and trailing" (1.5, 55.5) (trimWindow 60 [SilenceStart 0, SilenceEnd 2, SilenceStart 30, SilenceEnd 32, SilenceStart 55, SilenceEnd 60])
    equal "trailing without an end" (0, 50.5) (trimWindow 60 [SilenceStart 50])
    equal "middle silence stays" (0, 60) (trimWindow 60 [SilenceStart 10, SilenceEnd 20])
    equal "all silence is kept" (0, 60) (trimWindow 60 [SilenceStart 0, SilenceEnd 60])

mediaTests :: IO ()
mediaTests = withSystemTempDirectory "abrec-media" $ \dir -> do
    putStrLn "abrec: media"
    curl <- fakeCurl dir
    setEnv "ABREC_CURL" curl
    tools <- loadTools
    let ffmpeg arguments = callProcess (toolFfmpeg tools) (["-hide_banner", "-loglevel", "error", "-y"] ++ arguments)
        fixture = dir </> "fixture.wav"
        silence = ["-f", "lavfi", "-i", "anullsrc=r=48000:cl=mono:d=2"]
    ffmpeg (silence ++ ["-f", "lavfi", "-i", "sine=frequency=440:sample_rate=48000:duration=3"] ++ silence ++ ["-filter_complex", "[0][1][2]concat=n=3:v=0:a=1", fixture])

    duration <- expectRight =<< probeAudio tools fixture
    near "fixture duration" 7 duration
    writeFile (dir </> "text.wav") "not audio"
    expectLeft "text is not audio" =<< probeAudio tools (dir </> "text.wav")

    (encoded, facts) <- expectRight =<< cachedEncode tools (dir </> "cache") "abc" fixture duration
    near "trimmed to the tone plus half a second each side" 4 (encodedSeconds facts)
    stream <- probe tools ["stream=codec_name,channels,sample_rate"] encoded
    equal "feed format" ["codec_name=mp3", "sample_rate=44100", "channels=1"] stream
    before <- getModificationTime encoded
    (again, _) <- expectRight =<< cachedEncode tools (dir </> "cache") "abc" fixture duration
    after <- getModificationTime again
    equal "a cached encode is reused" before after

    ffmpeg ["-f", "lavfi", "-i", "color=c=red:s=800x600", "-frames:v", "1", dir </> "cover.png"]
    expectRight =<< normaliseCover tools (dir </> "cover.png") (dir </> "cover.jpg")
    coverSize <- probe tools ["stream=width,height"] (dir </> "cover.jpg")
    equal "cover is square" ["width=1400", "height=1400"] coverSize
    expectLeft "audio is not a cover" =<< normaliseCover tools fixture (dir </> "bad.jpg")
    badExists <- doesFileExist (dir </> "bad.jpg")
    assert (not badExists) "a rejected cover leaves no file"

    expectRight =<< titleCard tools "The Hobbit, or There and Back Again" (Just "J. R. R. %{pts} Tolkien") (dir </> "card.jpg")
    cardSize <- probe tools ["stream=width,height"] (dir </> "card.jpg")
    equal "title card is square" ["width=1400", "height=1400"] cardSize
    leftovers <- listDirectory dir
    assert (all (\name -> not (".txt" `T.isSuffixOf` T.pack name)) leftovers) "title card text files are removed"
    equal "short titles are large" 110 (fst (wrapTitle "The Hobbit"))
    equal "long titles are smaller" 80 (fst (wrapTitle (T.unwords (replicate 30 "word"))))

    expectLeft "plain http is refused" =<< fetchHttps tools "http://example.com/cover.png" (dir </> "fetched")
    setEnv "ABREC_FAKE_CURL_SOURCE" (dir </> "cover.png")
    expectRight =<< fetchHttps tools "https://example.com/cover.png" (dir </> "fetched")
    fetched <- doesFileExist (dir </> "fetched")
    assert fetched "https fetch writes the target"

-- | A curl stand-in that copies ABREC_FAKE_CURL_SOURCE to its --output.
fakeCurl :: FilePath -> IO FilePath
fakeCurl dir = do
    shell <- getEnv "ABREC_TEST_SHELL"
    let path = dir </> "curl"
    writeFile path . unlines $
        [ "#!" ++ shell
        , "while [ $# -gt 0 ]; do case \"$1\" in --output) out=\"$2\"; shift;; esac; shift; done"
        , "cp \"$ABREC_FAKE_CURL_SOURCE\" \"$out\""
        ]
    permissions <- getPermissions path
    setPermissions path (setOwnerExecutable True permissions)
    pure path

probe :: Tools -> [String] -> FilePath -> IO [String]
probe tools entries path = lines <$> readProcess (toolFfprobe tools) (["-v", "error", "-of", "default=noprint_wrappers=1"] ++ concatMap (\entry -> ["-show_entries", entry]) entries ++ [path]) ""

near :: String -> Double -> Double -> IO ()
near label expected actual = assert (abs (expected - actual) < 0.3) (label ++ ": expected about " ++ show expected ++ ", got " ++ show actual)
