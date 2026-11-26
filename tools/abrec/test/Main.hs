{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Either (isLeft)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian)
import System.Directory (doesFileExist, listDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Workshop.Abrec.Library
import Workshop.Test (assert, equal)

main :: IO ()
main = do
    episodeTitleTests
    slugTests
    libraryTests

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
