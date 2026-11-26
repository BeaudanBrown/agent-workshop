{-# LANGUAGE OverloadedStrings #-}

{- | The library on disk: books, chapters, masters and the listener secret.

Files are the source of truth. Every operation that reads or writes the
library holds the store's lock, and every JSON file is replaced by renaming a
complete temporary file, so a reader never sees a partial write.
-}
module Workshop.Abrec.Library (
    -- * Store
    Store,
    storeRoot,
    openStore,
    withStoreLock,

    -- * Model
    LibraryMeta (..),
    Book (..),
    Chapter (..),
    Master (..),
    Entry (..),
    Library (..),
    Staged (..),
    NewBook (..),
    ChapterFields (..),

    -- * Reading
    loadLibrary,
    findEntry,
    findChapter,
    readSecret,

    -- * Writing
    setLibraryTitle,
    rotateSecret,
    createBook,
    updateBook,
    setBookHidden,
    deleteBook,
    setBookCover,
    removeBookCover,
    addChapter,
    updateChapter,
    setChapterHidden,
    replaceChapterAudio,
    deleteChapter,

    -- * Uploads
    stageFile,
    createUpload,
    loadUpload,
    discardUpload,

    -- * Derived values
    episodeTitle,
    nextChapterNumber,
    missingNumbers,
    slugify,

    -- * Paths
    coverPath,
    masterPath,
    cacheDir,
    releasesDir,
    sitePath,
    scratchDir,

    -- * Helpers
    randomHex,
    sha256File,
    isIdentifier,
) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (IOException, catch, throwIO)
import Control.Monad (filterM, forM, forM_, unless, when)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson (FromJSON (..), Options (..), ToJSON (..), defaultOptions, eitherDecodeFileStrict', encode, genericParseJSON, genericToJSON)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as BL
import Data.Char (isAsciiLower, isDigit, toLower)
import Data.List (find, sortOn, (\\))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist, getFileSize, listDirectory, removeDirectoryRecursive, removeFile, renameFile)
import System.FilePath (takeExtension, (<.>), (</>))
import System.IO (IOMode (ReadMode), withBinaryFile)
import System.Posix.Files (setFileMode)

-- Store ----------------------------------------------------------------

data Store = Store
    { storeRoot :: FilePath
    , storeLock :: MVar ()
    }

{- | Open the library at a directory, creating its layout and secret if
missing. Only @releases@ is readable by other users.
-}
openStore :: FilePath -> IO Store
openStore root = do
    forM_
        [ (root, 0o711)
        , (root </> "books", 0o700)
        , (root </> "incoming", 0o700)
        , (root </> "cache", 0o700)
        , (root </> "tmp", 0o700)
        , (root </> "releases", 0o755)
        ]
        $ \(dir, mode) -> do
            createDirectoryIfMissing True dir
            setFileMode dir mode
    hasSecret <- doesFileExist (root </> "secret")
    unless hasSecret (newSecret root)
    Store root <$> newMVar ()

-- | Run an action while holding the library lock.
withStoreLock :: Store -> IO a -> IO a
withStoreLock store action = withMVar (storeLock store) (const action)

-- Model ----------------------------------------------------------------

data LibraryMeta = LibraryMeta
    { metaTitle :: Text
    , metaNextSeason :: Int
    }
    deriving (Eq, Show, Generic)

data Book = Book
    { bookTitle :: Text
    , bookAuthor :: Maybe Text
    , bookDescription :: Maybe Text
    , bookSeason :: Int
    , bookHidden :: Bool
    , bookCreated :: UTCTime
    , bookCoverSha256 :: Maybe Text
    }
    deriving (Eq, Show, Generic)

-- | A chapter's audio as received, stored under its SHA-256.
data Master = Master
    { masterFile :: Text
    , masterSha256 :: Text
    , masterBytes :: Integer
    , masterSeconds :: Double
    }
    deriving (Eq, Show, Generic)

data Chapter = Chapter
    { chapterId :: Text
    , chapterNumber :: Int
    , chapterTitle :: Maybe Text
    , chapterEpisodeTitle :: Maybe Text
    -- ^ free text replacing the default episode title
    , chapterHidden :: Bool
    , chapterRecorded :: UTCTime
    , chapterSource :: Text
    -- ^ the uploaded file name, or how the take was made
    , chapterMaster :: Master
    }
    deriving (Eq, Show, Generic)

-- | A book with its slug and chapters in number order.
data Entry = Entry
    { entrySlug :: Text
    , entryBook :: Book
    , entryChapters :: [Chapter]
    }
    deriving (Eq, Show)

-- | Every book, in season order.
data Library = Library
    { libraryMeta :: LibraryMeta
    , libraryEntries :: [Entry]
    }
    deriving (Eq, Show)

-- | Audio moved into the library's incoming area, not yet a chapter.
data Staged = Staged
    { stagedPath :: FilePath
    , stagedExtension :: Text
    , stagedSha256 :: Text
    , stagedBytes :: Integer
    , stagedSeconds :: Double
    , stagedSource :: Text
    }
    deriving (Eq, Show, Generic)

data NewBook = NewBook
    { newTitle :: Text
    , newAuthor :: Maybe Text
    , newDescription :: Maybe Text
    }
    deriving (Eq, Show)

data ChapterFields = ChapterFields
    { fieldNumber :: Int
    , fieldTitle :: Maybe Text
    , fieldEpisodeTitle :: Maybe Text
    }
    deriving (Eq, Show)

jsonOptions :: String -> Options
jsonOptions prefix =
    defaultOptions
        { fieldLabelModifier = lowerFirst . drop (length prefix)
        , omitNothingFields = True
        }
  where
    lowerFirst (c : rest) = toLower c : rest
    lowerFirst [] = []

instance ToJSON LibraryMeta where toJSON = genericToJSON (jsonOptions "meta")
instance FromJSON LibraryMeta where parseJSON = genericParseJSON (jsonOptions "meta")
instance ToJSON Book where toJSON = genericToJSON (jsonOptions "book")
instance FromJSON Book where parseJSON = genericParseJSON (jsonOptions "book")
instance ToJSON Master where toJSON = genericToJSON (jsonOptions "master")
instance FromJSON Master where parseJSON = genericParseJSON (jsonOptions "master")
instance ToJSON Chapter where toJSON = genericToJSON (jsonOptions "chapter")
instance FromJSON Chapter where parseJSON = genericParseJSON (jsonOptions "chapter")
instance ToJSON Staged where toJSON = genericToJSON (jsonOptions "staged")
instance FromJSON Staged where parseJSON = genericParseJSON (jsonOptions "staged")

-- Paths ----------------------------------------------------------------

booksDir, cacheDir, releasesDir, sitePath, scratchDir, incomingDir :: Store -> FilePath
booksDir store = storeRoot store </> "books"
cacheDir store = storeRoot store </> "cache"
releasesDir store = storeRoot store </> "releases"
sitePath store = storeRoot store </> "site"
scratchDir store = storeRoot store </> "tmp"
incomingDir store = storeRoot store </> "incoming"

bookDir :: Store -> Text -> FilePath
bookDir store slug = booksDir store </> T.unpack slug

coverPath :: Store -> Text -> FilePath
coverPath store slug = bookDir store slug </> "cover.jpg"

masterPath :: Store -> Text -> Master -> FilePath
masterPath store slug master = bookDir store slug </> "masters" </> T.unpack (masterFile master)

chapterPath :: Store -> Text -> Text -> FilePath
chapterPath store slug ident = bookDir store slug </> "chapters" </> T.unpack ident <.> "json"

metaPath, secretPath :: Store -> FilePath
metaPath store = storeRoot store </> "library.json"
secretPath store = storeRoot store </> "secret"

-- Reading --------------------------------------------------------------

loadLibrary :: Store -> IO Library
loadLibrary store = withStoreLock store (loadUnlocked store)

loadUnlocked :: Store -> IO Library
loadUnlocked store = do
    meta <- readMeta store
    slugs <- listDirectory (booksDir store) >>= filterM (\slug -> doesFileExist (booksDir store </> slug </> "book.json"))
    entries <- forM slugs $ \slugString -> do
        let slug = T.pack slugString
            chaptersDir = bookDir store slug </> "chapters"
        book <- readJson (bookDir store slug </> "book.json")
        names <- listDirectory chaptersDir
        chapters <- mapM (readJson . (chaptersDir </>)) [name | name <- names, takeExtension name == ".json"]
        pure (Entry slug book (sortOn chapterNumber chapters))
    pure (Library meta (sortOn (bookSeason . entryBook) entries))

readMeta :: Store -> IO LibraryMeta
readMeta store = do
    present <- doesFileExist (metaPath store)
    if present then readJson (metaPath store) else pure (LibraryMeta "Audiobooks" 1)

findEntry :: Library -> Text -> Maybe Entry
findEntry library slug = find ((== slug) . entrySlug) (libraryEntries library)

findChapter :: Entry -> Text -> Maybe Chapter
findChapter entry ident = find ((== ident) . chapterId) (entryChapters entry)

readSecret :: Store -> IO Text
readSecret store = T.strip . decodeUtf8 <$> BS.readFile (secretPath store)

-- Writing --------------------------------------------------------------

setLibraryTitle :: Store -> Text -> IO (Either Text ())
setLibraryTitle store title = withStoreLock store $ case required title of
    Nothing -> pure (Left "The library needs a title.")
    Just value -> do
        meta <- readMeta store
        Right <$> writeJson (metaPath store) meta{metaTitle = value}

-- | Replace the listener secret, invalidating every listener URL.
rotateSecret :: Store -> IO ()
rotateSecret store = withStoreLock store (newSecret (storeRoot store))

newSecret :: FilePath -> IO ()
newSecret root = do
    secret <- randomHex 16
    let path = root </> "secret"
    BS.writeFile (path <.> "tmp") (encodeUtf8 (secret <> "\n"))
    renameFile (path <.> "tmp") path

-- | Create a book and return its slug, which never changes afterwards.
createBook :: Store -> NewBook -> IO (Either Text Text)
createBook store new = withStoreLock store $ case required (newTitle new) of
    Nothing -> pure (Left "A book needs a title.")
    Just title -> do
        existing <- listDirectory (booksDir store)
        let slug = uniqueSlug (map T.pack existing) (slugify title)
        meta <- readMeta store
        now <- getCurrentTime
        let book =
                Book
                    { bookTitle = title
                    , bookAuthor = newAuthor new >>= required
                    , bookDescription = newDescription new >>= required
                    , bookSeason = metaNextSeason meta
                    , bookHidden = False
                    , bookCreated = now
                    , bookCoverSha256 = Nothing
                    }
        forM_ ["chapters", "masters"] $ \sub -> createDirectoryIfMissing True (bookDir store slug </> sub)
        writeJson (bookDir store slug </> "book.json") book
        writeJson (metaPath store) meta{metaNextSeason = metaNextSeason meta + 1}
        pure (Right slug)

-- | Change a book's title, author and description; the slug stays.
updateBook :: Store -> Text -> NewBook -> IO (Either Text ())
updateBook store slug edit = case required (newTitle edit) of
    Nothing -> pure (Left "A book needs a title.")
    Just title -> modifyBook store slug $ \book ->
        Right
            book
                { bookTitle = title
                , bookAuthor = newAuthor edit >>= required
                , bookDescription = newDescription edit >>= required
                }

setBookHidden :: Store -> Text -> Bool -> IO (Either Text ())
setBookHidden store slug hidden = modifyBook store slug (\book -> Right book{bookHidden = hidden})

-- | Remove a book, its chapters and masters permanently.
deleteBook :: Store -> Text -> IO (Either Text ())
deleteBook store slug = withStoreLock store $ do
    present <- bookExists store slug
    if present
        then Right <$> removeDirectoryRecursive (bookDir store slug)
        else pure (Left "No such book.")

-- | Install a normalised JPEG, moving it out of its scratch location.
setBookCover :: Store -> Text -> FilePath -> IO (Either Text ())
setBookCover store slug jpeg = withStoreLock store $ do
    present <- bookExists store slug
    if not present
        then pure (Left "No such book.")
        else do
            digest <- sha256File jpeg
            moveFile jpeg (coverPath store slug)
            modifyBookUnlocked store slug (\book -> Right book{bookCoverSha256 = Just digest})

removeBookCover :: Store -> Text -> IO (Either Text ())
removeBookCover store slug = withStoreLock store $ do
    result <- modifyBookUnlocked store slug (\book -> Right book{bookCoverSha256 = Nothing})
    present <- doesFileExist (coverPath store slug)
    when present (removeFile (coverPath store slug))
    pure result

modifyBook :: Store -> Text -> (Book -> Either Text Book) -> IO (Either Text ())
modifyBook store slug change = withStoreLock store (modifyBookUnlocked store slug change)

modifyBookUnlocked :: Store -> Text -> (Book -> Either Text Book) -> IO (Either Text ())
modifyBookUnlocked store slug change = do
    present <- bookExists store slug
    if not present
        then pure (Left "No such book.")
        else do
            let path = bookDir store slug </> "book.json"
            book <- readJson path
            traverse (writeJson path) (change book)

bookExists :: Store -> Text -> IO Bool
bookExists store slug
    | isSlug slug = doesFileExist (bookDir store slug </> "book.json")
    | otherwise = pure False

{- | Add staged audio as a chapter and return its identifier. When the number
is taken, the existing chapter's audio is replaced only if @replace@ is set;
it keeps its identifier, and its titles unless new ones are given.
-}
addChapter :: Store -> Text -> ChapterFields -> Bool -> Staged -> IO (Either Text Text)
addChapter store slug fields replace staged = withStoreLock store $ do
    loaded <- loadEntryUnlocked store slug
    case loaded of
        Nothing -> pure (Left "No such book.")
        Just entry
            | fieldNumber fields < 1 -> pure (Left "Chapter numbers start at 1.")
            | otherwise -> case find ((== fieldNumber fields) . chapterNumber) (entryChapters entry) of
                Just existing
                    | replace -> do
                        master <- installMaster store slug staged
                        writeChapter
                            store
                            slug
                            existing
                                { chapterTitle = firstJust (fieldTitle fields >>= required) (chapterTitle existing)
                                , chapterEpisodeTitle = firstJust (fieldEpisodeTitle fields >>= required) (chapterEpisodeTitle existing)
                                , chapterSource = stagedSource staged
                                , chapterMaster = master
                                }
                        releaseMaster store slug (chapterMaster existing)
                        pure (Right (chapterId existing))
                    | otherwise -> pure (Left ("Chapter " <> tshow (fieldNumber fields) <> " already exists."))
                Nothing -> do
                    master <- installMaster store slug staged
                    ident <- randomHex 8
                    now <- getCurrentTime
                    writeChapter
                        store
                        slug
                        Chapter
                            { chapterId = ident
                            , chapterNumber = fieldNumber fields
                            , chapterTitle = fieldTitle fields >>= required
                            , chapterEpisodeTitle = fieldEpisodeTitle fields >>= required
                            , chapterHidden = False
                            , chapterRecorded = now
                            , chapterSource = stagedSource staged
                            , chapterMaster = master
                            }
                    pure (Right ident)
  where
    firstJust (Just value) _ = Just value
    firstJust Nothing fallback = fallback

-- | Change a chapter's number, title and episode title.
updateChapter :: Store -> Text -> Text -> ChapterFields -> IO (Either Text ())
updateChapter store slug ident fields = modifyChapter store slug ident edit
  where
    edit entry chapter
        | fieldNumber fields < 1 = Left "Chapter numbers start at 1."
        | any (\other -> chapterNumber other == fieldNumber fields && chapterId other /= ident) (entryChapters entry) =
            Left ("Chapter " <> tshow (fieldNumber fields) <> " already exists.")
        | otherwise =
            Right
                chapter
                    { chapterNumber = fieldNumber fields
                    , chapterTitle = fieldTitle fields >>= required
                    , chapterEpisodeTitle = fieldEpisodeTitle fields >>= required
                    }

setChapterHidden :: Store -> Text -> Text -> Bool -> IO (Either Text ())
setChapterHidden store slug ident hidden = modifyChapter store slug ident (\_ chapter -> Right chapter{chapterHidden = hidden})

replaceChapterAudio :: Store -> Text -> Text -> Staged -> IO (Either Text ())
replaceChapterAudio store slug ident staged = withStoreLock store $ do
    loaded <- loadChapterUnlocked store slug ident
    case loaded of
        Nothing -> pure (Left "No such chapter.")
        Just (_, chapter) -> do
            master <- installMaster store slug staged
            writeChapter store slug chapter{chapterMaster = master, chapterSource = stagedSource staged}
            releaseMaster store slug (chapterMaster chapter)
            pure (Right ())

-- | Remove a chapter and, unless another chapter shares it, its master.
deleteChapter :: Store -> Text -> Text -> IO (Either Text ())
deleteChapter store slug ident = withStoreLock store $ do
    loaded <- loadChapterUnlocked store slug ident
    case loaded of
        Nothing -> pure (Left "No such chapter.")
        Just (_, chapter) -> do
            removeFile (chapterPath store slug ident)
            releaseMaster store slug (chapterMaster chapter)
            pure (Right ())

modifyChapter :: Store -> Text -> Text -> (Entry -> Chapter -> Either Text Chapter) -> IO (Either Text ())
modifyChapter store slug ident change = withStoreLock store $ do
    loaded <- loadChapterUnlocked store slug ident
    case loaded of
        Nothing -> pure (Left "No such chapter.")
        Just (entry, chapter) -> traverse (writeChapter store slug) (change entry chapter)

loadEntryUnlocked :: Store -> Text -> IO (Maybe Entry)
loadEntryUnlocked store slug = do
    present <- bookExists store slug
    if present
        then (`findEntry` slug) <$> loadUnlocked store
        else pure Nothing

loadChapterUnlocked :: Store -> Text -> Text -> IO (Maybe (Entry, Chapter))
loadChapterUnlocked store slug ident = do
    loaded <- loadEntryUnlocked store slug
    pure (loaded >>= \entry -> (,) entry <$> findChapter entry ident)

writeChapter :: Store -> Text -> Chapter -> IO ()
writeChapter store slug chapter = writeJson (chapterPath store slug (chapterId chapter)) chapter

-- | Move staged audio into the book's masters, named by its hash.
installMaster :: Store -> Text -> Staged -> IO Master
installMaster store slug staged = do
    let master = Master (stagedSha256 staged <> "." <> stagedExtension staged) (stagedSha256 staged) (stagedBytes staged) (stagedSeconds staged)
        target = masterPath store slug master
    exists <- doesFileExist target
    if exists then removeFile (stagedPath staged) else moveFile (stagedPath staged) target
    pure master

-- | After a chapter stopped using a master, delete it unless another chapter uses it.
releaseMaster :: Store -> Text -> Master -> IO ()
releaseMaster store slug master = do
    current <- loadEntryUnlocked store slug
    let referenced = maybe False (any ((== masterFile master) . masterFile . chapterMaster) . entryChapters) current
        path = masterPath store slug master
    present <- doesFileExist path
    when (present && not referenced) (removeFile path)

-- Uploads --------------------------------------------------------------

{- | Move an uploaded or recorded file into @incoming@, recording its hash,
size and the duration the caller measured.
-}
stageFile :: Store -> FilePath -> Text -> Double -> IO Staged
stageFile store source name seconds = do
    ident <- randomHex 8
    let extension = fileExtension name
        target = incomingDir store </> T.unpack ident <.> T.unpack extension
    moveFile source target
    digest <- sha256File target
    size <- getFileSize target
    pure (Staged target extension digest size seconds name)

-- | Record staged files as one upload awaiting chapter numbers.
createUpload :: Store -> [Staged] -> IO Text
createUpload store staged = do
    ident <- randomHex 8
    writeJson (uploadPath store ident) staged
    pure ident

loadUpload :: Store -> Text -> IO (Maybe [Staged])
loadUpload store ident
    | not (isIdentifier ident) = pure Nothing
    | otherwise = do
        present <- doesFileExist (uploadPath store ident)
        if present then Just <$> readJson (uploadPath store ident) else pure Nothing

-- | Remove an upload's record and whatever staged files remain.
discardUpload :: Store -> Text -> IO ()
discardUpload store ident = do
    loaded <- loadUpload store ident
    forM_ loaded $ \staged -> do
        forM_ staged $ \file -> do
            present <- doesFileExist (stagedPath file)
            when present (removeFile (stagedPath file))
        removeFile (uploadPath store ident)

uploadPath :: Store -> Text -> FilePath
uploadPath store ident = incomingDir store </> T.unpack ident <.> "upload"

-- Derived values -------------------------------------------------------

{- | The title a podcast app shows: free text if given, otherwise
@13. Fire and Water@, or @Chapter 13@ without a chapter title.
-}
episodeTitle :: Chapter -> Text
episodeTitle chapter = fromMaybe fallback (chapterEpisodeTitle chapter)
  where
    number = tshow (chapterNumber chapter)
    fallback = maybe ("Chapter " <> number) ((number <> ". ") <>) (chapterTitle chapter)

nextChapterNumber :: Entry -> Int
nextChapterNumber entry = 1 + maximum (0 : map chapterNumber (entryChapters entry))

-- | Numbers below the highest chapter that have no chapter.
missingNumbers :: Entry -> [Int]
missingNumbers entry = [1 .. nextChapterNumber entry - 1] \\ map chapterNumber (entryChapters entry)

-- | A URL-safe name from a title: lowercase ASCII letters, digits and hyphens.
slugify :: Text -> Text
slugify title =
    let mapped = T.map (\c -> if isAsciiLower c || isDigit c then c else '-') (T.toLower title)
        joined = T.intercalate "-" (filter (not . T.null) (T.splitOn "-" mapped))
        trimmed = T.dropWhileEnd (== '-') (T.take 60 joined)
     in if T.null trimmed then "book" else trimmed

uniqueSlug :: [Text] -> Text -> Text
uniqueSlug existing base = go (1 :: Int)
  where
    go n =
        let candidate = if n == 1 then base else base <> "-" <> tshow n
         in if candidate `elem` existing then go (n + 1) else candidate

isSlug :: Text -> Bool
isSlug slug = not (T.null slug) && T.all (\c -> isAsciiLower c || isDigit c || c == '-') slug

-- | Identifiers made by 'randomHex'.
isIdentifier :: Text -> Bool
isIdentifier ident = not (T.null ident) && T.all (\c -> isDigit c || (c >= 'a' && c <= 'f')) ident

fileExtension :: Text -> Text
fileExtension name =
    let extension = T.toLower (T.drop 1 (T.pack (takeExtension (T.unpack name))))
     in if not (T.null extension) && T.length extension <= 8 && T.all (\c -> isAsciiLower c || isDigit c) extension
            then extension
            else "audio"

-- Helpers --------------------------------------------------------------

-- | A random lowercase hexadecimal string from @n@ bytes of /dev/urandom.
randomHex :: Int -> IO Text
randomHex n = withBinaryFile "/dev/urandom" ReadMode $ \handle ->
    decodeUtf8 . Base16.encode <$> BS.hGet handle n

sha256File :: FilePath -> IO Text
sha256File path = decodeUtf8 . Base16.encode . SHA256.hashlazy <$> BL.readFile path

required :: Text -> Maybe Text
required value = let stripped = T.strip value in if T.null stripped then Nothing else Just stripped

tshow :: Int -> Text
tshow = T.pack . show

readJson :: (FromJSON a) => FilePath -> IO a
readJson path = eitherDecodeFileStrict' path >>= either (\err -> throwIO (userError (path ++ ": " ++ err))) pure

writeJson :: (ToJSON a) => FilePath -> a -> IO ()
writeJson path value = do
    BL.writeFile (path <.> "tmp") (encode value)
    renameFile (path <.> "tmp") path

-- | Rename, or copy and remove when the source is on another file system.
moveFile :: FilePath -> FilePath -> IO ()
moveFile source target =
    renameFile source target `catch` \(_ :: IOException) -> do
        copyFile source target
        removeFile source
