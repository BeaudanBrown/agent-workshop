{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{- | Render the public site from the library's shown books and chapters.

A render writes a complete release next to the current one and then swaps the
@site@ symlink, so listeners only ever see a finished release. A failed render
removes its partial release and leaves the current site serving.
-}
module Workshop.Abrec.Site (
    renderSite,
    bookArtwork,
    listenerBase,
    bookFeedUrl,
    libraryFeedUrl,
) where

import Control.Exception (IOException, catch)
import Control.Monad (forM, forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as BL
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.IO qualified as TIO
import Data.Time (addUTCTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import IHP.HSX.QQ (hsx)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, pathIsSymbolicLink, removeDirectoryRecursive, removeFile)
import System.FilePath ((<.>), (</>))
import System.Posix.Files (createLink, createSymbolicLink, rename)
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Workshop.Abrec.Feed (Channel (..), Item (..), renderFeed)
import Workshop.Abrec.Library
import Workshop.Abrec.Media (Encoded (..), Tools, cachedEncode, titleCard)

-- | A shown chapter with its encoded audio.
data Published = Published
    { publishedChapter :: Chapter
    , publishedAudio :: FilePath
    , publishedFile :: Text
    , publishedEncoded :: Encoded
    }

-- | A shown book with its artwork and shown chapters.
data Shelf = Shelf
    { shelfEntry :: Entry
    , shelfArtwork :: FilePath
    , shelfArtworkFile :: Text
    , shelfChapters :: [Published]
    }

listenerBase :: Text -> Text -> Text
listenerBase publicUrl secret = publicUrl <> "/" <> secret <> "/"

bookFeedUrl :: Text -> Text -> Text -> Text
bookFeedUrl publicUrl secret slug = listenerBase publicUrl secret <> slug <> "/feed.xml"

libraryFeedUrl :: Text -> Text -> Text
libraryFeedUrl publicUrl secret = listenerBase publicUrl secret <> "feed.xml"

renderSite :: Tools -> Store -> Text -> IO (Either Text ())
renderSite tools store publicUrl = do
    library <- loadLibrary store
    secret <- readSecret store
    now <- getPOSIXTime
    suffix <- randomHex 3
    -- Zero-padded microseconds sort releases by age, even within one second.
    let stamp = T.unpack (T.justifyRight 20 '0' (T.pack (show (floor (now * 1000000) :: Integer))))
        release = stamp ++ "-" ++ T.unpack suffix
        partial = releasesDir store </> release <.> "partial"
    result <- runExceptT $ do
        shelves <- forM (shownEntries library) (prepare tools store)
        libraryCard <- card tools store (metaTitle (libraryMeta library)) Nothing
        liftIO (writeRelease partial secret publicUrl (libraryMeta library) libraryCard shelves)
    case result of
        Left err -> do
            exists <- doesDirectoryExist partial
            when exists (removeDirectoryRecursive partial)
            pure (Left err)
        Right () -> do
            rename partial (releasesDir store </> release)
            swapSite store release
            prune store release
            pure (Right ())

shownEntries :: Library -> [Entry]
shownEntries library =
    [ entry{entryChapters = filter (not . chapterHidden) (entryChapters entry)}
    | entry <- libraryEntries library
    , not (bookHidden (entryBook entry))
    ]

prepare :: Tools -> Store -> Entry -> ExceptT Text IO Shelf
prepare tools store entry = do
    let slug = entrySlug entry
        book = entryBook entry
    (artwork, artworkFile) <- ExceptT (bookArtwork tools store entry)
    chapters <- forM (entryChapters entry) $ \chapter -> do
        let master = chapterMaster chapter
        (audio, encoded) <-
            ExceptT $
                prefixed ("Chapter " <> T.pack (show (chapterNumber chapter)) <> " of " <> bookTitle book <> ": ")
                    <$> cachedEncode tools (cacheDir store) (masterSha256 master) (masterPath store slug master) (masterSeconds master)
        let file = T.justifyRight 3 '0' (T.pack (show (chapterNumber chapter))) <> "-" <> T.take 12 (masterSha256 master) <> ".mp3"
        pure (Published chapter audio file encoded)
    pure (Shelf entry artwork artworkFile chapters)
  where
    prefixed prefix = either (Left . (prefix <>)) Right

-- | A book's cover, or its cached title card, with the file name listeners see.
bookArtwork :: Tools -> Store -> Entry -> IO (Either Text (FilePath, Text))
bookArtwork tools store entry = case bookCoverSha256 book of
    Just digest -> pure (Right (coverPath store (entrySlug entry), "cover-" <> T.take 12 digest <> ".jpg"))
    Nothing -> runExceptT (card tools store (bookTitle book) (bookAuthor book))
  where
    book = entryBook entry

-- | A cached title card for a title and author.
card :: Tools -> Store -> Text -> Maybe Text -> ExceptT Text IO (FilePath, Text)
card tools store title author = do
    let digest = decodeUtf8 (Base16.encode (SHA256.hash (encodeUtf8 (T.intercalate "\n" ["card-v1", title, fromMaybe "" author]))))
        name = "card-" <> T.take 12 digest <> ".jpg"
        path = cacheDir store </> T.unpack name
    liftIO (createDirectoryIfMissing True (cacheDir store))
    present <- liftIO (doesFileExist path)
    unless present (ExceptT (titleCard tools title author path))
    pure (path, name)

writeRelease :: FilePath -> Text -> Text -> LibraryMeta -> (FilePath, Text) -> [Shelf] -> IO ()
writeRelease partial secret publicUrl meta (libraryArtwork, libraryArtworkFile) shelves = do
    let root = partial </> T.unpack secret
        base = listenerBase publicUrl secret
    createDirectoryIfMissing True root
    TIO.writeFile (root </> "style.css") listenerStyle
    link libraryArtwork (root </> T.unpack libraryArtworkFile)
    TIO.writeFile (root </> "feed.xml") (renderFeed (libraryChannel base meta libraryArtworkFile shelves))
    BL.writeFile (root </> "index.html") (renderHtml (libraryPage base meta libraryArtworkFile shelves))
    forM_ shelves $ \shelf -> do
        let slug = entrySlug (shelfEntry shelf)
            dir = root </> T.unpack slug
        createDirectoryIfMissing True dir
        link (shelfArtwork shelf) (dir </> T.unpack (shelfArtworkFile shelf))
        forM_ (shelfChapters shelf) $ \published -> link (publishedAudio published) (dir </> T.unpack (publishedFile published))
        TIO.writeFile (dir </> "feed.xml") (renderFeed (bookChannel base shelf))
        BL.writeFile (dir </> "index.html") (renderHtml (bookPage base shelf))

-- | Hard-link a cached file into a release, copying if linking fails.
link :: FilePath -> FilePath -> IO ()
link source target = createLink source target `catch` \(_ :: IOException) -> BL.readFile source >>= BL.writeFile target

swapSite :: Store -> FilePath -> IO ()
swapSite store release = do
    let next = sitePath store <.> "next"
    stale <- pathIsSymbolicLink next `catch` \(_ :: IOException) -> pure False
    when stale (removeFile next)
    createSymbolicLink ("releases" </> release) next
    rename next (sitePath store)

-- | Keep the current release and the newest other one; remove the rest.
prune :: Store -> FilePath -> IO ()
prune store current = do
    names <- listDirectory (releasesDir store)
    let others = sortOn Down [name | name <- names, name /= current, not (".partial" `T.isSuffixOf` T.pack name)]
        stale = drop 1 others ++ [name | name <- names, ".partial" `T.isSuffixOf` T.pack name]
    forM_ stale $ \name -> removeDirectoryRecursive (releasesDir store </> name)

-- Feeds ----------------------------------------------------------------

bookChannel :: Text -> Shelf -> Channel
bookChannel base shelf =
    Channel
        { channelTitle = bookTitle book
        , channelLink = bookBase
        , channelDescription = describe book
        , channelAuthor = bookAuthor book
        , channelImage = bookBase <> shelfArtworkFile shelf
        , channelItems = map (item base shelf Nothing) (newestFirst (shelfChapters shelf))
        }
  where
    book = entryBook (shelfEntry shelf)
    bookBase = base <> entrySlug (shelfEntry shelf) <> "/"

libraryChannel :: Text -> LibraryMeta -> Text -> [Shelf] -> Channel
libraryChannel base meta artworkFile shelves =
    Channel
        { channelTitle = metaTitle meta
        , channelLink = base
        , channelDescription = "Every book in " <> metaTitle meta <> ", one season per book."
        , channelAuthor = Nothing
        , channelImage = base <> artworkFile
        , channelItems =
            [ item base shelf (Just (bookSeason (entryBook (shelfEntry shelf)))) published
            | shelf <- sortOn (Down . bookSeason . entryBook . shelfEntry) shelves
            , published <- newestFirst (shelfChapters shelf)
            ]
        }

item :: Text -> Shelf -> Maybe Int -> Published -> Item
item base shelf season published =
    Item
        { itemTitle = maybe title (const (bookTitle book <> " · " <> title)) season
        , itemGuid = "abrec:" <> slug <> ":" <> chapterId chapter <> ":" <> T.take 16 (masterSha256 (chapterMaster chapter))
        , itemUrl = bookBase <> publishedFile published
        , itemBytes = encodedBytes (publishedEncoded published)
        , itemSeconds = round (encodedSeconds (publishedEncoded published))
        , itemPublished = addUTCTime (fromIntegral (chapterNumber chapter * 60)) (bookCreated book)
        , itemEpisode = chapterNumber chapter
        , itemSeason = season
        , itemImage = (bookBase <> shelfArtworkFile shelf) <$ season
        }
  where
    chapter = publishedChapter published
    title = episodeTitle chapter
    book = entryBook (shelfEntry shelf)
    slug = entrySlug (shelfEntry shelf)
    bookBase = base <> slug <> "/"

newestFirst :: [Published] -> [Published]
newestFirst = sortOn (Down . chapterNumber . publishedChapter)

describe :: Book -> Text
describe book = fromMaybe ("Read aloud: " <> bookTitle book) (bookDescription book)

-- Listener pages -------------------------------------------------------

libraryPage :: Text -> LibraryMeta -> Text -> [Shelf] -> Html
libraryPage base meta artworkFile shelves =
    page
        (metaTitle meta)
        "style.css"
        [hsx|
            <header>
                <img class="art" src={artworkFile} alt=""/>
                <h1>{metaTitle meta}</h1>
            </header>
            <p class="note">Follow each book on its own page, or everything at once below. Choose one way: following both shows every chapter twice.</p>
            <ul class="books">{mconcat (map bookItem shelves)}</ul>
            <section class="follow">
                <h2>Follow everything</h2>
                <p>One show with every book as a season. New books appear without following again.</p>
                {followLinks (base <> "feed.xml")}
            </section>
        |]
  where
    bookItem shelf =
        let entry = shelfEntry shelf
            slug = entrySlug entry
         in [hsx|
                <li>
                    <a href={slug <> "/"}>
                        <img class="thumb" src={slug <> "/" <> shelfArtworkFile shelf} alt=""/>
                        <span class="title">{bookTitle (entryBook entry)}</span>
                    </a>
                    <span class="meta">{chapterCount (length (shelfChapters shelf))}</span>
                </li>
            |]

bookPage :: Text -> Shelf -> Html
bookPage base shelf =
    page
        (bookTitle book)
        "../style.css"
        [hsx|
            <p><a href="../">All books</a></p>
            <header>
                <img class="art" src={shelfArtworkFile shelf} alt=""/>
                <div>
                    <h1>{bookTitle book}</h1>
                    {maybe mempty authorLine (bookAuthor book)}
                </div>
            </header>
            {maybe mempty descriptionLine (bookDescription book)}
            <section class="follow">
                <h2>Follow this book</h2>
                {followLinks (base <> entrySlug (shelfEntry shelf) <> "/feed.xml")}
            </section>
            <ol class="chapters">{mconcat (map chapterItem (shelfChapters shelf))}</ol>
        |]
  where
    book = entryBook (shelfEntry shelf)
    authorLine author = [hsx|<p class="meta">{author}</p>|]
    descriptionLine description = [hsx|<p>{description}</p>|]
    chapterItem published =
        [hsx|
            <li>
                <span class="title">{episodeTitle (publishedChapter published)}</span>
                <span class="meta">{minutes (encodedSeconds (publishedEncoded published))}</span>
                <audio controls="controls" preload="none" src={publishedFile published}></audio>
            </li>
        |]

-- | A podcast-app link and the feed address to copy.
followLinks :: Text -> Html
followLinks feed =
    [hsx|
        <p><a class="button" href={podcastLink}>Open in Apple Podcasts</a></p>
        <p>Or add this address in your podcast app:</p>
        <p><input class="feed" type="text" value={feed} readonly="readonly"/></p>
    |]
  where
    podcastLink = maybe feed ("podcast://" <>) (T.stripPrefix "https://" feed)

page :: Text -> Text -> Html -> Html
page title stylesheet body =
    [hsx|
        <!DOCTYPE html>
        <html lang="en">
            <head>
                <meta charset="utf-8"/>
                <meta name="viewport" content="width=device-width, initial-scale=1"/>
                <meta name="robots" content="noindex, nofollow"/>
                <title>{title}</title>
                <link rel="stylesheet" href={stylesheet}/>
            </head>
            <body><main>{body}</main></body>
        </html>
    |]

chapterCount :: Int -> Text
chapterCount 1 = "1 chapter"
chapterCount n = T.pack (show n) <> " chapters"

minutes :: Double -> Text
minutes value
    | value < 60 = "under a minute"
    | otherwise = T.pack (show (round (value / 60) :: Int)) <> " min"

listenerStyle :: Text
listenerStyle =
    T.unlines
        [ "body { margin: 0; font: 17px/1.5 system-ui, sans-serif; background: #f6f5f1; color: #2f3e46; }"
        , "main { max-width: 40rem; margin: 0 auto; padding: 1.5rem 1rem 4rem; }"
        , "header { display: flex; gap: 1rem; align-items: center; margin-bottom: 1rem; }"
        , "h1 { margin: 0; font-size: 1.6rem; } h2 { font-size: 1.15rem; }"
        , "a { color: #2f6f73; }"
        , ".art { width: 7rem; height: 7rem; border-radius: 0.5rem; }"
        , ".thumb { width: 3.5rem; height: 3.5rem; border-radius: 0.3rem; vertical-align: middle; margin-right: 0.75rem; }"
        , "ul.books, ol.chapters { list-style: none; padding: 0; }"
        , "ul.books li, ol.chapters li { padding: 0.6rem 0; border-bottom: 1px solid #ddd8cc; }"
        , ".meta { color: #6b705c; font-size: 0.9rem; margin-left: 0.5rem; }"
        , "audio { display: block; width: 100%; margin-top: 0.4rem; }"
        , ".follow { background: #fff; border-radius: 0.5rem; padding: 0.25rem 1rem 0.75rem; margin: 1.5rem 0; }"
        , ".button { display: inline-block; background: #2f3e46; color: #fff; padding: 0.5rem 1rem; border-radius: 0.4rem; text-decoration: none; }"
        , ".feed { width: 100%; font: inherit; font-size: 0.85rem; padding: 0.4rem; box-sizing: border-box; }"
        , ".note { color: #6b705c; }"
        ]
