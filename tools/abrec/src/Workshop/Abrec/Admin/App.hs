{-# LANGUAGE OverloadedStrings #-}

-- | The admin web application: routes over the library and publisher.
module Workshop.Abrec.Admin.App (
    Env (..),
    adminApp,
) where

import Control.Monad (forM, forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 qualified as BC
import Data.Char (isDigit)
import Data.Either (lefts, rights)
import Data.List (nub)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Text.Lazy qualified as LT
import Data.Time (getCurrentTimeZone)
import Network.HTTP.Types (Status, status400, status404)
import Network.Wai (Application)
import Network.Wai.Parse (FileInfo (..), defaultParseRequestBodyOptions)
import System.Directory (doesFileExist)
import System.FilePath (takeBaseName, takeExtension, (</>))
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Read (readMaybe)
import Web.Scotty (ActionM, File, ScottyM, file, filesOpts, finish, formParamMaybe, get, html, pathParam, post, redirect, scottyApp, setHeader, status)
import Workshop.Abrec.Admin.Security (adminGuard)
import Workshop.Abrec.Admin.Views
import Workshop.Abrec.Config (Config (..))
import Workshop.Abrec.Library
import Workshop.Abrec.Media (Tools, fetchHttps, normaliseCover, probeAudio)
import Workshop.Abrec.Publisher (Publisher, publishState, requestPublish)
import Workshop.Abrec.Site (bookFeedUrl, libraryFeedUrl, listenerBase)

data Env = Env
    { envConfig :: Config
    , envTools :: Tools
    , envStore :: Store
    , envPublisher :: Publisher
    , envStatic :: FilePath
    -- ^ admin.css and htmx.min.js
    }

adminApp :: Env -> IO Application
adminApp env = adminGuard (configAdminDomain (envConfig env)) <$> scottyApp (routes env)

routes :: Env -> ScottyM ()
routes env = do
    get "/static/:file" $ do
        name <- pathParam "file"
        case lookup (name :: Text) [("admin.css", "text/css; charset=utf-8"), ("htmx.min.js", "text/javascript; charset=utf-8")] of
            Nothing -> notFound
            Just mime -> setHeader "Content-Type" mime >> file (envStatic env </> T.unpack name)

    get "/status" $ statusFragment env >>= html . renderHtml

    post "/publish" $ do
        published env
        redirect "/"

    get "/" $ do
        library <- liftIO (loadLibrary store)
        secret <- liftIO (readSecret store)
        render env (metaTitle (libraryMeta library)) (libraryView library (listenerBase publicUrl secret) (libraryFeedUrl publicUrl secret))

    post "/library" $ do
        title <- field "title"
        outcome "/" (setLibraryTitle store title)
        redirect "/"

    post "/books" $ do
        new <- NewBook <$> field "title" <*> optionalField "author" <*> optionalField "description"
        slug <- outcome "/" (createBook store new)
        published env
        redirect (LT.fromStrict ("/books/" <> slug))

    get "/books/:slug" $ withEntry env $ \entry -> do
        secret <- liftIO (readSecret store)
        let slug = entrySlug entry
        render env (bookTitle (entryBook entry)) (bookView entry (bookFeedUrl publicUrl secret slug) (listenerBase publicUrl secret <> slug <> "/"))

    post "/books/:slug" $ withEntry env $ \entry -> do
        edit <- NewBook <$> field "title" <*> optionalField "author" <*> optionalField "description"
        changed env (bookPath entry) (updateBook store (entrySlug entry) edit)

    post "/books/:slug/hidden" $ withEntry env $ \entry -> do
        hidden <- (== "on") <$> field "hidden"
        changed env (bookPath entry) (setBookHidden store (entrySlug entry) hidden)

    get "/books/:slug/delete" $ withEntry env $ \entry ->
        render env "Delete book" $
            confirmView
                "Delete book"
                ("Delete " <> bookTitle (entryBook entry) <> " and all " <> tshow (length (entryChapters entry)) <> " chapters permanently? Listeners lose this book's feed.")
                (bookPath entry <> "/delete")
                (bookPath entry)

    post "/books/:slug/delete" $ withEntry env $ \entry -> do
        changed env "/" (deleteBook store (entrySlug entry))

    get "/books/:slug/cover" $ withEntry env $ \entry -> do
        let slug = entrySlug entry
        present <- liftIO (doesFileExist (coverPath store slug))
        if present
            then setHeader "Content-Type" "image/jpeg" >> file (coverPath store slug)
            else do
                -- The title card the listener sees, from the published site.
                secret <- liftIO (readSecret store)
                redirect (LT.fromStrict (listenerBase publicUrl secret <> slug <> "/"))

    post "/books/:slug/cover" $ withEntry env $ \entry -> filesOpts defaultParseRequestBodyOptions $ \params files -> do
        let slug = entrySlug entry
            target = scratchDir store </> T.unpack slug <> ".cover.jpg"
            url = T.strip (fromMaybe "" (lookup "url" params))
        source <- case [info | (_, info) <- files, not (BC.null (fileName info))] of
            info : _ -> pure (fileContent info)
            []
                | T.null url -> failWith status400 "Choose an image or paste an address." (bookPath entry)
                | otherwise -> do
                    let fetched = scratchDir store </> T.unpack slug <> ".fetched"
                    outcome (bookPath entry) (fetchHttps tools url fetched)
                    pure fetched
        outcome (bookPath entry) (normaliseCover tools source target)
        changed env (bookPath entry) (setBookCover store slug target)

    post "/books/:slug/cover/remove" $ withEntry env $ \entry ->
        changed env (bookPath entry) (removeBookCover store (entrySlug entry))

    post "/books/:slug/uploads" $ withEntry env $ \entry -> filesOpts defaultParseRequestBodyOptions $ \_ files -> do
        staged <- stageUploads env (bookPath entry) files
        when (null staged) (failWith status400 "Choose at least one audio file." (bookPath entry))
        ident <- liftIO (createUpload store staged)
        redirect (LT.fromStrict (bookPath entry <> "/uploads/" <> ident))

    get "/books/:slug/uploads/:upload" $ withEntry env $ \entry -> withUpload env entry $ \ident staged ->
        render env "Add chapters" (uploadView entry ident (uploadRows entry staged))

    post "/books/:slug/uploads/:upload" $ withEntry env $ \entry -> withUpload env entry $ \ident staged -> do
        let back = bookPath entry <> "/uploads/" <> ident
        choices <- forM (zip [0 :: Int ..] staged) $ \(index, file') -> do
            number <- readMaybe . T.unpack <$> field ("number-" <> tshow index)
            title <- optionalField ("title-" <> tshow index)
            replace <- isJust <$> optionalField ("replace-" <> tshow index)
            case number of
                Just n -> pure (n, title, replace, file')
                Nothing -> failWith status400 "Every file needs a chapter number." back
        let numbers = [n | (n, _, _, _) <- choices]
            existing = map chapterNumber (entryChapters entry)
        unless (length (nub numbers) == length numbers) (failWith status400 "Two files have the same chapter number." back)
        forM_ choices $ \(n, _, replace, _) ->
            when (n `elem` existing && not replace) (failWith status400 ("Chapter " <> tshow n <> " already exists; tick Replace to replace it.") back)
        results <- liftIO . forM choices $ \(n, title, replace, file') -> addChapter store (entrySlug entry) (ChapterFields n title Nothing) replace file'
        liftIO (discardUpload store ident)
        published env
        case lefts results of
            [] -> redirect (LT.fromStrict (bookPath entry))
            errors -> failWith status400 (T.unwords errors) (bookPath entry)

    post "/books/:slug/uploads/:upload/discard" $ withEntry env $ \entry -> withUpload env entry $ \ident _ -> do
        liftIO (discardUpload store ident)
        redirect (LT.fromStrict (bookPath entry))

    get "/books/:slug/chapters/:chapter" $ withChapter env $ \entry chapter ->
        render env (episodeTitle chapter) (chapterView entry chapter)

    post "/books/:slug/chapters/:chapter" $ withChapter env $ \entry chapter -> do
        number <- readMaybe . T.unpack <$> field "number"
        title <- optionalField "title"
        episode <- optionalField "episodeTitle"
        case number of
            Nothing -> failWith status400 "The chapter number must be a whole number." (chapterPath' entry chapter)
            Just n -> changed env (chapterPath' entry chapter) (updateChapter store (entrySlug entry) (chapterId chapter) (ChapterFields n title episode))

    post "/books/:slug/chapters/:chapter/hidden" $ withChapter env $ \entry chapter -> do
        hidden <- (== "on") <$> field "hidden"
        changed env (chapterPath' entry chapter) (setChapterHidden store (entrySlug entry) (chapterId chapter) hidden)

    post "/books/:slug/chapters/:chapter/audio" $ withChapter env $ \entry chapter -> filesOpts defaultParseRequestBodyOptions $ \_ files -> do
        staged <- stageUploads env (chapterPath' entry chapter) files
        case staged of
            [one] -> changed env (chapterPath' entry chapter) (replaceChapterAudio store (entrySlug entry) (chapterId chapter) one)
            _ -> failWith status400 "Choose one audio file." (chapterPath' entry chapter)

    get "/books/:slug/chapters/:chapter/master" $ withChapter env $ \entry chapter -> do
        let master = chapterMaster chapter
        setHeader "Content-Type" (LT.fromStrict (audioMime (masterFile master)))
        file (masterPath store (entrySlug entry) master)

    get "/books/:slug/chapters/:chapter/delete" $ withChapter env $ \entry chapter ->
        render env "Delete chapter" $
            confirmView
                "Delete chapter"
                ("Delete " <> episodeTitle chapter <> " permanently? It disappears from listeners' feeds.")
                (chapterPath' entry chapter <> "/delete")
                (chapterPath' entry chapter)

    post "/books/:slug/chapters/:chapter/delete" $ withChapter env $ \entry chapter ->
        changed env (bookPath entry) (deleteChapter store (entrySlug entry) (chapterId chapter))

    get "/secret/rotate" $
        render env "Rotate secret" $
            confirmView "Rotate secret" "Every listener URL stops working. Listeners must follow the feeds again with the new links." "/secret/rotate" "/"

    post "/secret/rotate" $ do
        liftIO (rotateSecret store)
        published env
        redirect "/"
  where
    store = envStore env
    tools = envTools env
    publicUrl = configPublicUrl (envConfig env)

-- Helpers --------------------------------------------------------------

bookPath :: Entry -> Text
bookPath entry = "/books/" <> entrySlug entry

chapterPath' :: Entry -> Chapter -> Text
chapterPath' entry chapter = bookPath entry <> "/chapters/" <> chapterId chapter

render :: Env -> Text -> Html -> ActionM ()
render env title body = do
    status' <- statusFragment env
    html (renderHtml (layout title status' body))

statusFragment :: Env -> ActionM Html
statusFragment env = liftIO $ statusView <$> getCurrentTimeZone <*> publishState (envPublisher env)

published :: Env -> ActionM ()
published env = liftIO (requestPublish (envPublisher env))

-- | Apply a library change, publish, and return to a page; or show its error.
changed :: Env -> Text -> IO (Either Text ()) -> ActionM ()
changed env back action = do
    outcome back action
    published env
    redirect (LT.fromStrict back)

outcome :: Text -> IO (Either Text a) -> ActionM a
outcome back action = liftIO action >>= either (\err -> failWith status400 err back) pure

failWith :: Status -> Text -> Text -> ActionM a
failWith code message back = do
    status code
    html (renderHtml (layout "Problem" mempty (messageView message back)))
    finish

notFound :: ActionM a
notFound = failWith status404 "Not found." "/"

withEntry :: Env -> (Entry -> ActionM ()) -> ActionM ()
withEntry env continue = do
    slug <- pathParam "slug"
    library <- liftIO (loadLibrary (envStore env))
    maybe notFound continue (findEntry library slug)

withChapter :: Env -> (Entry -> Chapter -> ActionM ()) -> ActionM ()
withChapter env continue = withEntry env $ \entry -> do
    ident <- pathParam "chapter"
    maybe notFound (continue entry) (findChapter entry ident)

withUpload :: Env -> Entry -> (Text -> [Staged] -> ActionM ()) -> ActionM ()
withUpload env _ continue = do
    ident <- pathParam "upload"
    staged <- liftIO (loadUpload (envStore env) ident)
    maybe notFound (continue ident) staged

-- | Check every uploaded file is audio, then move them all into the library.
stageUploads :: Env -> Text -> [File FilePath] -> ActionM [Staged]
stageUploads env back files = do
    let chosen = [(decodeUtf8Lenient (fileName info), fileContent info) | (_, info) <- files, not (BC.null (fileName info))]
    probed <- liftIO . forM chosen $ \(name, path) -> fmap ((,,) name path) <$> probeAudio (envTools env) path
    case [name <> ": " <> err | ((name, _), Left err) <- zip chosen probed] of
        [] -> liftIO (mapM (\(name, path, seconds) -> stageFile (envStore env) path name seconds) (rights probed))
        problems -> failWith status400 (T.unwords problems) back

-- | Defaults for an upload: the first number in each file name, the rest of the name as a title.
uploadRows :: Entry -> [Staged] -> [UploadRow]
uploadRows entry staged = assign 0 (nextChapterNumber entry) (map guess staged)
  where
    guess file' =
        let stem = T.pack (takeBaseName (T.unpack (stagedSource file')))
            (before, rest) = T.break isDigit stem
            digits = T.takeWhile isDigit rest
            title = T.strip (T.dropAround (\c -> c `elem` ("-_.:" :: String) || c == ' ') (before <> T.drop (T.length digits) rest))
         in (file', readMaybe (T.unpack digits), if T.null digits then "" else title)
    -- Files without a number take the next free one.
    assign _ _ [] = []
    assign index next ((file', number, title) : more) =
        let chosen = fromMaybe next number
         in UploadRow index file' chosen title : assign (index + 1) (max next chosen + 1) more

audioMime :: Text -> Text
audioMime name = case takeExtension (T.unpack name) of
    ".mp3" -> "audio/mpeg"
    ".wav" -> "audio/wav"
    ".flac" -> "audio/flac"
    ".m4a" -> "audio/mp4"
    ".mp4" -> "audio/mp4"
    ".ogg" -> "audio/ogg"
    ".opus" -> "audio/ogg"
    ".webm" -> "audio/webm"
    _ -> "application/octet-stream"

field :: Text -> ActionM Text
field name = fromMaybe "" <$> formParamMaybe (LT.fromStrict name)

optionalField :: Text -> ActionM (Maybe Text)
optionalField name = do
    value <- formParamMaybe (LT.fromStrict name)
    pure (value >>= \text -> if T.null (T.strip text) then Nothing else Just text)

tshow :: Int -> Text
tshow = T.pack . show
