{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Admin pages. Plain forms that post and redirect; htmx only refreshes status.
module Workshop.Abrec.Admin.Views (
    UploadRow (..),
    layout,
    statusView,
    libraryView,
    bookView,
    uploadView,
    chapterView,
    confirmView,
    messageView,
) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (TimeZone, defaultTimeLocale, formatTime, utcToLocalTime)
import IHP.HSX.QQ (hsx)
import Text.Blaze.Html (Html)
import Workshop.Abrec.Library
import Workshop.Abrec.Publisher (PublishState (..))

-- | One staged file on the upload confirmation page.
data UploadRow = UploadRow
    { rowIndex :: Int
    , rowStaged :: Staged
    , rowNumber :: Int
    , rowTitle :: Text
    }

layout :: Text -> Html -> Html -> Html
layout title status body =
    [hsx|
        <!DOCTYPE html>
        <html lang="en">
            <head>
                <meta charset="utf-8"/>
                <meta name="viewport" content="width=device-width, initial-scale=1"/>
                <meta name="robots" content="noindex, nofollow"/>
                <meta name="htmx-config" content={htmxConfig}/>
                <title>{title} · abrec</title>
                <link rel="stylesheet" href="/static/admin.css"/>
                <script src="/static/htmx.min.js" defer="defer"></script>
            </head>
            <body>
                <main>
                    <nav><a href="/">Library</a></nav>
                    {status}
                    {body}
                </main>
            </body>
        </html>
    |]

-- | htmx must not inject inline styles, which the admin CSP forbids.
htmxConfig :: Text
htmxConfig = "{\"includeIndicatorStyles\": false}"

-- | The publish status; polls itself only while publishing.
statusView :: TimeZone -> PublishState -> Html
statusView zone state = case state of
    Publishing ->
        [hsx|<div id="status" class="status publishing" hx-get="/status" hx-trigger="every 2s" hx-swap="outerHTML">Publishing…</div>|]
    NeverPublished ->
        [hsx|<div id="status" class="status publishing" hx-get="/status" hx-trigger="every 2s" hx-swap="outerHTML">Waiting to publish…</div>|]
    Published time ->
        [hsx|<div id="status" class="status">Public site is up to date (published {clock time}).</div>|]
    PublishFailed time err ->
        [hsx|
            <div id="status" class="status failed">
                Publishing failed at {clock time}: {err}
                <form class="inline" method="post" action="/publish"><button type="submit" class="secondary">Try again</button></form>
            </div>
        |]
  where
    clock = T.pack . formatTime defaultTimeLocale "%H:%M:%S" . utcToLocalTime zone

libraryView :: Library -> Text -> Text -> Html
libraryView library listener libraryFeed =
    [hsx|
        <h1>{metaTitle (libraryMeta library)}</h1>
        <section>
            <h2>Books</h2>
            {booksTable}
        </section>
        <section>
            <h2>New book</h2>
            <form method="post" action="/books">
                <label for="title">Title</label>
                <input id="title" type="text" name="title" required="required"/>
                <label for="author">Author (optional)</label>
                <input id="author" type="text" name="author"/>
                <label for="description">Description (optional)</label>
                <textarea id="description" name="description"></textarea>
                <button type="submit">Create book</button>
            </form>
        </section>
        <section>
            <h2>Listener links</h2>
            <p>Listener page: <a class="url" href={listener}>{listener}</a></p>
            <p>Library feed (every book as a season): <span class="url">{libraryFeed}</span></p>
            <p><a href="/secret/rotate">Rotate secret…</a> <span class="muted">Replaces every listener URL.</span></p>
        </section>
        <section>
            <h2>Library title</h2>
            <form method="post" action="/library">
                <label for="library-title">Title of the library feed</label>
                <input id="library-title" type="text" name="title" value={metaTitle (libraryMeta library)} required="required"/>
                <button type="submit">Save</button>
            </form>
        </section>
    |]
  where
    booksTable
        | null (libraryEntries library) = [hsx|<p class="muted">No books yet.</p>|]
        | otherwise =
            [hsx|
                <table>
                    <tr><th>Title</th><th>Author</th><th>Chapters</th></tr>
                    {mconcat (map bookRow (libraryEntries library))}
                </table>
            |]
    bookRow entry =
        [hsx|
            <tr>
                <td><a href={"/books/" <> entrySlug entry}>{bookTitle (entryBook entry)}</a>{hiddenBadge (bookHidden (entryBook entry))}</td>
                <td>{fromMaybe "" (bookAuthor (entryBook entry))}</td>
                <td>{tshow (length (entryChapters entry))}</td>
            </tr>
        |]

bookView :: Entry -> Text -> Text -> Html
bookView entry feed listener =
    [hsx|
        <h1>{bookTitle book}{hiddenBadge (bookHidden book)}</h1>
        {gapWarning}
        <section>
            <h2>Chapters</h2>
            {chaptersTable}
            <h3>Upload chapters</h3>
            <form method="post" action={base <> "/uploads"} enctype="multipart/form-data">
                <input type="file" name="audio" accept="audio/*" multiple="multiple" required="required"/>
                <button type="submit">Upload</button>
                <p class="muted">Chapter numbers are taken from a number in each file name; you can change them before saving.</p>
            </form>
        </section>
        <section>
            <h2>Details</h2>
            <form method="post" action={base}>
                <label for="title">Title</label>
                <input id="title" type="text" name="title" value={bookTitle book} required="required"/>
                <label for="author">Author (optional)</label>
                <input id="author" type="text" name="author" value={fromMaybe "" (bookAuthor book)}/>
                <label for="description">Description (optional)</label>
                <textarea id="description" name="description">{fromMaybe "" (bookDescription book)}</textarea>
                <button type="submit">Save details</button>
            </form>
        </section>
        <section>
            <h2>Cover</h2>
            <img class="cover" src={base <> "/cover"} alt="Current cover or title card"/>
            {coverNote}
            <form method="post" action={base <> "/cover"} enctype="multipart/form-data">
                <label for="cover-file">Upload an image</label>
                <input id="cover-file" type="file" name="cover" accept="image/*"/>
                <label for="cover-url">or paste an https:// address</label>
                <input id="cover-url" type="url" name="url" placeholder="https://"/>
                <button type="submit">Set cover</button>
            </form>
            {removeCover}
        </section>
        <section>
            <h2>Listening</h2>
            <p>Feed for this book: <span class="url">{feed}</span></p>
            <p>Listener page: <a class="url" href={listener}>{listener}</a></p>
            <form method="post" action={base <> "/hidden"}>
                <input type="hidden" name="hidden" value={if bookHidden book then "off" else "on" :: Text}/>
                <button type="submit" class="secondary">{if bookHidden book then "Show this book" else "Hide this book" :: Text}</button>
            </form>
            <p><a href={base <> "/delete"}>Delete this book…</a></p>
        </section>
    |]
  where
    book = entryBook entry
    base = "/books/" <> entrySlug entry
    gapWarning = case missingNumbers entry of
        [] -> mempty
        gaps -> [hsx|<p class="warning">Missing chapter numbers: {T.intercalate ", " (map tshow gaps)}</p>|]
    coverNote = case bookCoverSha256 book of
        Nothing -> [hsx|<p class="muted">No cover; listeners see this title card.</p>|]
        Just _ -> mempty
    removeCover = case bookCoverSha256 book of
        Nothing -> mempty
        Just _ ->
            [hsx|
                <form method="post" action={base <> "/cover/remove"}>
                    <button type="submit" class="secondary">Remove cover</button>
                </form>
            |]
    chaptersTable
        | null (entryChapters entry) = [hsx|<p class="muted">No chapters yet.</p>|]
        | otherwise =
            [hsx|
                <table>
                    <tr><th>No.</th><th>Episode title</th><th>Length</th><th>Recorded</th></tr>
                    {mconcat (map chapterRow (entryChapters entry))}
                </table>
            |]
    chapterRow chapter =
        [hsx|
            <tr>
                <td>{tshow (chapterNumber chapter)}</td>
                <td><a href={base <> "/chapters/" <> chapterId chapter}>{episodeTitle chapter}</a>{hiddenBadge (chapterHidden chapter)}</td>
                <td>{duration (masterSeconds (chapterMaster chapter))}</td>
                <td>{T.pack (formatTime defaultTimeLocale "%e %b %Y" (chapterRecorded chapter))}</td>
            </tr>
        |]

uploadView :: Entry -> Text -> [UploadRow] -> Html
uploadView entry ident rows =
    [hsx|
        <h1>Add chapters to {bookTitle (entryBook entry)}</h1>
        <form method="post" action={action}>
            <table>
                <tr><th>File</th><th>Chapter</th><th>Title (optional)</th><th></th></tr>
                {mconcat (map row rows)}
            </table>
            <button type="submit">Save chapters</button>
        </form>
        <form method="post" action={action <> "/discard"}>
            <button type="submit" class="secondary">Discard upload</button>
        </form>
    |]
  where
    action = "/books/" <> entrySlug entry <> "/uploads/" <> ident
    taken = map chapterNumber (entryChapters entry)
    row upload =
        let index = tshow (rowIndex upload)
         in [hsx|
                <tr>
                    <td>{stagedSource (rowStaged upload)}<br/><span class="muted">{duration (stagedSeconds (rowStaged upload))}</span></td>
                    <td><input type="number" min="1" name={"number-" <> index} value={tshow (rowNumber upload)} required="required"/></td>
                    <td><input type="text" name={"title-" <> index} value={rowTitle upload}/></td>
                    <td>{replaceBox index (rowNumber upload)}</td>
                </tr>
            |]
    replaceBox index number
        | number `elem` taken =
            [hsx|<label class="muted"><input type="checkbox" name={"replace-" <> index}/> Replace existing chapter {tshow number}</label>|]
        | otherwise = mempty

chapterView :: Entry -> Chapter -> Html
chapterView entry chapter =
    [hsx|
        <p><a href={bookBase}>{bookTitle (entryBook entry)}</a></p>
        <h1>{episodeTitle chapter}{hiddenBadge (chapterHidden chapter)}</h1>
        <audio controls="controls" preload="none" src={base <> "/master"}></audio>
        <p class="muted">{chapterSource chapter} · {duration (masterSeconds (chapterMaster chapter))}</p>
        <section>
            <h2>Details</h2>
            <form method="post" action={base}>
                <label for="number">Chapter number</label>
                <input id="number" type="number" min="1" name="number" value={tshow (chapterNumber chapter)} required="required"/>
                <label for="title">Chapter title (optional)</label>
                <input id="title" type="text" name="title" value={fromMaybe "" (chapterTitle chapter)}/>
                <label for="episode-title">Episode title (optional free text)</label>
                <input id="episode-title" type="text" name="episodeTitle" value={fromMaybe "" (chapterEpisodeTitle chapter)} placeholder={defaultTitle}/>
                <p class="muted">Leave empty to use the number and chapter title.</p>
                <button type="submit">Save</button>
            </form>
        </section>
        <section>
            <h2>Audio</h2>
            <form method="post" action={base <> "/audio"} enctype="multipart/form-data">
                <label for="audio">Replace this chapter's audio</label>
                <input id="audio" type="file" name="audio" accept="audio/*" required="required"/>
                <button type="submit">Replace audio</button>
            </form>
        </section>
        <section>
            <h2>Visibility</h2>
            <form method="post" action={base <> "/hidden"}>
                <input type="hidden" name="hidden" value={if chapterHidden chapter then "off" else "on" :: Text}/>
                <button type="submit" class="secondary">{if chapterHidden chapter then "Show this chapter" else "Hide this chapter" :: Text}</button>
            </form>
            <p><a href={base <> "/delete"}>Delete this chapter…</a></p>
        </section>
    |]
  where
    bookBase = "/books/" <> entrySlug entry
    base = bookBase <> "/chapters/" <> chapterId chapter
    defaultTitle = episodeTitle chapter{chapterEpisodeTitle = Nothing}

-- | Ask before something permanent.
confirmView :: Text -> Text -> Text -> Text -> Html
confirmView heading message action cancel =
    [hsx|
        <h1>{heading}</h1>
        <p class="warning">{message}</p>
        <form method="post" action={action}>
            <button type="submit" class="danger">{heading}</button>
            <a href={cancel}>Cancel</a>
        </form>
    |]

messageView :: Text -> Text -> Html
messageView message back =
    [hsx|
        <p class="error">{message}</p>
        <p><a href={back}>Back</a></p>
    |]

hiddenBadge :: Bool -> Html
hiddenBadge True = [hsx|<span class="badge">hidden</span>|]
hiddenBadge False = mempty

duration :: Double -> Text
duration value =
    let total = round value :: Int
        (hours, rest) = total `divMod` 3600
        (minutes, seconds) = rest `divMod` 60
        pad n = T.justifyRight 2 '0' (tshow n)
     in if hours > 0 then tshow hours <> ":" <> pad minutes <> ":" <> pad seconds else tshow minutes <> ":" <> pad seconds

tshow :: Int -> Text
tshow = T.pack . show
