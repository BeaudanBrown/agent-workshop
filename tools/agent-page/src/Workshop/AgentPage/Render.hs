{-# LANGUAGE OverloadedStrings #-}

{- | Render a document to one self-contained HTML page.

Pages that only present content need no scripts: media is embedded as data
URIs and tabs switch with radio inputs and CSS, so the page survives
sanitising previews that strip scripts, links and external resources.

Pages with questions or reply boxes are interactive. One inline script turns
what the reader enters into reply text to copy back to the agent. Without
scripts the page still reads normally and says that replying needs a
browser.
-}
module Workshop.AgentPage.Render (Placed (..), renderPage) where

import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Builder (Builder, byteString, intDec, stringUtf8)
import Data.List (intersperse, mapAccumL)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8Builder)
import Workshop.AgentPage.Document
import Workshop.AgentPage.Media (Embedded (..), Encoded (..))

-- | Encoded media with the caption and line written beside it in the source.
data Placed = Placed
    { placedCaption :: Text
    , placedLine :: Int
    , placedMedia :: Encoded
    }

-- | Where a block sits, for reply markers.
data Env = Env
    { envReply :: Bool
    , envPath :: [Text]
    -- ^ section heading and tab label
    , envInQuestion :: Bool
    }

data Counters = Counters
    { nextGroup :: Int
    -- ^ numbers tab groups so their inputs have unique names
    , nextQuestion :: Int
    }

{- | Render a page. The footer identifies the tool and format version. The
source path and the leading hex digits of its SHA-256 appear in reply text,
so the agent can find what each reply is about and notice when the source
has changed since the page was built.
-}
renderPage :: Text -> FilePath -> Text -> Document Placed -> Builder
renderPage footer source digest doc =
    mconcat
        [ "<!doctype html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n"
        , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"
        , "<title>"
        , escape (docTitle doc)
        , "</title>\n<style>\n"
        , stylesheet
        , if interactive then interactiveStylesheet else mempty
        , foldMap tabRule [1 .. maximum (0 : concatMap (concatMap tabCounts . sectionBlocks) (docSections doc))]
        , "</style>\n</head>\n<body>\n"
        , if interactive
            then
                "<main data-title=\""
                    <> escape (docTitle doc)
                    <> "\" data-source=\""
                    <> escape (T.pack source)
                    <> "\" data-source-sha256=\""
                    <> escape digest
                    <> "\">\n"
            else "<main>\n"
        , "<header>\n<h1>"
        , escape (docTitle doc)
        , "</h1>\n"
        , metaLine
        , "</header>\n"
        , mconcat renderedSections
        , if interactive then respondPanel else mempty
        , "</main>\n"
        , if interactive then "<script>\n" <> encodeUtf8Builder script <> "</script>\n" else mempty
        , "<footer>"
        , escape footer
        , "</footer>\n</body>\n</html>\n"
        ]
  where
    interactive = docReply doc || any (any asks . sectionBlocks) (docSections doc)
    metaLine = case filter (not . T.null) (maybe [] pure (docMeta doc) ++ maybe [] pure (docDate doc)) of
        [] -> mempty
        parts -> "<p class=\"meta\">" <> escape (T.intercalate " · " parts) <> "</p>\n"
    (_, renderedSections) = mapAccumL (renderSection (docReply doc)) (Counters 1 1) (docSections doc)

asks :: Block media -> Bool
asks block = case block of
    Ask _ -> True
    Tabs tabs -> any (any asks . tabBlocks) tabs
    _ -> False

renderSection :: Bool -> Counters -> Section Placed -> (Counters, Builder)
renderSection reply counters section =
    let path = maybe [] pure (sectionHeading section)
        (after, blocks) = mapAccumL (renderBlock (Env reply path False)) counters (sectionBlocks section)
        heading = foldMap (\text -> "<h2>" <> escape text <> "</h2>\n") (sectionHeading section)
        box = case sectionHeading section of
            Just _ | reply -> replyBox "section" (sectionLine section) path Nothing "Reply to this section"
            _ -> mempty
     in (after, "<section>\n" <> heading <> mconcat blocks <> box <> "</section>\n")

renderBlock :: Env -> Counters -> Block Placed -> (Counters, Builder)
renderBlock env counters block = case block of
    Paragraph text -> (counters, "<p>" <> escape text <> "</p>\n")
    List items -> (counters, "<ul>\n" <> foldMap (\item -> "<li>" <> escape item <> "</li>\n") items <> "</ul>\n")
    Code label codeLines -> (counters, renderCode label codeLines)
    Table header rows -> (counters, renderTable header rows)
    Media placed -> (counters, renderMedia env placed)
    Ask question -> renderQuestion env counters question
    Tabs tabs ->
        let group = "t" <> intDec (nextGroup counters)
            inputId index = group <> "-" <> intDec index
            controls =
                [ "<input type=\"radio\" name=\""
                    <> group
                    <> "\" id=\""
                    <> inputId index
                    <> "\""
                    <> (if index == 1 then " checked" else "")
                    <> ">"
                    <> "<label for=\""
                    <> inputId index
                    <> "\">"
                    <> escape (tabLabel tab)
                    <> "</label>\n"
                | (index, tab) <- zip [1 :: Int ..] tabs
                ]
            (after, panels) = mapAccumL renderPanel counters{nextGroup = nextGroup counters + 1} tabs
         in (after, "<div class=\"tabs\">\n" <> mconcat controls <> mconcat panels <> "</div>\n")
  where
    renderPanel inner tab =
        let (after, blocks) = mapAccumL (renderBlock env{envPath = envPath env ++ [tabLabel tab]}) inner (tabBlocks tab)
         in (after, "<div class=\"panel\">\n" <> mconcat blocks <> "</div>\n")

renderMedia :: Env -> Placed -> Builder
renderMedia env (Placed captionText line encoded) = case encodedAs encoded of
    EmbeddedImage ->
        "<figure>\n<img src=\"" <> source <> "\" alt=\"" <> caption <> "\"" <> dimensions <> ">\n" <> figcaption "image"
    EmbeddedVideo loops ->
        "<figure>\n<video controls muted playsinline preload=\"metadata\""
            <> dimensions
            <> (if loops then " loop autoplay" else "")
            <> ">\n<source type=\""
            <> stringUtf8 (encodedMime encoded)
            <> "\" src=\""
            <> source
            <> "\">\n</video>\n"
            <> figcaption "video"
  where
    source = "data:" <> stringUtf8 (encodedMime encoded) <> ";base64," <> byteString (Base64.encode (encodedBytes encoded))
    dimensions = " width=\"" <> intDec (encodedWidth encoded) <> "\" height=\"" <> intDec (encodedHeight encoded) <> "\""
    caption = escape captionText
    figcaption kind =
        "<figcaption>"
            <> caption
            <> "</figcaption>\n"
            <> (if envReply env && not (envInQuestion env) then feedback kind else mempty)
            <> "</figure>\n"
    feedback kind =
        "<div class=\"feedback\" data-item data-reply=\""
            <> kind
            <> "\" data-line=\""
            <> intDec line
            <> "\""
            <> pathAttribute (envPath env)
            <> " data-caption=\""
            <> caption
            <> "\">\n<div class=\"verdict\">"
            <> foldMap verdict ["OK", "Change", "Question"]
            <> "</div>\n<details><summary>Reply</summary>\n<textarea rows=\"3\" aria-label=\"Reply\"></textarea>\n</details>\n</div>\n"
    verdict name =
        "<label><input type=\"radio\" name=\"v"
            <> intDec line
            <> "\" value=\""
            <> name
            <> "\"><span>"
            <> name
            <> "</span></label>"

-- | A code block; a diff colours added, removed and hunk lines.
renderCode :: Maybe Text -> [Text] -> Builder
renderCode label codeLines =
    "<div class=\"code\">"
        <> foldMap (\text -> "<div class=\"lang\">" <> escape text <> "</div>") label
        <> "<pre><code>"
        <> body
        <> "</code></pre></div>\n"
  where
    body
        | label == Just "diff" = mconcat (intersperse "\n" (map diffLine codeLines))
        | otherwise = escape (T.intercalate "\n" codeLines)
    diffLine text
        | any (`T.isPrefixOf` text) ["+++", "---"] = marked "meta" text
        | "+" `T.isPrefixOf` text = marked "add" text
        | "-" `T.isPrefixOf` text = marked "del" text
        | "@@" `T.isPrefixOf` text = marked "hunk" text
        | otherwise = escape text
    marked kind text = "<span class=\"" <> kind <> "\">" <> escape text <> "</span>"

renderTable :: [Text] -> [[Text]] -> Builder
renderTable header rows =
    "<div class=\"table\"><table>\n<thead><tr>"
        <> foldMap (cell "th") header
        <> "</tr></thead>\n<tbody>\n"
        <> foldMap (\row -> "<tr>" <> foldMap (cell "td") row <> "</tr>\n") rows
        <> "</tbody>\n</table></div>\n"
  where
    cell tag text = "<" <> tag <> ">" <> escape text <> "</" <> tag <> ">"

renderQuestion :: Env -> Counters -> Question Placed -> (Counters, Builder)
renderQuestion env counters question =
    ( after
    , mconcat
        [ "<fieldset class=\"q\" data-item data-q=\""
        , intDec number
        , "\" data-kind=\""
        , kindName
        , "\" data-line=\""
        , intDec (questionLine question)
        , "\""
        , pathAttribute (envPath env)
        , " data-text=\""
        , escape (questionText question)
        , "\">\n<legend><span class=\"qn\">Q"
        , intDec number
        , "</span> "
        , escape (questionText question)
        , "</legend>\n"
        , mconcat context
        , body
        , "</fieldset>\n"
        ]
    )
  where
    number = nextQuestion counters
    (after, context) =
        mapAccumL (renderBlock env{envInQuestion = True}) counters{nextQuestion = number + 1} (questionContext question)
    name = "q" <> intDec number
    (kindName, inputType) = case questionKind question of
        SingleChoice -> ("choice", "radio")
        MultipleChoice -> ("multi", "checkbox")
        FreeText -> ("text", "") :: (Builder, Builder)
    pick extra = "<input type=\"" <> inputType <> "\" name=\"" <> name <> "\"" <> extra <> ">"
    option choice =
        "<label class=\"opt\">"
            <> pick (" value=\"" <> escape (choiceLabel choice) <> "\"")
            <> " <span>"
            <> escape (choiceLabel choice)
            <> "</span>"
            <> (if choiceRecommended choice then " <span class=\"rec\">recommended</span>" else mempty)
            <> "</label>\n"
    body = case questionKind question of
        FreeText -> "<textarea class=\"ap-answer\" rows=\"4\" aria-label=\"Answer\"></textarea>\n"
        _ ->
            "<div class=\"opts\">\n"
                <> foldMap option (questionChoices question)
                <> "<label class=\"opt\">"
                <> pick " class=\"ap-other-pick\""
                <> " <span>Other</span> <input type=\"text\" class=\"ap-other\" aria-label=\"Other answer\"></label>\n"
                <> "</div>\n<details class=\"note\"><summary>Add a note</summary>\n"
                <> "<textarea class=\"ap-note\" rows=\"2\" aria-label=\"Note\"></textarea>\n</details>\n"

-- | A collapsed reply box marked with what it replies to.
replyBox :: Builder -> Int -> [Text] -> Maybe Text -> Builder -> Builder
replyBox kind line path caption summary =
    "<details class=\"reply\" data-item data-reply=\""
        <> kind
        <> "\" data-line=\""
        <> intDec line
        <> "\""
        <> pathAttribute path
        <> foldMap (\text -> " data-caption=\"" <> escape text <> "\"") caption
        <> "><summary>"
        <> summary
        <> "</summary>\n<textarea rows=\"3\" aria-label=\"Reply\"></textarea>\n</details>\n"

pathAttribute :: [Text] -> Builder
pathAttribute path
    | null path = mempty
    | otherwise = " data-path=\"" <> escape (T.intercalate " › " path) <> "\""

respondPanel :: Builder
respondPanel =
    mconcat
        [ "<section class=\"respond\">\n<h2>Your reply</h2>\n"
        , "<div class=\"overall\" data-item data-reply=\"overall\"><label for=\"ap-overall\">Overall</label>\n"
        , "<textarea id=\"ap-overall\" rows=\"3\"></textarea></div>\n"
        , "<p class=\"ap-static\">Replying needs scripts, and this preview runs without them. Open the page in a browser to answer and copy your reply.</p>\n"
        , "<div class=\"ap-live\">\n<p><button type=\"button\" id=\"ap-copy\">Copy reply</button> <span id=\"ap-status\" role=\"status\"></span></p>\n"
        , "<textarea id=\"ap-out\" readonly rows=\"10\" aria-label=\"Reply text\"></textarea>\n"
        , "<p class=\"hint\">Paste the copied text into the chat. Nothing is saved: reloading the page clears it.</p>\n</div>\n"
        , "</section>\n"
        ]

-- | Tab counts of every tab group in a block, including groups inside panels.
tabCounts :: Block media -> [Int]
tabCounts block = case block of
    Tabs tabs -> length tabs : concatMap (concatMap tabCounts . tabBlocks) tabs
    _ -> []

-- | Show the panel matching the checked input in position n of any group.
tabRule :: Int -> Builder
tabRule n =
    ".tabs>input:nth-of-type(" <> intDec n <> "):checked~.panel:nth-of-type(" <> intDec n <> "){display:block}\n"

escape :: Text -> Builder
escape = encodeUtf8Builder . T.concatMap entity
  where
    entity c = case c of
        '&' -> "&amp;"
        '<' -> "&lt;"
        '>' -> "&gt;"
        '"' -> "&quot;"
        '\'' -> "&#39;"
        _ -> T.singleton c

stylesheet :: Builder
stylesheet =
    mconcat
        [ ":root{color-scheme:light dark;--bg:#f6f7f5;--fg:#18201d;--muted:#5b6862;--card:#fff;--line:#d6ddd8;--accent:#1f6f5c}\n"
        , "@media (prefers-color-scheme:dark){:root{--bg:#111614;--fg:#e3eae6;--muted:#9aa7a0;--card:#19201d;--line:#2d3833;--accent:#6cc4a7}}\n"
        , "*{box-sizing:border-box}\n"
        , "body{margin:0;background:var(--bg);color:var(--fg);font:16px/1.6 system-ui,-apple-system,\"Segoe UI\",sans-serif}\n"
        , "main{max-width:960px;margin:0 auto;padding:32px 20px 8px}\n"
        , "header{margin-bottom:28px}\n"
        , "h1{font-size:2rem;line-height:1.2;margin:0 0 6px}\n"
        , ".meta{color:var(--muted);margin:0;font-size:.95rem}\n"
        , "section{margin:0 0 36px}\n"
        , "h2{font-size:1.35rem;line-height:1.3;margin:0 0 14px;padding-bottom:6px;border-bottom:1px solid var(--line)}\n"
        , "p{margin:0 0 14px}\n"
        , "figure{margin:0 0 22px;background:var(--card);border:1px solid var(--line);border-radius:10px;overflow:hidden}\n"
        , "figure img,figure video{display:block;width:100%;height:auto}\n"
        , "figure video{background:#000}\n"
        , "figcaption{padding:10px 14px;color:var(--muted);font-size:.93rem}\n"
        , ".tabs{margin:0 0 22px}\n"
        , ".tabs>input{position:absolute;opacity:0;pointer-events:none}\n"
        , ".tabs>label{display:inline-block;padding:6px 14px;margin:0 6px 12px 0;border:1px solid var(--line);border-radius:999px;cursor:pointer;color:var(--muted);background:var(--card)}\n"
        , ".tabs>input:checked+label{color:var(--bg);background:var(--accent);border-color:var(--accent)}\n"
        , ".tabs>input:focus-visible+label{outline:2px solid var(--accent);outline-offset:2px}\n"
        , ".tabs>.panel{display:none}\n"
        , "ul{margin:0 0 14px;padding-left:22px}\n"
        , ".code{margin:0 0 18px;background:var(--card);border:1px solid var(--line);border-radius:10px;overflow:hidden}\n"
        , ".code .lang{padding:4px 12px;color:var(--muted);font-size:.8rem;border-bottom:1px solid var(--line)}\n"
        , ".code pre{margin:0;padding:10px 12px;overflow-x:auto;font:13px/1.5 ui-monospace,SFMono-Regular,Menlo,monospace}\n"
        , ".code .add,.code .del{display:inline-block;min-width:100%}\n"
        , ".code .add{background:rgba(46,160,67,.18)}\n"
        , ".code .del{background:rgba(248,81,73,.18)}\n"
        , ".code .hunk{color:var(--accent)}\n"
        , ".code .meta{color:var(--muted)}\n"
        , ".table{margin:0 0 18px;overflow-x:auto}\n"
        , "table{border-collapse:collapse;background:var(--card);font-size:.93rem}\n"
        , "th,td{padding:6px 10px;border:1px solid var(--line);text-align:left;vertical-align:top}\n"
        , "th{background:var(--bg)}\n"
        , "footer{max-width:960px;margin:0 auto;padding:0 20px 28px;color:var(--muted);font-size:.8rem}\n"
        ]

interactiveStylesheet :: Builder
interactiveStylesheet =
    mconcat
        [ "textarea,input[type=text]{width:100%;font:inherit;color:inherit;background:var(--bg);border:1px solid var(--line);border-radius:8px;padding:8px 10px}\n"
        , "textarea:focus,input[type=text]:focus{outline:2px solid var(--accent);outline-offset:1px}\n"
        , "summary{cursor:pointer;color:var(--accent);font-size:.93rem}\n"
        , "section>details.reply{margin:0 0 14px}\n"
        , ".feedback{padding:0 14px 12px}\n"
        , ".verdict{display:flex;flex-wrap:wrap;gap:6px;margin:0 0 6px}\n"
        , ".verdict input{position:absolute;opacity:0;pointer-events:none}\n"
        , ".verdict span{display:inline-block;padding:1px 12px;border:1px solid var(--line);border-radius:999px;cursor:pointer;color:var(--muted);font-size:.88rem}\n"
        , ".verdict input:checked+span{color:var(--bg);background:var(--accent);border-color:var(--accent)}\n"
        , ".verdict input:focus-visible+span{outline:2px solid var(--accent);outline-offset:2px}\n"
        , "details textarea{margin-top:8px}\n"
        , ".q{margin:0 0 22px;padding:14px 16px;background:var(--card);border:1px solid var(--line);border-radius:10px;min-width:0}\n"
        , ".q>legend{float:left;width:100%;padding:0;margin:0 0 12px;font-weight:600}\n"
        , ".q>legend+*{clear:both}\n"
        , ".qn{display:inline-block;margin-right:6px;padding:0 8px;border-radius:999px;background:var(--accent);color:var(--bg);font-size:.85rem}\n"
        , ".q figure{background:var(--bg)}\n"
        , ".opts{margin:0 0 10px}\n"
        , ".opt{display:flex;gap:8px;align-items:center;margin:6px 0;cursor:pointer}\n"
        , ".opt input[type=text]{flex:1;padding:4px 8px}\n"
        , ".rec{font-size:.75rem;padding:0 8px;border:1px solid var(--accent);border-radius:999px;color:var(--accent)}\n"
        , ".overall label{display:block;font-weight:600;margin-bottom:6px}\n"
        , ".overall{margin:0 0 16px}\n"
        , "button{font:inherit;padding:8px 18px;border-radius:999px;border:1px solid var(--accent);background:var(--accent);color:var(--bg);cursor:pointer}\n"
        , "#ap-status{color:var(--muted);font-size:.93rem}\n"
        , "#ap-out{font:13px/1.5 ui-monospace,SFMono-Regular,Menlo,monospace}\n"
        , ".hint,.ap-static{color:var(--muted);font-size:.93rem}\n"
        , ".ap-live{display:none}\n"
        , ".ap-js .ap-live{display:block}\n"
        , ".ap-js .ap-static{display:none}\n"
        ]

{- | Collects every filled-in answer and reply, in page order, into the text
the reader copies back to the agent. Page facts come from data attributes, so
no source text is ever placed inside the script.
-}
script :: Text
script =
    T.unlines
        [ "(function () {"
        , "  var NL = String.fromCharCode(10);"
        , "  var main = document.querySelector('main');"
        , "  var out = document.getElementById('ap-out');"
        , "  var status = document.getElementById('ap-status');"
        , "  if (!main || !out || !status) return;"
        , "  document.documentElement.className += ' ap-js';"
        , "  var source = main.getAttribute('data-source');"
        , "  var file = source.split('/').pop();"
        , "  function value(el) { return el ? el.value.trim() : ''; }"
        , "  function reply() {"
        , "    var parts = [], skipped = [];"
        , "    var items = main.querySelectorAll('[data-item]');"
        , "    for (var i = 0; i < items.length; i++) {"
        , "      var el = items[i], kind = el.getAttribute('data-reply');"
        , "      var at = file + ':' + el.getAttribute('data-line') + (el.hasAttribute('data-path') ? ' · ' + el.getAttribute('data-path') : '');"
        , "      if (el.hasAttribute('data-q')) {"
        , "        var id = 'Q' + el.getAttribute('data-q');"
        , "        var head = id + ' · ' + at + NL + '> ' + el.getAttribute('data-text');"
        , "        if (el.getAttribute('data-kind') === 'text') {"
        , "          var answer = value(el.querySelector('.ap-answer'));"
        , "          if (answer) parts.push(head + NL + answer); else skipped.push(id);"
        , "          continue;"
        , "        }"
        , "        var picks = [], inputs = el.querySelectorAll('.opts input');"
        , "        for (var j = 0; j < inputs.length; j++) {"
        , "          var input = inputs[j];"
        , "          if (input.type === 'text' || !input.checked) continue;"
        , "          if (input.className === 'ap-other-pick') {"
        , "            var other = value(el.querySelector('.ap-other'));"
        , "            picks.push(other ? 'Other: ' + other : 'Other');"
        , "          } else picks.push(input.value);"
        , "        }"
        , "        var note = value(el.querySelector('.ap-note'));"
        , "        if (!picks.length && !note) { skipped.push(id); continue; }"
        , "        parts.push(head + NL + '→ ' + (picks.length ? picks.join('; ') : '(nothing chosen)') + (note ? NL + 'note: ' + note : ''));"
        , "      } else {"
        , "        var text = value(el.querySelector('textarea'));"
        , "        var verdict = el.querySelector('.verdict input:checked');"
        , "        if (!text && !verdict) continue;"
        , "        if (kind === 'overall') { parts.push('Overall' + NL + text); continue; }"
        , "        var caption = el.getAttribute('data-caption');"
        , "        parts.push('Reply · ' + at + ' · ' + kind + (caption ? NL + '> \"' + caption + '\"' : '') + (verdict ? NL + '→ ' + verdict.value : '') + (text ? NL + text : ''));"
        , "      }"
        , "    }"
        , "    if (skipped.length) parts.push('Skipped: ' + skipped.join(', '));"
        , "    if (!parts.length) parts.push('(nothing entered yet)');"
        , "    var header = ['agent-page reply · format 3', 'page: ' + main.getAttribute('data-title'), 'source: ' + source + ' · sha256 ' + main.getAttribute('data-source-sha256')];"
        , "    return header.join(NL) + NL + NL + parts.join(NL + NL) + NL;"
        , "  }"
        , "  function refresh(event) {"
        , "    var target = event && event.target;"
        , "    if (target && target.className === 'ap-other' && target.value.trim()) {"
        , "      var pick = target.parentNode.querySelector('.ap-other-pick');"
        , "      if (pick) pick.checked = true;"
        , "    }"
        , "    out.value = reply();"
        , "    status.textContent = '';"
        , "  }"
        , "  function copied() { status.textContent = 'Copied. Paste it into the chat.'; }"
        , "  function fallback() {"
        , "    out.focus();"
        , "    out.select();"
        , "    var ok = false;"
        , "    try { ok = document.execCommand('copy'); } catch (e) { ok = false; }"
        , "    if (ok) copied(); else status.textContent = 'This view blocked copying. The text below is selected: press Ctrl+C or ⌘C.';"
        , "  }"
        , "  main.addEventListener('input', refresh);"
        , "  main.addEventListener('change', refresh);"
        , "  out.addEventListener('focus', function () { out.select(); });"
        , "  out.addEventListener('click', function () { out.select(); });"
        , "  document.getElementById('ap-copy').addEventListener('click', function () {"
        , "    out.value = reply();"
        , "    if (navigator.clipboard && navigator.clipboard.writeText) {"
        , "      navigator.clipboard.writeText(out.value).then(function () { out.select(); copied(); }, fallback);"
        , "    } else fallback();"
        , "  });"
        , "  refresh();"
        , "})();"
        ]
