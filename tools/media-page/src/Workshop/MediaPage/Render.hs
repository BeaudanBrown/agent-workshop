{-# LANGUAGE OverloadedStrings #-}

{- | Render a document to one self-contained HTML page.

Every page is complete without scripts: media is embedded as data URIs and
tabs switch with radio inputs and CSS, so the page survives sanitising
previews that strip scripts, links and external resources.
-}
module Workshop.MediaPage.Render (Placed (..), renderPage) where

import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Builder (Builder, byteString, intDec, stringUtf8)
import Data.List (mapAccumL)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8Builder)
import Workshop.MediaPage.Document
import Workshop.MediaPage.Media (Embedded (..), Encoded (..))

-- | Encoded media with the caption written beside it in the source.
data Placed = Placed
    { placedCaption :: Text
    , placedMedia :: Encoded
    }

-- | Render a page; the footer text identifies the tool and format version.
renderPage :: Text -> Document Placed -> Builder
renderPage footer doc =
    mconcat
        [ "<!doctype html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n"
        , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"
        , "<title>"
        , escape (docTitle doc)
        , "</title>\n<style>\n"
        , stylesheet
        , foldMap tabRule [1 .. maximum (0 : concatMap (concatMap tabCounts . sectionBlocks) (docSections doc))]
        , "</style>\n</head>\n<body>\n<main>\n<header>\n<h1>"
        , escape (docTitle doc)
        , "</h1>\n"
        , metaLine
        , "</header>\n"
        , mconcat renderedSections
        , "</main>\n<footer>"
        , escape footer
        , "</footer>\n</body>\n</html>\n"
        ]
  where
    metaLine = case filter (not . T.null) (maybe [] pure (docMeta doc) ++ maybe [] pure (docDate doc)) of
        [] -> mempty
        parts -> "<p class=\"meta\">" <> escape (T.intercalate " · " parts) <> "</p>\n"
    (_, renderedSections) = mapAccumL renderSection (1 :: Int) (docSections doc)

renderSection :: Int -> Section Placed -> (Int, Builder)
renderSection next section =
    let (after, blocks) = mapAccumL renderBlock next (sectionBlocks section)
     in (after, "<section>\n<h2>" <> escape (sectionHeading section) <> "</h2>\n" <> mconcat blocks <> "</section>\n")

-- | Render a block; the counter numbers tab groups so their inputs have unique names.
renderBlock :: Int -> Block Placed -> (Int, Builder)
renderBlock next block = case block of
    Paragraph text -> (next, "<p>" <> escape text <> "</p>\n")
    Media placed -> (next, renderMedia placed)
    Tabs tabs ->
        let group = "t" <> intDec next
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
            (after, panels) = mapAccumL renderPanel (next + 1) tabs
         in (after, "<div class=\"tabs\">\n" <> mconcat controls <> mconcat panels <> "</div>\n")
  where
    renderPanel counter tab =
        let (after, blocks) = mapAccumL renderBlock counter (tabBlocks tab)
         in (after, "<div class=\"panel\">\n" <> mconcat blocks <> "</div>\n")

renderMedia :: Placed -> Builder
renderMedia (Placed captionText encoded) = case encodedAs encoded of
    EmbeddedImage ->
        "<figure>\n<img src=\"" <> source <> "\" alt=\"" <> caption <> "\"" <> dimensions <> ">\n" <> figcaption
    EmbeddedVideo loops ->
        "<figure>\n<video controls muted playsinline preload=\"metadata\""
            <> dimensions
            <> (if loops then " loop autoplay" else "")
            <> ">\n<source type=\""
            <> stringUtf8 (encodedMime encoded)
            <> "\" src=\""
            <> source
            <> "\">\n</video>\n"
            <> figcaption
  where
    source = "data:" <> stringUtf8 (encodedMime encoded) <> ";base64," <> byteString (Base64.encode (encodedBytes encoded))
    dimensions = " width=\"" <> intDec (encodedWidth encoded) <> "\" height=\"" <> intDec (encodedHeight encoded) <> "\""
    caption = escape captionText
    figcaption = "<figcaption>" <> caption <> "</figcaption>\n</figure>\n"

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
        , "footer{max-width:960px;margin:0 auto;padding:0 20px 28px;color:var(--muted);font-size:.8rem}\n"
        ]
