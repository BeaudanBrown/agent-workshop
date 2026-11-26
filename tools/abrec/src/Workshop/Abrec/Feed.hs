{-# LANGUAGE OverloadedStrings #-}

-- | Podcast RSS for Apple Podcasts and other apps, as plain text.
module Workshop.Abrec.Feed (
    Channel (..),
    Item (..),
    renderFeed,
    rfc822,
) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime)

data Channel = Channel
    { channelTitle :: Text
    , channelLink :: Text
    , channelDescription :: Text
    , channelAuthor :: Maybe Text
    , channelImage :: Text
    , channelItems :: [Item]
    }
    deriving (Eq, Show)

data Item = Item
    { itemTitle :: Text
    , itemGuid :: Text
    , itemUrl :: Text
    , itemBytes :: Integer
    , itemSeconds :: Int
    , itemPublished :: UTCTime
    , itemEpisode :: Int
    , itemSeason :: Maybe Int
    , itemImage :: Maybe Text
    }
    deriving (Eq, Show)

{- | A serial, unlisted show. Items keep the order given; apps order a serial
show by season and episode number.
-}
renderFeed :: Channel -> Text
renderFeed channel =
    T.unlines $
        [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
        , "<rss version=\"2.0\" xmlns:itunes=\"http://www.itunes.com/dtds/podcast-1.0.dtd\">"
        , "<channel>"
        , element "title" (channelTitle channel)
        , element "link" (channelLink channel)
        , element "description" (channelDescription channel)
        , element "language" "en"
        , element "itunes:type" "serial"
        , element "itunes:block" "Yes"
        , element "itunes:explicit" "false"
        , "<itunes:image href=\"" <> escape (channelImage channel) <> "\"/>"
        , "<image>" <> element "url" (channelImage channel) <> element "title" (channelTitle channel) <> element "link" (channelLink channel) <> "</image>"
        ]
            ++ maybe [] (\author -> [element "itunes:author" author]) (channelAuthor channel)
            ++ concatMap item (channelItems channel)
            ++ ["</channel>", "</rss>"]
  where
    item it =
        [ "<item>"
        , element "title" (itemTitle it)
        , element "itunes:title" (itemTitle it)
        , element "itunes:episodeType" "full"
        , element "itunes:episode" (tshow (itemEpisode it))
        ]
            ++ maybe [] (\season -> [element "itunes:season" (tshow season)]) (itemSeason it)
            ++ [ "<enclosure url=\"" <> escape (itemUrl it) <> "\" length=\"" <> T.pack (show (itemBytes it)) <> "\" type=\"audio/mpeg\"/>"
               , "<guid isPermaLink=\"false\">" <> escape (itemGuid it) <> "</guid>"
               , element "pubDate" (rfc822 (itemPublished it))
               , element "itunes:duration" (tshow (itemSeconds it))
               ]
            ++ maybe [] (\image -> ["<itunes:image href=\"" <> escape image <> "\"/>"]) (itemImage it)
            ++ ["</item>"]

element :: Text -> Text -> Text
element name value = "<" <> name <> ">" <> escape value <> "</" <> name <> ">"

escape :: Text -> Text
escape = T.concatMap $ \c -> case c of
    '&' -> "&amp;"
    '<' -> "&lt;"
    '>' -> "&gt;"
    '"' -> "&quot;"
    '\'' -> "&apos;"
    _ -> T.singleton c

rfc822 :: UTCTime -> Text
rfc822 = T.pack . formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S +0000"

tshow :: Int -> Text
tshow = T.pack . show
