{-# LANGUAGE OverloadedStrings #-}

{- | Request checks for an admin site that has no login.

Reaching the site is its access control, so it must not be reachable through
another name (DNS rebinding) or writable from another site's pages.
-}
module Workshop.Abrec.Admin.Security (
    adminGuard,
) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import Data.Char (isDigit, toLower)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types (hContentType, status400, status403)
import Network.Wai (Middleware, requestHeaderHost, requestHeaders, requestMethod, responseLBS)

{- | Answer only to the admin domain, and refuse every request that could
change something unless the browser says it came from the admin site itself.
-}
adminGuard :: Text -> Middleware
adminGuard domain app request respond
    | fmap normaliseHost (requestHeaderHost request) /= Just expectedHost = refuse status400 "Unknown host."
    | requestMethod request `notElem` ["GET", "HEAD"] && not sameOrigin = refuse status403 "Cross-site request refused."
    | otherwise = app request respond
  where
    expectedHost = BC.map toLower (encodeUtf8 domain)
    headers = requestHeaders request
    sameOrigin = case lookup "Sec-Fetch-Site" headers of
        Just site -> site == "same-origin"
        Nothing -> lookup "Origin" headers == Just ("https://" <> expectedHost)
    refuse status message = respond (responseLBS status [(hContentType, "text/plain; charset=utf-8")] message)

-- | Lowercase, without a port.
normaliseHost :: ByteString -> ByteString
normaliseHost host =
    let lowered = BC.map toLower host
        (name, port) = BC.breakEnd (== ':') lowered
     in if not (BC.null name) && not (BC.null port) && BC.all isDigit port then BC.init name else lowered
