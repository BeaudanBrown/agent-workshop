{-# LANGUAGE OverloadedStrings #-}

{- | Turn a source file into a written page.

Validation runs to completion before any encoding, and the page is written
atomically, so a failed run leaves no output behind.
-}
module Workshop.MediaPage.Build (
    Config (..),
    buildPage,
    defaultMaxBytes,
) where

import Control.Exception (IOException, try)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.Char (isAsciiLower, isDigit, toLower)
import Data.Foldable (toList)
import Data.List (intercalate, sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8')
import System.Directory (createDirectoryIfMissing, doesFileExist, renameFile)
import System.FilePath (isAbsolute, takeDirectory, takeExtension, takeFileName, (</>))
import System.Process (getCurrentPid)
import Text.Printf (printf)
import Workshop.MediaPage.Document
import Workshop.MediaPage.Media
import Workshop.MediaPage.Parse (ParseError (..), parseDocument)
import Workshop.MediaPage.Render (Placed (..), renderPage)

data Config = Config
    { configTools :: Tools
    , configOutDir :: FilePath
    -- ^ default directory for pages, and home of the encoding cache
    , configMaxBytes :: Int
    , configOutput :: Maybe FilePath
    -- ^ explicit output path, overriding the default directory
    , configFooter :: Text
    }

defaultMaxBytes :: Int
defaultMaxBytes = 64 * 1024 * 1024

-- | Build the page for a source file, returning its path or every error found.
buildPage :: Config -> FilePath -> IO (Either [String] FilePath)
buildPage config input = do
    readResult <- try (BS.readFile input) :: IO (Either IOException BS.ByteString)
    case readResult of
        Left err -> failed [input ++ ": cannot read the file: " ++ show err]
        Right bytes -> case decodeUtf8' bytes of
            Left _ -> failed [input ++ ": the file is not valid UTF-8"]
            Right source -> case parseDocument source of
                Left errs -> failed [located (errorLine e) (T.unpack (errorMessage e)) | e <- errs]
                Right doc -> do
                    problems <- concat <$> mapM checkSource (toList doc)
                    if null problems then encodeAll doc else failed problems
  where
    failed = pure . Left
    baseDir = takeDirectory input
    resolve ref = let path = refPath ref in if isAbsolute path then path else baseDir </> path
    located line message = input ++ ":" ++ show line ++ ": " ++ message

    checkSource ref = case sourceKind (refPath ref) of
        Nothing ->
            pure
                [ located (refLine ref) $
                    "unsupported media type '"
                        ++ takeExtension (refPath ref)
                        ++ "'; use "
                        ++ unwords (map fst supportedExtensions)
                ]
        Just _ -> do
            exists <- doesFileExist (resolve ref)
            pure [located (refLine ref) ("no such file: " ++ resolve ref) | not exists]

    encodeAll doc = do
        placed <- traverse encodeRef doc
        case sequenceA placed of
            Left err -> failed [err]
            Right ready -> writePage ready

    encodeRef ref = case sourceKind (refPath ref) of
        Nothing -> pure (Left (located (refLine ref) "unsupported media type"))
        Just kind -> do
            result <- encodeMedia (configTools config) (configOutDir config </> ".cache") (resolve ref) kind
            pure (either (Left . located (refLine ref)) (Right . Placed (refCaption ref)) result)

    writePage doc = do
        let page = BL.toStrict (toLazyByteString (renderPage (configFooter config) doc))
            size = BS.length page
        if size > configMaxBytes config
            then failed [input ++ ": " ++ oversize size doc]
            else do
                let digest = take 8 (BC.unpack (Base16.encode (SHA256.hash page)))
                    target = case configOutput config of
                        Just path -> path
                        Nothing -> configOutDir config </> slug (docTitle doc) ++ "-" ++ digest ++ ".html"
                createDirectoryIfMissing True (takeDirectory target)
                pid <- getCurrentPid
                -- Written by name so the page gets the usual creation mode; a
                -- private openTempFile mode would survive the rename.
                let partial = takeDirectory target </> (".partial-" ++ show pid ++ "-" ++ takeFileName target)
                BS.writeFile partial page
                renameFile partial target
                pure (Right target)

    oversize size doc =
        let largest = take 3 (sortOn (Down . snd) [(placedCaption p, BS.length (encodedBytes (placedMedia p))) | p <- toList doc])
            describe (caption, bytes) = "\"" ++ T.unpack caption ++ "\" (" ++ mebibytes bytes ++ " before embedding)"
         in "the page would be "
                ++ mebibytes size
                ++ ", over the "
                ++ mebibytes (configMaxBytes config)
                ++ " limit set by MEDIA_PAGE_MAX_BYTES; largest media: "
                ++ intercalate ", " (map describe largest)

mebibytes :: Int -> String
mebibytes bytes = printf "%.1f MiB" (fromIntegral bytes / (1024 * 1024) :: Double)

-- | A file-name stem from the title: lowercase ASCII words joined by hyphens.
slug :: Text -> String
slug title = case take 60 (hyphenate (map normal (T.unpack title))) of
    [] -> "page"
    stem -> reverse (dropWhile (== '-') (reverse stem))
  where
    normal c
        | isAsciiLower (toLower c) || isDigit c = toLower c
        | otherwise = '-'
    hyphenate = collapse . dropWhile (== '-')
    collapse ('-' : '-' : rest) = collapse ('-' : rest)
    collapse (c : rest) = c : collapse rest
    collapse [] = []
