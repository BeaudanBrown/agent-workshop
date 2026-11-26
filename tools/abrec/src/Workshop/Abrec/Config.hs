{-# LANGUAGE OverloadedStrings #-}

-- | The service configuration file written by the NixOS module.
module Workshop.Abrec.Config (
    Config (..),
    loadConfig,
) where

import Data.Aeson (FromJSON (..), eitherDecodeFileStrict', withObject, (.:))
import Data.Text (Text)
import Data.Text qualified as T
import System.Exit (die)

data Config = Config
    { configLibraryDir :: FilePath
    , configAdminDomain :: Text
    -- ^ the only Host the admin site answers to
    , configPublicUrl :: Text
    -- ^ base URL of the public site, without a trailing slash
    , configPort :: Int
    -- ^ admin site port on 127.0.0.1
    }
    deriving (Eq, Show)

instance FromJSON Config where
    parseJSON = withObject "abrec configuration" $ \obj ->
        Config
            <$> obj .: "libraryDir"
            <*> obj .: "adminDomain"
            <*> (T.dropWhileEnd (== '/') <$> obj .: "publicUrl")
            <*> obj .: "port"

loadConfig :: FilePath -> IO Config
loadConfig path = eitherDecodeFileStrict' path >>= either (\err -> die ("abrec: " ++ path ++ ": " ++ err)) pure
