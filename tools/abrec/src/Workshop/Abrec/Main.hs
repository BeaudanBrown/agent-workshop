{-# LANGUAGE OverloadedStrings #-}

-- | Command-line entry point.
module Workshop.Abrec.Main (main) where

import Data.Text qualified as T
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)
import System.Directory (doesDirectoryExist)
import System.Environment (getArgs, lookupEnv, setEnv)
import System.Exit (die)
import System.FilePath (isAbsolute, (</>))
import Workshop.Abrec.Admin.App (Env (..), adminApp)
import Workshop.Abrec.Config (Config (..), loadConfig)
import Workshop.Abrec.Library (openStore)
import Workshop.Abrec.Media (loadTools)
import Workshop.Abrec.Publisher (requestPublish, startPublisher)
import Workshop.Abrec.Site (renderSite)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["serve", path] -> serve path
        ["render", path] -> do
            config <- loadConfig path
            tools <- loadTools
            store <- openStore (configLibraryDir config)
            result <- renderSite tools store (configPublicUrl config)
            either (die . ("abrec: " ++) . T.unpack) (const (putStrLn "abrec: published")) result
        ["--help"] ->
            putStrLn . unlines $
                [ usage
                , ""
                , "serve CONFIG   run the admin site and publish in the background"
                , "render CONFIG  rebuild the public site from the library once"
                ]
        _ -> die usage

usage :: String
usage = "Usage: abrec serve CONFIG | abrec render CONFIG"

serve :: FilePath -> IO ()
serve path = do
    config <- loadConfig path
    tools <- loadTools
    static <- lookupEnv "ABREC_STATIC"
    staticDir <- case static of
        Just dir | isAbsolute dir -> do
            present <- doesDirectoryExist dir
            if present then pure dir else die ("abrec: ABREC_STATIC does not exist: " ++ dir)
        _ -> die "abrec: configure ABREC_STATIC with an absolute path to the static files."
    store <- openStore (configLibraryDir config)
    -- Uploads are buffered here so staging them is a rename, not a copy.
    setEnv "TMPDIR" (configLibraryDir config </> "tmp")
    publisher <- startPublisher (renderSite tools store (configPublicUrl config))
    requestPublish publisher
    app <- adminApp (Env config tools store publisher staticDir)
    putStrLn ("abrec: admin site on 127.0.0.1:" ++ show (configPort config))
    runSettings (setHost "127.0.0.1" (setPort (configPort config) defaultSettings)) app
