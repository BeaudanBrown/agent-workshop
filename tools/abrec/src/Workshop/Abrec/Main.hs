-- | Command-line entry point.
module Workshop.Abrec.Main (main) where

import Data.Text qualified as T
import System.Environment (getArgs)
import System.Exit (die)
import Workshop.Abrec.Config (Config (..), loadConfig)
import Workshop.Abrec.Library (openStore)
import Workshop.Abrec.Media (loadTools)
import Workshop.Abrec.Site (renderSite)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["render", path] -> do
            config <- loadConfig path
            tools <- loadTools
            store <- openStore (configLibraryDir config)
            result <- renderSite tools store (configPublicUrl config)
            either (die . ("abrec: " ++) . T.unpack) (const (putStrLn "abrec: published")) result
        ["--help"] -> putStrLn (unlines [usage, "", "render CONFIG  rebuild the public site from the library"])
        _ -> die usage

usage :: String
usage = "Usage: abrec render CONFIG"
