-- | Command-line entry point.
module Workshop.Abrec.Main (main) where

import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--help"] -> putStrLn usage
        _ -> die usage

usage :: String
usage = "Usage: abrec --help"
