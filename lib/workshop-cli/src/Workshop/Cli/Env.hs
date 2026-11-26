-- | Configuration a tool reads from its Nix wrapper.
module Workshop.Cli.Env (configuredExecutable) where

import System.Environment (lookupEnv)
import System.Exit (die)
import System.FilePath (isAbsolute, takeFileName)

{- | Read the absolute path of a program the Nix wrapper provides.

Tools resolve every external program through a variable set by their wrapper,
never through PATH, so a user's PATH cannot substitute another program. The
first argument is the tool name that prefixes the failure message.
-}
configuredExecutable :: String -> String -> FilePath -> IO FilePath
configuredExecutable tool variable basename = do
    value <- lookupEnv variable
    case value of
        Just path | isAbsolute path && takeFileName path == basename -> pure path
        _ -> die (tool ++ ": configure " ++ variable ++ " with an absolute path to " ++ basename ++ ".")
