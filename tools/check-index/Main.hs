module Main (main) where

import System.FilePath (addTrailingPathSeparator)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess)

main :: IO ()
main = withSystemTempDirectory "agent-workshop-index" $ \snapshot -> do
    callProcess "git" ["checkout-index", "--all", "--ignore-skip-worktree-bits", "--prefix=" ++ addTrailingPathSeparator snapshot]
    callProcess "nix" ["flake", "check", "--no-update-lock-file", "path:" ++ snapshot]
