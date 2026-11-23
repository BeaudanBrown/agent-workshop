{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (IOException, evaluate, handle)
import Control.Monad (unless, when)
import Data.Aeson (FromJSON (parseJSON), eitherDecodeStrict', withObject, (.:), (.:?))
import Data.Char (isAlphaNum, isAscii, isSpace, toLower)
import Data.List (dropWhileEnd, isPrefixOf, stripPrefix)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import System.Directory (doesFileExist, doesPathExist, getCurrentDirectory)
import System.Environment (getArgs, getEnvironment, getExecutablePath, lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (ExitSuccess), die)
import System.FilePath (isAbsolute, takeDirectory, takeFileName, (</>))
import System.IO (hFlush, hGetContents, hIsEOF, hIsTerminalDevice, hPutStr, stderr, stdin)
import System.Posix.Process (executeFile)
import System.Process (CreateProcess (..), StdStream (..), proc, readCreateProcessWithExitCode, waitForProcess, withCreateProcess)

data VaultState = SignedOut | Locked | Unlocked deriving (Eq)

data VaultStatus = VaultStatus (Maybe String) VaultState

instance FromJSON VaultStatus where
    parseJSON = withObject "Bitwarden status" $ \obj -> do
        server <- obj .:? "serverUrl"
        state <- obj .: "status"
        case state :: String of
            "unauthenticated" -> pure (VaultStatus server SignedOut)
            "locked" -> pure (VaultStatus server Locked)
            "unlocked" -> pure (VaultStatus server Unlocked)
            _ -> fail "unknown Bitwarden status"

main :: IO ()
main = handle ioFailure $ do
    args <- getArgs
    case args of
        ["--help"] -> putStrLn "Usage: sec COMMAND [ARG ...]\nRun a trusted command with the nearest project's default SecretSpec profile."
        "--internal-exec" : originalPath : command : rest -> launch (Just originalPath) command rest
        "--internal-exec-no-path" : command : rest -> launch Nothing command rest
        "--" : command : rest -> run command rest
        command : rest | not ("-" `isPrefixOf` command) -> run command rest
        _ -> die "Usage: sec COMMAND [ARG ...]"
  where
    ioFailure :: IOException -> IO ()
    ioFailure _ = die "sec: I/O failure; check terminal, project files, and executable paths."

run :: FilePath -> [String] -> IO ()
run command args = do
    manifest <- getCurrentDirectory >>= locateManifest
    bw <- configuredExecutable "SEC_BW" "bw"
    secretSpec <- configuredExecutable "SEC_SECRETSPEC" "secretspec"
    configuredServer <- lookupEnv "SEC_VAULTWARDEN_URL" >>= traverse validateServer . nonempty
    currentEnv <- getEnvironment
    let cleanEnv = filter ((/= "BW_SESSION") . fst) currentEnv
        originalPath = lookup "PATH" cleanEnv
        providerPath = takeDirectory bw ++ maybe "" (':' :) originalPath
        provider = maybe "bw://" ("bw://?server=" ++) configuredServer
    state <- readStatus bw cleanEnv configuredServer
    interactive <- hIsTerminalDevice stdin
    unless interactive $ die "sec: vault login and unlock require an interactive terminal."
    token <- case state of
        SignedOut -> do
            approveLogin
            fromLogin <- captureRaw bw cleanEnv "login"
            stateAfterLogin <- readStatus bw cleanEnv configuredServer
            when (stateAfterLogin == SignedOut) $ die "sec: Bitwarden is still signed out after login."
            if null fromLogin then unlock bw cleanEnv else validateToken fromLogin
        _ -> unlock bw cleanEnv
    self <- getExecutablePath
    let launcher = maybe ["--internal-exec-no-path"] (\path -> ["--internal-exec", path]) originalPath
        envForResolution = putEnv "BW_SESSION" token (putEnv "PATH" providerPath cleanEnv)
        invocation =
            ["--file", manifest, "run", "--profile", "default", "--provider", provider, "--"]
                ++ [self]
                ++ launcher
                ++ (command : args)
    -- Execute SecretSpec directly so it receives signals and preserves the child's exit status.
    executeFile secretSpec False invocation (Just envForResolution)

locateManifest :: FilePath -> IO FilePath
locateManifest dir = do
    let candidate = dir </> "secretspec.toml"
        parent = takeDirectory dir
    present <- doesFileExist candidate
    if present
        then pure candidate
        else do
            boundary <- doesPathExist (dir </> ".git")
            if boundary || parent == dir
                then die "sec: no secretspec.toml found in this project."
                else locateManifest parent

configuredExecutable :: String -> FilePath -> IO FilePath
configuredExecutable variable basename = do
    value <- lookupEnv variable
    case value of
        Just path | isAbsolute path && takeFileName path == basename -> pure path
        _ -> die ("sec: configure " ++ variable ++ " with an absolute path to " ++ basename ++ ".")

validateServer :: String -> IO String
validateServer url =
    let normalized = stripSlash url
        -- Only accept a bare HTTPS host and optional port, not query or user information.
        valid c = isAscii c && (isAlphaNum c || c `elem` (".-:" :: String))
     in case stripPrefix "https://" normalized of
            Just (first : rest) | isAscii first && isAlphaNum first && all valid rest -> pure normalized
            _ -> die "sec: vaultwardenUrl must be a bare HTTPS origin (host and optional port)."

readStatus :: FilePath -> [(String, String)] -> Maybe String -> IO VaultState
readStatus bw environment expected = do
    (code, output, _) <- readCreateProcessWithExitCode ((proc bw ["status"]){env = Just environment}) ""
    unless (code == ExitSuccess) $ die "sec: bw status failed; run bw status in your terminal for details."
    VaultStatus server state <-
        either (const (die "sec: Bitwarden returned an unrecognized status.")) pure $
            eitherDecodeStrict' (encodeUtf8 (Text.pack output))
    let actual = fmap stripSlash (server >>= nonempty . Just)
        hostedOrigins = ["https://vault.bitwarden.com", "https://vault.bitwarden.eu"]
        matches = case expected of
            Just url -> actual == Just url
            Nothing -> maybe True (`elem` hostedOrigins) actual
    unless matches $ die $ case expected of
        Just url -> "sec: Bitwarden is configured for a different server or no server. Configure it explicitly with bw config server " ++ url ++ "."
        Nothing -> "sec: Bitwarden is configured for a self-hosted server; set the sec vaultwardenUrl."
    pure state

approveLogin :: IO ()
approveLogin = do
    hPutStr stderr "Bitwarden is signed out. Run bw login now? [y/N] "
    hFlush stderr
    eof <- hIsEOF stdin
    answer <- if eof then pure "" else getLine
    unless (map toLower (trim answer) `elem` ["y", "yes"]) $ die "sec: Bitwarden login declined."

unlock :: FilePath -> [(String, String)] -> IO String
unlock bw environment = captureRaw bw environment "unlock" >>= validateToken

captureRaw :: FilePath -> [(String, String)] -> String -> IO String
captureRaw bw environment action =
    withCreateProcess ((proc bw [action, "--raw"]){env = Just environment, std_out = CreatePipe, delegate_ctlc = True}) $
        \_ output _ child -> do
            stream <- maybe (die "sec: could not capture the Bitwarden session.") pure output
            raw <- hGetContents stream
            _ <- evaluate (length raw)
            code <- waitForProcess child
            unless (code == ExitSuccess) $ die ("sec: bw " ++ action ++ " failed.")
            pure (trim raw)

validateToken :: String -> IO String
validateToken token
    | not (null token) && all (\c -> isAscii c && (isAlphaNum c || c `elem` ("+/=_-" :: String))) token = pure token
    | otherwise = die "sec: Bitwarden did not return a usable session."

launch :: Maybe String -> FilePath -> [String] -> IO ()
launch originalPath command args = do
    unsetEnv "BW_SESSION"
    maybe (unsetEnv "PATH") (setEnv "PATH") originalPath
    executeFile command True args Nothing

nonempty :: Maybe String -> Maybe String
nonempty (Just "") = Nothing
nonempty value = value

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

stripSlash :: String -> String
stripSlash = dropWhileEnd (== '/')

putEnv :: String -> String -> [(String, String)] -> [(String, String)]
putEnv key value entries = (key, value) : filter ((/= key) . fst) entries
