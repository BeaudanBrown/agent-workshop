{-# LANGUAGE OverloadedStrings #-}

module Workshop.Sec (main) where

import Control.Exception (IOException, evaluate, handle)
import Control.Monad (unless, when)
import Data.Aeson (FromJSON (parseJSON), eitherDecodeStrict', withObject, (.:), (.:?))
import Data.Char (isAlpha, isAlphaNum, isAscii, isSpace, toLower)
import Data.List (dropWhileEnd, intercalate, isPrefixOf, nub, stripPrefix)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import System.Directory (doesFileExist, doesPathExist, getCurrentDirectory)
import System.Environment (getArgs, getEnvironment, getExecutablePath, lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (ExitSuccess), die, exitWith)
import System.FilePath (takeDirectory, (</>))
import System.IO (hFlush, hGetContents, hIsTerminalDevice, hPutStr, hPutStrLn, isEOF, stderr, stdin)
import System.Posix.Process (executeFile)
import System.Process (CreateProcess (..), StdStream (..), proc, readCreateProcessWithExitCode, waitForProcess, withCreateProcess)
import Workshop.Cli.Env qualified as Env

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
        ["--help"] -> putStrLn $ unlines [usage, "Run a trusted command or set values in the nearest project's default SecretSpec profile."]
        ["set", "--help"] -> putStrLn setUsage
        "set" : keys -> setSecrets keys
        "--internal-exec" : originalPath : command : rest -> launch (Just originalPath) command rest
        "--internal-exec-no-path" : command : rest -> launch Nothing command rest
        "--" : command : rest -> run command rest
        command : rest | not ("-" `isPrefixOf` command) -> run command rest
        _ -> die usage
  where
    ioFailure :: IOException -> IO ()
    ioFailure _ = die "sec: I/O failure; check terminal, project files, and executable paths."

usage, setUsage :: String
usage = "Usage: sec [--] COMMAND [ARG ...]\n       sec set KEY [KEY ...]"
setUsage = "Usage: sec set KEY [KEY ...]\nUnlock once and prompt for each value in the project's default profile."

data Session = Session
    { executable :: FilePath
    , manifestPath :: FilePath
    , providerUri :: String
    , sessionEnv :: [(String, String)]
    , userPath :: Maybe String
    }

prepareSession :: IO Session
prepareSession = do
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
    let envForResolution = putEnv "BW_SESSION" token (putEnv "PATH" providerPath cleanEnv)
    pure (Session secretSpec manifest provider envForResolution originalPath)

secretSpecArgs :: Session -> String -> [String] -> [String]
secretSpecArgs session action args =
    ["--file", manifestPath session, action, "--profile", "default", "--provider", providerUri session, "--"] ++ args

run :: FilePath -> [String] -> IO ()
run command args = do
    session <- prepareSession
    self <- getExecutablePath
    let launcher = maybe ["--internal-exec-no-path"] (\path -> ["--internal-exec", path]) (userPath session)
        invocation = secretSpecArgs session "run" ([self] ++ launcher ++ (command : args))
    -- Execute SecretSpec directly so it receives signals and preserves the child's exit status.
    executeFile (executable session) False invocation (Just (sessionEnv session))

setSecrets :: [String] -> IO ()
setSecrets keys = do
    when (null keys) $ die setUsage
    unless (all validKey keys) $ die "sec set: keys must be environment variable names (ASCII letters, digits or underscores; cannot start with a digit)."
    unless (nub keys == keys) $ die "sec set: duplicate keys are not allowed."
    session <- prepareSession
    writeBatch session [] keys
  where
    validKey [] = False
    validKey (first : rest) = (first == '_' || isAscii first && isAlpha first) && all (\c -> c == '_' || isAscii c && isAlphaNum c) rest

writeBatch :: Session -> [String] -> [String] -> IO ()
writeBatch _ _ [] = pure ()
writeBatch session saved (key : rest) = do
    let child = (proc (executable session) (secretSpecArgs session "set" [key])){env = Just (sessionEnv session), delegate_ctlc = True}
        reportFailure = hPutStrLn stderr $ "sec set: stopped at " ++ key ++ ". Previously saved: " ++ if null saved then "none." else intercalate ", " (reverse saved) ++ "."
        childFailure :: IOException -> IO ExitCode
        childFailure _ = reportFailure >> die "sec set: could not run SecretSpec."
    result <- handle childFailure $ withCreateProcess child $ \_ _ _ process -> waitForProcess process
    if result == ExitSuccess
        then writeBatch session (key : saved) rest
        else reportFailure >> exitWith result

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
configuredExecutable = Env.configuredExecutable "sec"

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
    eof <- isEOF
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
