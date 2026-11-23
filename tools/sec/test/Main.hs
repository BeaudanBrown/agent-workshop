module Main (main) where

import Control.Monad (unless)
import Data.List (isInfixOf)
import System.Directory (
    Permissions (executable),
    canonicalizePath,
    createDirectory,
    createDirectoryIfMissing,
    createFileLink,
    findExecutable,
    getPermissions,
    setPermissions,
 )
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode (..), die)
import System.FilePath ((</>))
import System.IO (readFile')
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (cwd, env), proc, readCreateProcessWithExitCode)
import System.Timeout (timeout)

data Tools = Tools
    { sec :: FilePath
    , sh :: FilePath
    , git :: FilePath
    , script :: FilePath
    }

data Fixture = Fixture
    { root :: FilePath
    , manifest :: FilePath
    , work :: FilePath
    , originalPath :: FilePath
    , environment :: [(String, String)]
    }

data Input = Pipe String | Terminal String

data Result = Result
    { status :: ExitCode
    , output :: String
    , events :: [String]
    }

session :: String
session = "fake-bw-session-do-not-forward"

staleSession :: String
staleSession = "stale-bw-session-do-not-reuse"

customServer :: String
customServer = "https://vault.example.test"

main :: IO ()
main = do
    args <- getArgs
    (secPath, realSecretspec) <- case args of
        [path] -> (\resolved -> (resolved, Nothing)) <$> canonicalizePath path
        [path, real] -> (,) <$> canonicalizePath path <*> (Just <$> canonicalizePath real)
        _ -> die "usage: sec-test /path/to/compiled/sec [/path/to/real/secretspec]"
    testShell <- lookupEnv "SEC_TEST_SHELL" >>= maybe (requireProgram "sh") pure
    tools <- Tools secPath testShell <$> requireProgram "git" <*> requireProgram "script"
    withSystemTempDirectory "sec-test" $ \tmp -> do
        base <- canonicalizePath tmp
        let test name action = do
                putStrLn ("sec: " ++ name)
                fixture tools (base </> name) >>= action
            hosted state = [("FAKE_STATUS", statusJSON state "null")]
            custom state url =
                [ ("SEC_VAULTWARDEN_URL", customServer)
                , ("FAKE_STATUS", statusJSON state url)
                ]
            invoke f input vars = runSec tools f input vars ["probe"]
            completed f prefix = expectCompleted tools f prefix "bw://" [] ExitSuccess
        test "nested Git, custom provider, argv, and environment" $ \f -> do
            let tricky = ["a b", "", "--option=value", "*", "a'b\"c", "line 1\nline 2", "$(exit 99); & |"]
                vars = custom "unlocked" (show customServer) ++ [("BW_SESSION", staleSession)]
            result <- runSec tools f (Terminal "") vars ("probe" : tricky)
            expectCompleted tools f ["bw status", "bw unlock --raw"] ("bw://?server=" ++ customServer) tricky ExitSuccess result
        test "no manifest within Git boundary" $ \f -> do
            let inner = work f </> "inner repository"
            createDirectory inner
            initRepo tools f inner
            result <- invoke (f{work = inner}) (Pipe "") (hosted "unlocked")
            expectStop result [] "secretspec.toml"
        test "locked unlocks on a TTY" $ \f -> do
            result <- invoke f (Terminal "") (hosted "locked")
            completed f ["bw status", "bw unlock --raw"] result
        test "locked refuses noninteractive unlock" $ \f -> do
            result <- invoke f (Pipe "yes\n") (hosted "locked")
            expectStop result ["bw status"] "unlock"
        test "signed out rejects piped confirmation" $ \f -> do
            result <- invoke f (Pipe "y\n") (hosted "unauthenticated")
            expectStop result ["bw status"] "login"
            assert (not ("[y/N]" `isInfixOf` output result)) "noninteractive run prompted for login"
        test "signed out declines on Enter" $ \f -> do
            result <- invoke f (Terminal "\n") (hosted "unauthenticated")
            expectStop result ["bw status"] "[y/N]"
        test "signed out accepts login on a TTY" $ \f -> do
            result <- invoke f (Terminal "y\n") (hosted "unauthenticated")
            expectOutput result "[y/N]"
            completed f ["bw status", "bw login --raw", "bw status"] result
        test "login without a session falls back to unlock" $ \f -> do
            result <- invoke f (Terminal "y\n") (("FAKE_LOGIN_EMPTY", "1") : hosted "unauthenticated")
            completed f ["bw status", "bw login --raw", "bw status", "bw unlock --raw"] result
        test "wrong server is rejected before authentication" $ \f -> do
            result <- invoke f (Terminal "y\n") (custom "unauthenticated" (show "https://other.example.test"))
            expectStop result ["bw status"] "bw config server"
            assert (not ("[y/N]" `isInfixOf` output result)) "wrong server prompted for login"
        test "null server is not accepted for a custom provider" $ \f -> do
            result <- invoke f (Terminal "y\n") (custom "unauthenticated" "null")
            expectStop result ["bw status"] "bw config server"
        test "missing secret values block the command" $ \f -> do
            result <- invoke f (Terminal "") [("FAKE_MISSING", "1")]
            equal "exit status" (ExitFailure 23) (status result)
            equal "calls" ["bw status", "bw unlock --raw", "secretspec", "bw sync"] (events result)
            expectOutput result "missing required value: FAKE_REQUIRED"
        test "failed unlock blocks the command" $ \f -> do
            result <- invoke f (Terminal "") [("FAKE_BAD_UNLOCK", "1")]
            expectStop result ["bw status", "bw unlock --raw"] "bw unlock failed"
        test "command nonzero exit status is preserved" $ \f -> do
            result <- invoke f (Terminal "") [("FAKE_EXIT", "42"), ("SEC_VAULTWARDEN_URL", "")]
            expectCompleted tools f ["bw status", "bw unlock --raw"] "bw://" [] (ExitFailure 42) result
        case realSecretspec of
            Nothing -> pure ()
            Just executablePath -> test "real SecretSpec resolves a fake Vaultwarden item" $ \f -> do
                writeFile (manifest f) $
                    unlines
                        [ "[project]"
                        , "name = \"fixture\""
                        , "revision = \"1.0\""
                        , "require_reason = false"
                        , "[profiles.default]"
                        , "FAKE_INJECTED = { description = \"Synthetic test value\", required = true }"
                        ]
                let vars = ("SEC_SECRETSPEC", executablePath) : custom "locked" (show customServer)
                result <- invoke f (Terminal "") vars
                equal ("real SecretSpec exit status; output: " ++ output result ++ "; calls: " ++ show (events result)) ExitSuccess (status result)
                probeArgs <- readFile' (root f </> "probe-args")
                equal "real provider value, original cwd and PATH, no session" (nulFields [work f, originalPath f, "", "secret from fake provider"]) probeArgs

requireProgram :: String -> IO FilePath
requireProgram name = do
    path <- findExecutable name
    maybe (die ("sec tests require " ++ name ++ " on PATH")) canonicalizePath path

statusJSON :: String -> String -> String
statusJSON state server = "{\"status\":" ++ show state ++ ",\"serverUrl\":" ++ server ++ "}"

fixture :: Tools -> FilePath -> IO Fixture
fixture tools dir = do
    let fake = dir </> "configured executables"
        bin = dir </> "path bin"
        home = dir </> "home"
        outer = dir </> "outer repo"
        repo = outer </> "nested repo"
        nested = repo </> "with spaces" </> "child's cwd"
        manifestPath = repo </> "secretspec.toml"
        vars =
            [ ("HOME", home)
            , ("PATH", bin)
            , ("SHELL", sh tools)
            , ("GIT_CONFIG_NOSYSTEM", "1")
            , ("SEC_BW", fake </> "bw")
            , ("SEC_SECRETSPEC", fake </> "secretspec")
            , ("FAKE_DIR", dir)
            , ("FAKE_SESSION", session)
            , ("FAKE_STATUS", statusJSON "unlocked" "null")
            , ("FAKE_LOGGED_IN_STATUS", statusJSON "locked" "null")
            ]
        f = Fixture dir manifestPath nested bin vars
        make = writeExecutable (sh tools)
    mapM_ (createDirectoryIfMissing True) [fake, bin, home, nested]
    initRepo tools f outer
    initRepo tools f repo
    writeFile (outer </> "secretspec.toml") "# decoy in outer Git repository\n"
    writeFile manifestPath "# fake secretspec reads no manifest contents\n"
    createFileLink (git tools) (bin </> "git")
    make (fake </> "bw") fakeBw
    make (fake </> "secretspec") fakeSecretspec
    make (bin </> "probe") fakeProbe
    mapM_ (\name -> make (bin </> name) ["echo 'unexpected PATH fallback: " ++ name ++ "' >&2", "exit 98"]) ["bw", "secretspec"]
    pure f

initRepo :: Tools -> Fixture -> FilePath -> IO ()
initRepo tools f path = do
    (code, out, err) <- readCreateProcessWithExitCode ((proc (git tools) ["init", "--quiet", path]){env = Just (environment f)}) ""
    unless (code == ExitSuccess) (die ("git init failed: " ++ out ++ err))

writeExecutable :: FilePath -> FilePath -> [String] -> IO ()
writeExecutable shell path body = do
    writeFile path (unlines (("#!" ++ shell) : "set -eu" : body))
    perms <- getPermissions path
    setPermissions path (perms{executable = True})

fakeBw :: [String]
fakeBw =
    [ "printf 'bw %s\n' \"$*\" >> \"$FAKE_DIR/events\""
    , "case \"$*\" in"
    , "  status)"
    , "    test -z \"${BW_SESSION+x}\""
    , "    if [ -f \"$FAKE_DIR/logged-in\" ]; then FAKE_STATUS=$FAKE_LOGGED_IN_STATUS; fi"
    , "    printf '%s\n' \"$FAKE_STATUS\" ;;"
    , "  'login --raw')"
    , "    test -t 0; test -t 2"
    , "    test -z \"${BW_SESSION+x}\""
    , "    : > \"$FAKE_DIR/logged-in\""
    , "    if [ \"${FAKE_LOGIN_EMPTY:-0}\" = 0 ]; then printf '%s\n' \"$FAKE_SESSION\"; fi ;;"
    , "  'unlock --raw')"
    , "    test -t 0; test -t 2"
    , "    test -z \"${BW_SESSION+x}\""
    , "    if [ \"${FAKE_BAD_UNLOCK:-0}\" = 1 ]; then exit 1; fi"
    , "    printf '%s\n' \"$FAKE_SESSION\" ;;"
    , "  sync) test \"${BW_SESSION-}\" = \"$FAKE_SESSION\" ;;"
    , "  '--nointeraction status')"
    , "    test \"${BW_SESSION-}\" = \"$FAKE_SESSION\""
    , "    printf '%s\n' '{\"status\":\"unlocked\",\"serverUrl\":\"https://vault.example.test\"}' ;;"
    , "  '--nointeraction list items')"
    , "    test \"${BW_SESSION-}\" = \"$FAKE_SESSION\""
    , "    printf '%s\n' '[{\"id\":\"123\",\"type\":2,\"name\":\"secretspec/fixture/default/FAKE_INJECTED\",\"notes\":\"secret from fake provider\"}]' ;;"
    , "  *) echo 'unexpected bw arguments' >&2; exit 97 ;;"
    , "esac"
    ]

fakeSecretspec :: [String]
fakeSecretspec =
    [ "printf 'secretspec\n' >> \"$FAKE_DIR/events\""
    , "printf '%s\\000' \"$@\" > \"$FAKE_DIR/provider-args\""
    , "test \"${BW_SESSION-}\" = \"$FAKE_SESSION\""
    , "bw sync"
    , "if [ \"${FAKE_MISSING:-0}\" = 1 ]; then echo 'missing required value: FAKE_REQUIRED' >&2; exit 23; fi"
    , "while [ \"$#\" -gt 0 ] && [ \"$1\" != -- ]; do shift; done"
    , "shift"
    , "export FAKE_INJECTED='secret from fake provider'"
    , "exec \"$@\""
    ]

fakeProbe :: [String]
fakeProbe =
    [ "printf 'probe\n' >> \"$FAKE_DIR/events\""
    , "printf '%s\\000' \"$(pwd -P)\" \"$PATH\" \"${BW_SESSION+x}\" \"${FAKE_INJECTED-}\" \"$@\" > \"$FAKE_DIR/probe-args\""
    , "echo 'probe stdout'"
    , "echo 'probe stderr' >&2"
    , "exit \"${FAKE_EXIT:-0}\""
    ]

runSec :: Tools -> Fixture -> Input -> [(String, String)] -> [String] -> IO Result
runSec tools f input vars args = do
    let (command, stdin) = case input of
            Pipe text -> (proc (sec tools) args, text)
            Terminal text ->
                (proc (script tools) ["-q", "-e", "-c", unwords ("exec" : map shellQuote (sec tools : args)), "/dev/null"], text)
    writeFile (root f </> "events") ""
    terminated <- timeout (15 * 1000000) $ readCreateProcessWithExitCode (command{cwd = Just (work f), env = Just (vars ++ filter ((`notElem` map fst vars) . fst) (environment f))}) stdin
    case terminated of
        Nothing -> do
            calls <- readFile' (root f </> "events")
            die ("sec timed out; fake calls: " ++ show (lines calls))
        Just (code, out, err) -> do
            calls <- lines <$> readFile' (root f </> "events")
            let combined = out ++ err
            mapM_ (\token -> assert (not (token `isInfixOf` combined)) "session token appeared in process output") [session, staleSession]
            pure (Result code combined calls)

shellQuote :: String -> String
shellQuote word = "'" ++ concatMap quote word ++ "'"
  where
    quote '\'' = "'\\''"
    quote c = [c]

expectStop :: Result -> [String] -> String -> IO ()
expectStop result calls diagnostic = do
    assert (status result /= ExitSuccess) ("unexpected success: " ++ output result)
    equal "calls" calls (events result)
    expectOutput result diagnostic

expectCompleted :: Tools -> Fixture -> [String] -> String -> [String] -> ExitCode -> Result -> IO ()
expectCompleted tools f calls provider args code result = do
    equal ("exit status; output: " ++ output result ++ "; calls: " ++ show (events result)) code (status result)
    equal "calls" (calls ++ ["secretspec", "bw sync", "probe"]) (events result)
    let expectedProvider = ["--file", manifest f, "run", "--profile", "default", "--provider", provider, "--", sec tools, "--internal-exec", originalPath f, "probe"]
    providerArgs <- readFile' (root f </> "provider-args")
    equal "secretspec argv" (nulFields (expectedProvider ++ args)) providerArgs
    probeArgs <- readFile' (root f </> "probe-args")
    equal "command cwd, PATH, session absence, injected secret and argv" (nulFields ([work f, originalPath f, "", "secret from fake provider"] ++ args)) probeArgs
    mapM_ (expectOutput result) ["probe stdout", "probe stderr"]

nulFields :: [String] -> String
nulFields = concatMap (++ "\NUL")

expectOutput :: Result -> String -> IO ()
expectOutput result part =
    assert (part `isInfixOf` output result) ("expected output containing " ++ show part ++ "; got " ++ show (output result))

equal :: (Eq a, Show a) => String -> a -> a -> IO ()
equal label expected actual =
    assert (expected == actual) (label ++ ": expected " ++ show expected ++ ", got " ++ show actual)

assert :: Bool -> String -> IO ()
assert condition message = unless condition (die message)
