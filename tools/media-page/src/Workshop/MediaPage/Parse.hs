{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Parse the media-page source format.

The grammar is line-based and strict: every construct outside it is reported
with its line number rather than rendered approximately. New features are
added as directive names ('containerDirectives'), never as new syntax.
-}
module Workshop.MediaPage.Parse (
    ParseError (..),
    parseDocument,
    containerDirectives,
) where

import Data.Char (isAlphaNum, isDigit)
import Data.List (sortOn)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Workshop.MediaPage.Document

data ParseError = ParseError
    { errorLine :: Int
    , errorMessage :: Text
    }
    deriving (Eq, Show)

-- | Container directives (`::: name`) the current version understands.
containerDirectives :: [Text]
containerDirectives = ["tabs"]

parseDocument :: Text -> Either [ParseError] (Document MediaRef)
parseDocument input
    | null errors = Right (Document (fromMaybe "" (headerTitle header)) (headerMeta header) (headerDate header) sections)
    | otherwise = Left (sortOn errorLine errors)
  where
    numbered = zip [1 ..] (map (T.dropWhileEnd (== '\r')) (T.lines input))
    lastLine = max 1 (length numbered)
    (headerErrors, header, body) = parseHeader lastLine numbered
    (bodyErrors, sections) = parseBody lastLine body
    errors = headerErrors ++ bodyErrors

-- Header ---------------------------------------------------------------

data Header = Header
    { headerTitle :: Maybe Text
    , headerMeta :: Maybe Text
    , headerDate :: Maybe Text
    }

parseHeader :: Int -> [(Int, Text)] -> ([ParseError], Header, [(Int, Text)])
parseHeader lastLine numbered = case numbered of
    (_, first) : rest | T.strip first == "---" -> go emptyHeader [] rest
    _ -> ([ParseError 1 "start the file with a '---' header block containing 'title: ...'"], emptyHeader, numbered)
  where
    emptyHeader = Header Nothing Nothing Nothing
    go header errs [] = (reverse (ParseError lastLine "close the header with a '---' line" : errs), header, [])
    go header errs ((n, line) : more)
        | T.strip line == "---" = (reverse errs ++ missingTitle header, header, more)
        | T.null (T.strip line) = go header errs more
        | otherwise = case T.breakOn ":" line of
            (_, "") -> go header (ParseError n "header lines are 'key: value'" : errs) more
            (rawKey, rawValue) ->
                let key = T.strip rawKey
                    value = T.strip (T.drop 1 rawValue)
                    set current update
                        | T.null value = go header (ParseError n ("'" <> key <> "' needs a value") : errs) more
                        | isJust (current header) = go header (ParseError n ("'" <> key <> "' is set twice") : errs) more
                        | otherwise = go (update value) errs more
                 in case key of
                        "title" -> set headerTitle (\v -> header{headerTitle = Just v})
                        "meta" -> set headerMeta (\v -> header{headerMeta = Just v})
                        "date" -> set headerDate (\v -> header{headerDate = Just v})
                        _ -> go header (ParseError n ("unknown header key '" <> key <> "'; use title, meta or date") : errs) more
    missingTitle header = [ParseError 1 "the header needs 'title: ...'" | isNothing (headerTitle header)]

-- Body -----------------------------------------------------------------

data Container = Container
    { containerLine :: Int
    , containerValid :: Bool
    , containerTabs :: [(Int, Tab MediaRef)]
    -- ^ completed tabs, newest first, with their label line
    , containerCurrent :: Maybe (Int, Text, [Block MediaRef])
    -- ^ open tab: label line, label, blocks newest first
    }

data State = State
    { stateSections :: [Section MediaRef]
    , stateCurrent :: Maybe (Text, [Block MediaRef])
    , stateParagraph :: Maybe (Int, [Text])
    , stateContainer :: Maybe Container
    , stateErrors :: [ParseError]
    , stateOrphanReported :: Bool
    }

parseBody :: Int -> [(Int, Text)] -> ([ParseError], [Section MediaRef])
parseBody lastLine numbered = (reverse (stateErrors final), reverse (stateSections final))
  where
    final = finish (foldl' step initial numbered)
    initial = State [] Nothing Nothing Nothing [] False
    finish state0 =
        let state1 = flushParagraph state0
            state2 = case stateContainer state1 of
                Just container ->
                    (failAt (containerLine container) "'::: tabs' is never closed; end it with ':::'" state1){stateContainer = Nothing}
                Nothing -> state1
            state3 = closeSection state2
         in if null (stateSections state3) && null (stateErrors state3)
                then failAt lastLine "add at least one '# Section' heading" state3
                else state3

step :: State -> (Int, Text) -> State
step state (n, raw)
    | T.null line = flushParagraph state
    | any (`T.isPrefixOf` line) ["```", "~~~"] =
        failAt n "fenced blocks are reserved for future directive payloads (such as CSV tables) and are not supported yet" state
    | line == ":::" = closeContainer n (flushParagraph state)
    | ":::" `T.isPrefixOf` line = openContainer n (T.strip (T.drop 3 line)) (flushParagraph state)
    | "::" `T.isPrefixOf` line =
        failAt n "unknown directive; this version has no '::' leaf directives, so write media as '![caption](file)'" state
    | "# " `T.isPrefixOf` line = newSection n (T.strip (T.drop 2 line)) (flushParagraph state)
    | "## " `T.isPrefixOf` line = newTab n (T.strip (T.drop 3 line)) (flushParagraph state)
    | "#" `T.isPrefixOf` line = failAt n "only '# Section' headings and '## label' tab labels inside '::: tabs' are supported" state
    | "![" `T.isPrefixOf` line = media n line (flushParagraph state)
    | isListOrQuote line = failAt n "lists, quotes and tables are not supported; write plain paragraphs" state
    | otherwise = paragraphLine n line state
  where
    line = T.strip raw

isListOrQuote :: Text -> Bool
isListOrQuote line =
    any (`T.isPrefixOf` line) ["- ", "* ", "+ ", "> ", "|"] || orderedItem
  where
    (digits, rest) = T.span isDigit line
    orderedItem = not (T.null digits) && any (`T.isPrefixOf` rest) [". ", ") "]

paragraphLine :: Int -> Text -> State -> State
paragraphLine n line state
    | "![" `T.isInfixOf` line = failAt n "put media on its own line as '![caption](file)'" state
    | "](" `T.isInfixOf` line = failAt n "links are not supported; the sidebar removes them, so write the address as plain text" state
    | any (`T.isInfixOf` line) ["**", "__"] = failAt n "bold and italic markup is not supported; write plain text" state
    | "`" `T.isInfixOf` line = failAt n "inline code is not supported yet; write plain text" state
    | otherwise = case stateParagraph state of
        Just (start, linesSoFar) -> state{stateParagraph = Just (start, line : linesSoFar)}
        Nothing -> state{stateParagraph = Just (n, [line])}

flushParagraph :: State -> State
flushParagraph state = case stateParagraph state of
    Nothing -> state
    Just (start, linesSoFar) ->
        addBlock start (Paragraph (T.unwords (reverse linesSoFar))) state{stateParagraph = Nothing}

media :: Int -> Text -> State -> State
media n line state = case parseMedia line of
    Left message -> failAt n message state
    Right (caption, path) -> addBlock n (Media (MediaRef caption (T.unpack path) n)) state

parseMedia :: Text -> Either Text (Text, Text)
parseMedia line
    | not (")" `T.isSuffixOf` line) || T.null rest = Left "write media as '![caption](file)' on its own line"
    | T.null caption = Left "media needs a caption: '![what this shows](file)'"
    | T.null path = Left "media needs a file path: '![caption](file)'"
    | otherwise = Right (caption, path)
  where
    (rawCaption, rest) = T.breakOn "](" (T.drop 2 line)
    caption = T.strip rawCaption
    path = T.strip (T.dropEnd 1 (T.drop 2 rest))

addBlock :: Int -> Block MediaRef -> State -> State
addBlock n block state = case stateContainer state of
    Just container -> case containerCurrent container of
        Just (labelLine, label, blocks) ->
            state{stateContainer = Just container{containerCurrent = Just (labelLine, label, block : blocks)}}
        Nothing -> failAt n "start each tab with a '## label' line before its content" state
    Nothing -> case stateCurrent state of
        Just (heading, blocks) -> state{stateCurrent = Just (heading, block : blocks)}
        Nothing
            | stateOrphanReported state -> state
            | otherwise -> (failAt n "content must follow a '# Section' heading" state){stateOrphanReported = True}

newSection :: Int -> Text -> State -> State
newSection n heading state0
    | T.null heading = failAt n "a '#' heading needs text" state1
    | otherwise = (closeSection state1){stateCurrent = Just (heading, [])}
  where
    state1 = case stateContainer state0 of
        Just _ -> (failAt n "close '::: tabs' with ':::' before the next '#' section" state0){stateContainer = Nothing}
        Nothing -> state0

closeSection :: State -> State
closeSection state = case stateCurrent state of
    Nothing -> state
    Just (heading, blocks) ->
        state{stateSections = Section heading (reverse blocks) : stateSections state, stateCurrent = Nothing}

openContainer :: Int -> Text -> State -> State
openContainer n spec state
    | Just _ <- stateContainer state =
        failAt n "containers cannot be nested; close the open one with ':::' first" state
    | T.null name = failAt n "name the container, for example '::: tabs'" (opened False)
    | name `notElem` containerDirectives =
        failAt n ("unknown container '" <> name <> "'; available: " <> T.intercalate ", " containerDirectives) (opened False)
    | not (T.null options) = failAt n ("'" <> name <> "' takes no options") (opened True)
    | otherwise = opened True
  where
    (name, rawOptions) = T.span (\c -> isAlphaNum c || c == '-') spec
    options = T.strip rawOptions
    opened valid
        | Nothing <- stateCurrent state
        , not (stateOrphanReported state) =
            (failAt n "content must follow a '# Section' heading" (start valid)){stateOrphanReported = True}
        | otherwise = start valid
    start valid = state{stateContainer = Just (Container n valid [] Nothing)}

newTab :: Int -> Text -> State -> State
newTab n label state = case stateContainer state of
    Nothing -> failAt n "'##' is only used for tab labels inside '::: tabs'" state
    Just container
        | T.null label -> failAt n "a '##' tab label needs text" state
        | otherwise ->
            state{stateContainer = Just (finishTab container){containerCurrent = Just (n, label, [])}}

finishTab :: Container -> Container
finishTab container = case containerCurrent container of
    Nothing -> container
    Just (labelLine, label, blocks) ->
        container{containerTabs = (labelLine, Tab label (reverse blocks)) : containerTabs container, containerCurrent = Nothing}

closeContainer :: Int -> State -> State
closeContainer n state = case stateContainer state of
    Nothing -> failAt n "':::' closes nothing; open a container first, for example '::: tabs'" state
    Just container ->
        let tabs = reverse (containerTabs (finishTab container))
            closed = state{stateContainer = Nothing}
            emptyTabs = [ParseError labelLine ("tab '" <> tabLabel tab <> "' has no content") | (labelLine, tab) <- tabs, null (tabBlocks tab)]
         in if
                | not (containerValid container) -> closed
                | null tabs -> failAt (containerLine container) "'::: tabs' needs at least one '## label'" closed
                | not (null emptyTabs) -> closed{stateErrors = reverse emptyTabs ++ stateErrors closed}
                | otherwise -> addBlock (containerLine container) (Tabs (map snd tabs)) closed

failAt :: Int -> Text -> State -> State
failAt n message state = state{stateErrors = ParseError n message : stateErrors state}
