{-# LANGUAGE OverloadedStrings #-}

{- | Parse the agent-page source format.

The grammar is line-based and strict: every construct outside it is reported
with its line number rather than rendered approximately. New features are
added as directive names ('containerDirectives'), never as new syntax.
-}
module Workshop.AgentPage.Parse (
    ParseError (..),
    parseDocument,
    containerDirectives,
) where

import Data.Char (isAlphaNum, isDigit)
import Data.List (sortOn)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Workshop.AgentPage.Document

data ParseError = ParseError
    { errorLine :: Int
    , errorMessage :: Text
    }
    deriving (Eq, Show)

-- | Container directives (`::: name`) the current version understands.
containerDirectives :: [Text]
containerDirectives = "tabs" : map fst questionKinds

questionKinds :: [(Text, QuestionKind)]
questionKinds = [("choice", SingleChoice), ("multi", MultipleChoice), ("text", FreeText)]

parseDocument :: Text -> Either [ParseError] (Document MediaRef)
parseDocument input
    | null errors =
        Right
            ( Document
                (fromMaybe "" (headerTitle header))
                (headerMeta header)
                (headerDate header)
                (headerReply header == Just True)
                sections
            )
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
    , headerReply :: Maybe Bool
    }

parseHeader :: Int -> [(Int, Text)] -> ([ParseError], Header, [(Int, Text)])
parseHeader lastLine numbered = case numbered of
    (_, first) : rest | T.strip first == "---" -> go emptyHeader [] rest
    _ -> ([ParseError 1 "start the file with a '---' header block containing 'title: ...'"], emptyHeader, numbered)
  where
    emptyHeader = Header Nothing Nothing Nothing Nothing
    go header errs [] = (reverse (ParseError lastLine "close the header with a '---' line" : errs), header, [])
    go header errs ((n, line) : more)
        | T.strip line == "---" = (reverse errs ++ missingTitle header, header, more)
        | T.null (T.strip line) = go header errs more
        | otherwise = case T.breakOn ":" line of
            (_, "") -> go header (ParseError n "header lines are 'key: value'" : errs) more
            (rawKey, rawValue) ->
                let key = T.strip rawKey
                    value = T.strip (T.drop 1 rawValue)
                    twice = go header (ParseError n ("'" <> key <> "' is set twice") : errs) more
                    set current update
                        | T.null value = go header (ParseError n ("'" <> key <> "' needs a value") : errs) more
                        | isJust (current header) = twice
                        | otherwise = go (update value) errs more
                 in case key of
                        "title" -> set headerTitle (\v -> header{headerTitle = Just v})
                        "meta" -> set headerMeta (\v -> header{headerMeta = Just v})
                        "date" -> set headerDate (\v -> header{headerDate = Just v})
                        "reply"
                            | isJust (headerReply header) -> twice
                            | value == "on" -> go header{headerReply = Just True} errs more
                            | value == "off" -> go header{headerReply = Just False} errs more
                            | otherwise -> go header (ParseError n "'reply' is 'on' or 'off'" : errs) more
                        _ -> go header (ParseError n ("unknown header key '" <> key <> "'; use title, meta, date or reply") : errs) more
    missingTitle header = [ParseError 1 "the header needs 'title: ...'" | isNothing (headerTitle header)]

-- Body -----------------------------------------------------------------

-- | An open '::: tabs' group.
data Container = Container
    { containerLine :: Int
    , containerTabs :: [(Int, Tab MediaRef)]
    -- ^ completed tabs, newest first, with their label line
    , containerCurrent :: Maybe (Int, Text, [Block MediaRef])
    -- ^ open tab: label line, label, blocks newest first
    }

-- | An open question, or an unknown container whose content is skipped.
data Draft = Draft
    { draftKind :: Maybe QuestionKind
    -- ^ Nothing for an unknown container: its lines are ignored until ':::'
    , draftName :: Text
    , draftLine :: Int
    , draftText :: Maybe (Int, Text)
    , draftContext :: [Block MediaRef]
    -- ^ newest first
    , draftChoices :: [(Int, Choice)]
    -- ^ newest first
    }

data State = State
    { stateSections :: [Section MediaRef]
    -- ^ completed sections, newest first
    , stateCurrent :: Maybe (Maybe Text, Int, [Block MediaRef])
    -- ^ open section: heading, line, blocks newest first
    , stateParagraph :: Maybe (Int, [Text])
    , stateTabs :: Maybe Container
    , stateDraft :: Maybe Draft
    , stateErrors :: [ParseError]
    }

parseBody :: Int -> [(Int, Text)] -> ([ParseError], [Section MediaRef])
parseBody lastLine numbered = (reverse (stateErrors final), reverse (stateSections final))
  where
    final = finish (foldl' step initial numbered)
    initial = State [] Nothing Nothing Nothing Nothing []
    finish state0 =
        let state1 = flushParagraph state0
            state2 = case stateDraft state1 of
                Just draft ->
                    (failAt (draftLine draft) (opener draft <> " is never closed; end it with ':::'") state1){stateDraft = Nothing}
                Nothing -> state1
            state3 = case stateTabs state2 of
                Just container ->
                    (failAt (containerLine container) "'::: tabs' is never closed; end it with ':::'" state2){stateTabs = Nothing}
                Nothing -> state2
            state4 = closeSection state3
         in if null (stateSections state4) && null (stateErrors state4)
                then failAt lastLine "add some content after the header" state4
                else state4

step :: State -> (Int, Text) -> State
step state (n, raw)
    | skipping = if line == ":::" then closeContainer n state else state
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
    | inQuestion && (line == "-" || "- " `T.isPrefixOf` line) = choiceLine n (T.strip (T.drop 1 line)) (flushParagraph state)
    | isListOrQuote line =
        failAt n (if inQuestion then "write each option as '- label'; other lists, quotes and tables are not supported" else "lists, quotes and tables are not supported; write plain paragraphs") state
    | otherwise = paragraphLine n line state
  where
    line = T.strip raw
    inQuestion = isJust (stateDraft state)
    skipping = maybe False (isNothing . draftKind) (stateDraft state)

isListOrQuote :: Text -> Bool
isListOrQuote line =
    any (`T.isPrefixOf` line) ["- ", "* ", "+ ", "> ", "|"] || orderedItem
  where
    (digits, rest) = T.span isDigit line
    orderedItem = not (T.null digits) && any (`T.isPrefixOf` rest) [". ", ") "]

-- | Why a line of text cannot be shown as written, if it cannot.
inlineProblem :: Text -> Maybe Text
inlineProblem line
    | "![" `T.isInfixOf` line = Just "put media on its own line as '![caption](file)'"
    | "](" `T.isInfixOf` line = Just "links are not supported; the sidebar removes them, so write the address as plain text"
    | any (`T.isInfixOf` line) ["**", "__"] = Just "bold and italic markup is not supported; write plain text"
    | "`" `T.isInfixOf` line = Just "inline code is not supported yet; write plain text"
    | otherwise = Nothing

paragraphLine :: Int -> Text -> State -> State
paragraphLine n line state
    | Just problem <- inlineProblem line = failAt n problem state
    | otherwise = case stateParagraph state of
        Just (start, linesSoFar) -> state{stateParagraph = Just (start, line : linesSoFar)}
        Nothing -> state{stateParagraph = Just (n, [line])}

{- | End the open paragraph. Inside a question, its first paragraph is the
question text and later ones are context.
-}
flushParagraph :: State -> State
flushParagraph state = case stateParagraph state of
    Nothing -> state
    Just (start, linesSoFar) ->
        let text = T.unwords (reverse linesSoFar)
            cleared = state{stateParagraph = Nothing}
         in case stateDraft cleared of
                Just draft
                    | isNothing (draftText draft) -> cleared{stateDraft = Just draft{draftText = Just (start, text)}}
                _ -> addContent start (Paragraph text) cleared

media :: Int -> Text -> State -> State
media n line state = case parseMedia line of
    Left message -> failAt n message state
    Right (caption, path) -> addContent n (Media (MediaRef caption (T.unpack path) n)) state

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

-- | Add a paragraph or media to the open question, or else to the page.
addContent :: Int -> Block MediaRef -> State -> State
addContent n block state = case stateDraft state of
    Nothing -> addBlock n block state
    Just draft
        | isNothing (draftText draft) -> failAt n "start the question with its text; put media after it" state
        | not (null (draftChoices draft)) -> failAt n "options come last; move this above the first '- option'" state
        | otherwise -> state{stateDraft = Just draft{draftContext = block : draftContext draft}}

choiceLine :: Int -> Text -> State -> State
choiceLine n raw state = case stateDraft state of
    Nothing -> state
    Just draft
        | isNothing (draftText draft) -> failAt n "start the question with its text before its options" state
        | draftKind draft == Just FreeText -> failAt n "'::: text' questions take no options; use '::: choice' or '::: multi'" state
        | T.null label -> failAt n "an option needs text: '- label'" state
        | Just problem <- inlineProblem raw -> failAt n problem state
        | label `elem` map (choiceLabel . snd) (draftChoices draft) -> failAt n ("option '" <> label <> "' is listed twice") state
        | otherwise -> state{stateDraft = Just draft{draftChoices = (n, Choice label recommended) : draftChoices draft}}
  where
    marker = "(recommended)"
    recommended = marker `T.isSuffixOf` T.toLower raw
    label = T.strip (if recommended then T.dropEnd (T.length marker) raw else raw)

addBlock :: Int -> Block MediaRef -> State -> State
addBlock n block state = case stateTabs state of
    Just container -> case containerCurrent container of
        Just (labelLine, label, blocks) ->
            state{stateTabs = Just container{containerCurrent = Just (labelLine, label, block : blocks)}}
        Nothing -> failAt n "start each tab with a '## label' line before its content" state
    Nothing -> case stateCurrent state of
        Just (heading, line, blocks) -> state{stateCurrent = Just (heading, line, block : blocks)}
        Nothing -> state{stateCurrent = Just (Nothing, n, [block])}

newSection :: Int -> Text -> State -> State
newSection n heading state0
    | T.null heading = failAt n "a '#' heading needs text" state1
    | otherwise = (closeSection state1){stateCurrent = Just (Just heading, n, [])}
  where
    state1 = closeTabs (closeQuestion state0)
    closeQuestion s = case stateDraft s of
        Just draft -> (failAt n ("close " <> opener draft <> " with ':::' before the next '#' section") s){stateDraft = Nothing}
        Nothing -> s
    closeTabs s = case stateTabs s of
        Just _ -> (failAt n "close '::: tabs' with ':::' before the next '#' section" s){stateTabs = Nothing}
        Nothing -> s

closeSection :: State -> State
closeSection state = case stateCurrent state of
    Nothing -> state
    Just (heading, line, blocks) ->
        state{stateSections = Section heading line (reverse blocks) : stateSections state, stateCurrent = Nothing}

openContainer :: Int -> Text -> State -> State
openContainer n spec state
    | Just draft <- stateDraft state =
        failAt n ("questions cannot contain containers; close " <> opener draft <> " with ':::' first") state
    | name == "tabs" = case stateTabs state of
        Just _ -> failAt n "containers cannot be nested; close the open one with ':::' first" state
        Nothing -> checkOptions state{stateTabs = Just (Container n [] Nothing)}
    | Just kind <- lookup name questionKinds = checkOptions (opened (Just kind))
    | T.null name = failAt n "name the container, for example '::: tabs' or '::: choice'" (opened Nothing)
    | otherwise =
        failAt n ("unknown container '" <> name <> "'; available: " <> T.intercalate ", " containerDirectives) (opened Nothing)
  where
    (name, rawOptions) = T.span (\c -> isAlphaNum c || c == '-') spec
    checkOptions s
        | T.null (T.strip rawOptions) = s
        | otherwise = failAt n ("'" <> name <> "' takes no options") s
    opened kind = state{stateDraft = Just (Draft kind name n Nothing [] [])}

newTab :: Int -> Text -> State -> State
newTab n label state
    | Just draft <- stateDraft state =
        failAt n ("'##' tab labels cannot go inside " <> opener draft <> "; close it with ':::' first") state
    | otherwise = case stateTabs state of
        Nothing -> failAt n "'##' is only used for tab labels inside '::: tabs'" state
        Just container
            | T.null label -> failAt n "a '##' tab label needs text" state
            | otherwise ->
                state{stateTabs = Just (finishTab container){containerCurrent = Just (n, label, [])}}

finishTab :: Container -> Container
finishTab container = case containerCurrent container of
    Nothing -> container
    Just (labelLine, label, blocks) ->
        container{containerTabs = (labelLine, Tab label (reverse blocks)) : containerTabs container, containerCurrent = Nothing}

closeContainer :: Int -> State -> State
closeContainer n state = case (stateDraft state, stateTabs state) of
    (Just draft, _) -> finishQuestion draft state{stateDraft = Nothing}
    (Nothing, Just container) -> finishTabs container state{stateTabs = Nothing}
    (Nothing, Nothing) -> failAt n "':::' closes nothing; open a container first, for example '::: tabs'" state

finishTabs :: Container -> State -> State
finishTabs container closed
    | null tabs = failAt (containerLine container) "'::: tabs' needs at least one '## label'" closed
    | not (null emptyTabs) = closed{stateErrors = reverse emptyTabs ++ stateErrors closed}
    | otherwise = addBlock (containerLine container) (Tabs (map snd tabs)) closed
  where
    tabs = reverse (containerTabs (finishTab container))
    emptyTabs = [ParseError labelLine ("tab '" <> tabLabel tab <> "' has no content") | (labelLine, tab) <- tabs, null (tabBlocks tab)]

finishQuestion :: Draft -> State -> State
finishQuestion draft closed = case (draftKind draft, draftText draft) of
    (Nothing, _) -> closed
    (Just _, Nothing) -> failAt (draftLine draft) (opener draft <> " needs its question on the next line") closed
    (Just kind, Just (textLine, text))
        | kind /= FreeText && length choices < 2 ->
            failAt (draftLine draft) (opener draft <> " needs at least two '- option' lines") closed
        | kind == SingleChoice && length (filter choiceRecommended choices) > 1 ->
            failAt (draftLine draft) "a '::: choice' question can recommend only one option" closed
        | otherwise ->
            addBlock (draftLine draft) (Ask (Question kind textLine text (reverse (draftContext draft)) choices)) closed
  where
    choices = map snd (reverse (draftChoices draft))

-- | How the source opened a draft, for error messages.
opener :: Draft -> Text
opener draft = "'::: " <> draftName draft <> "'"

failAt :: Int -> Text -> State -> State
failAt n message state = state{stateErrors = ParseError n message : stateErrors state}
