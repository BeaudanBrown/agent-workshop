{- | The document model shared by the parser and the renderer.

A document is parameterised by its media payload: the parser produces
'MediaRef's naming source files, and the build step traverses the document to
replace them with encoded media.
-}
module Workshop.AgentPage.Document (
    Document (..),
    Section (..),
    Block (..),
    Tab (..),
    Question (..),
    QuestionKind (..),
    Choice (..),
    MediaRef (..),
) where

import Data.Text (Text)

data Document media = Document
    { docTitle :: Text
    , docMeta :: Maybe Text
    , docDate :: Maybe Text
    , docReply :: Bool
    -- ^ whether every section and media item gets a reply box
    , docSections :: [Section media]
    }
    deriving (Eq, Show, Functor, Foldable, Traversable)

{- | A run of content. Only content before the first heading has no heading;
it always comes first.
-}
data Section media = Section
    { sectionHeading :: Maybe Text
    , sectionLine :: Int
    , sectionBlocks :: [Block media]
    }
    deriving (Eq, Show, Functor, Foldable, Traversable)

data Block media
    = Paragraph Text
    | List [Text]
    | -- | label and lines exactly as written; the label `diff` marks a diff
      Code (Maybe Text) [Text]
    | -- | header and rows, every row as wide as the header
      Table [Text] [[Text]]
    | Media media
    | Tabs [Tab media]
    | Ask (Question media)
    deriving (Eq, Show, Functor, Foldable, Traversable)

data Tab media = Tab
    { tabLabel :: Text
    , tabBlocks :: [Block media]
    }
    deriving (Eq, Show, Functor, Foldable, Traversable)

data QuestionKind = SingleChoice | MultipleChoice | FreeText
    deriving (Eq, Show)

data Question media = Question
    { questionKind :: QuestionKind
    , questionLine :: Int
    -- ^ line of the question text
    , questionText :: Text
    , questionContext :: [Block media]
    -- ^ paragraphs, media, code and tables shown between the question and its options
    , questionChoices :: [Choice]
    -- ^ empty for free text
    }
    deriving (Eq, Show, Functor, Foldable, Traversable)

data Choice = Choice
    { choiceLabel :: Text
    , choiceRecommended :: Bool
    }
    deriving (Eq, Show)

-- | A media line as written: @![caption](path)@ on a given source line.
data MediaRef = MediaRef
    { refCaption :: Text
    , refPath :: FilePath
    , refLine :: Int
    }
    deriving (Eq, Show)
