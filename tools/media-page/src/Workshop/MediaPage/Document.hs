{- | The document model shared by the parser and the renderer.

A document is parameterised by its media payload: the parser produces
'MediaRef's naming source files, and the build step traverses the document to
replace them with encoded media.
-}
module Workshop.MediaPage.Document (
    Document (..),
    Section (..),
    Block (..),
    Tab (..),
    MediaRef (..),
) where

import Data.Text (Text)

data Document media = Document
    { docTitle :: Text
    , docMeta :: Maybe Text
    , docDate :: Maybe Text
    , docSections :: [Section media]
    }
    deriving (Eq, Show, Functor, Foldable, Traversable)

data Section media = Section
    { sectionHeading :: Text
    , sectionBlocks :: [Block media]
    }
    deriving (Eq, Show, Functor, Foldable, Traversable)

data Block media
    = Paragraph Text
    | Media media
    | Tabs [Tab media]
    deriving (Eq, Show, Functor, Foldable, Traversable)

data Tab media = Tab
    { tabLabel :: Text
    , tabBlocks :: [Block media]
    }
    deriving (Eq, Show, Functor, Foldable, Traversable)

-- | A media line as written: @![caption](path)@ on a given source line.
data MediaRef = MediaRef
    { refCaption :: Text
    , refPath :: FilePath
    , refLine :: Int
    }
    deriving (Eq, Show)
