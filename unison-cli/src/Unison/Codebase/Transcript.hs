{-# LANGUAGE PatternSynonyms #-}

-- | The data model for Unison transcripts.
module Unison.Codebase.Transcript
  ( ExpectingError,
    HasBug,
    ScratchFileName,
    Hidden (..),
    UcmLine (..),
    UcmContext (..),
    APIRequest (..),
    pattern CMarkCodeBlock,
    Stanza,
    InfoTags (..),
    getCommonInfoTags,
    Transcript (..),
    settings,
    TranscriptType (..),
    Behaviors (..),
    extractBehaviors,
    Settings (..),
    defaultInfoTags,
    ProcessedBlock (..),
    CMark.Node,
  )
where

import CMark qualified
import Data.Aeson.Types ((.:?), (.=))
import Data.Aeson.Types qualified as Aeson
import Data.Functor.Classes (Show1, liftShowsPrec)
import GHC.Show (showList__)
import Unison.Core.Project (ProjectBranchName, ProjectName)
import Unison.Prelude
import Unison.Project (ProjectAndBranch)

-- | A transcript is executable Markdown, akin to a Jupyter Notebook.
--
--  __NB__: This is effectively the AST. It needs to preserve anything we want to be able to serialize into transcript
--          output.
data Transcript = Transcript {frontmatter :: Aeson.Value, stanzas :: [Stanza]}
  deriving (Show)

type ExpectingError = Bool

type HasBug = Bool

type ScratchFileName = Text

data Hidden = Shown | HideOutput | HideAll
  deriving (Eq, Ord, Read, Show)

data UcmLine
  = UcmCommand UcmContext Text
  | -- | Text does not include the '--' prefix.
    UcmComment Text
  | UcmOutputLine Text
  deriving (Eq, Show)

-- | Where a command is run: a project branch (myproject/mybranch>).
data UcmContext
  = -- | Use the current project & branch
    UcmContextEmpty
  | -- | Explicit project & branch
    UcmContextProject (ProjectAndBranch ProjectName ProjectBranchName)
  deriving (Eq, Show)

data APIRequest
  = -- URL
    GetRequest Text
  | -- | URL, Body
    PostRequest Text Text
  | APIComment Text
  | APIResponse Text
  deriving (Eq, Show)

pattern CMarkCodeBlock :: (Maybe CMark.PosInfo) -> Text -> Text -> CMark.Node
pattern CMarkCodeBlock pos info body = CMark.Node pos (CMark.CODE_BLOCK info body) []

type Stanza = Either CMark.Node ProcessedBlock

data InfoTags a = InfoTags
  { -- | `Nothing` represents the default value, which differs depending on the `TranscriptType` and `ProcessedBlock`
    --   tag.
    hidden :: Maybe Hidden,
    expectingError :: ExpectingError,
    hasBug :: HasBug,
    generated :: Bool,
    additionalTags :: a
  }
  deriving (Eq, Ord, Read, Show, Foldable, Functor, Traversable)

-- | A set of customizable settings for a transcript.
data Settings = Settings
  { transcriptType :: Maybe TranscriptType,
    behaviors :: Behaviors Maybe
  }
  deriving (Show)

instance Aeson.FromJSON Settings where
  parseJSON = \case
    Aeson.Object v ->
      Settings
        <$> fmap (resolveType =<<) (v .:? "type")
        <*> (maybe (pure mempty) (fmap (flip Behaviors Nothing) . Aeson.parseJSON) =<< v .:? "autoupdate")
    invalid -> Aeson.typeMismatch "Settings" invalid

instance Aeson.ToJSON Settings where
  toJSON Settings {transcriptType, behaviors} =
    Aeson.object $
      catMaybes
        [ ("type" .=) <$> transcriptType,
          ("autoupdate" .=) <$> autoupdate behaviors
        ]

instance Semigroup Settings where
  Settings t b <> Settings t' b' = Settings (t <|> t') $ b <> b'

instance Monoid Settings where
  mempty = Settings Nothing mempty

settings :: Transcript -> Settings
settings = foldMap id . Aeson.fromJSON . frontmatter

-- | The atomic behaviors that can be controlled by settings. They may be set individually, or implied by some aggregate
--   setting.
data Behaviors f = Behaviors
  { autoupdate :: f Bool,
    getHidden :: f (ProcessedBlock -> Hidden)
  }

-- | `Behaviors` _may_ contain functions, so this instance outputs “_” in place of any functions.
instance (Show1 f) => Show (Behaviors f) where
  showsPrec p Behaviors {autoupdate, getHidden} =
    let appPrec = 10
        nextPrec = appPrec + 1
     in showParen (nextPrec <= p) $
          showString "Behaviors"
            . showsPrec nextPrec autoupdate
            . showString " "
            . liftShowsPrec (\_ _ -> ("_" <>)) (showList__ (const ("_" <>))) nextPrec getHidden

instance Semigroup (Behaviors Maybe) where
  Behaviors a h <> Behaviors a' h' = Behaviors (a <|> a') $ h <|> h'

instance Monoid (Behaviors Maybe) where
  mempty = Behaviors Nothing Nothing

extractBehaviors :: Settings -> Behaviors Identity
extractBehaviors Settings {transcriptType, behaviors} =
  let unified = behaviors <> transcriptBehaviors (fromMaybe Standard transcriptType)
      determineBehavior :: (forall f. Behaviors f -> f a) -> Identity a
      determineBehavior field = maybe (field defaultBehaviors) pure $ field unified
   in Behaviors (determineBehavior autoupdate) $ determineBehavior getHidden

defaultBehaviors :: Behaviors Identity
defaultBehaviors = Behaviors (Identity False) . Identity $ fromMaybe Shown . hidden . getCommonInfoTags

-- | Different types make various settingal changes.
data TranscriptType
  = -- | The default type, nothing changed from the default setting.
    Standard
  | -- | Enables auto-update and changes the default hidden value for `Unison` blocks to `HideOutput`.
    Tutorial
  deriving (Eq, Ord, Read, Show)

resolveType :: Text -> Maybe TranscriptType
resolveType = \case
  "standard" -> pure Standard
  "tutorial" -> pure Tutorial
  _ -> Nothing

instance Aeson.ToJSON TranscriptType where
  toJSON = \case
    Standard -> "standard"
    Tutorial -> "tutorial"

transcriptBehaviors :: TranscriptType -> Behaviors Maybe
transcriptBehaviors = \case
  Standard -> Behaviors Nothing Nothing
  Tutorial -> Behaviors (pure True) $ pure \case
    Unison tags _ -> fromMaybe HideOutput $ hidden tags
    block -> fromMaybe Shown . hidden $ getCommonInfoTags block

defaultInfoTags :: a -> InfoTags a
defaultInfoTags = InfoTags Nothing False False False

data ProcessedBlock
  = Ucm (InfoTags ()) [UcmLine]
  | Unison (InfoTags (Maybe ScratchFileName)) Text
  | API (InfoTags ()) [APIRequest]
  deriving (Eq, Show)

getCommonInfoTags :: ProcessedBlock -> InfoTags ()
getCommonInfoTags = \case
  Ucm tags _ -> tags
  Unison tags _ -> () <$ tags
  API tags _ -> tags
