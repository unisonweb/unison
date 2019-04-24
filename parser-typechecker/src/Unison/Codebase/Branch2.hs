{-# LANGUAGE LambdaCase #-}

module Unison.Codebase.Branch2 where

-- import qualified Unison.Codebase.Branch as Branch

import           Prelude                  hiding (head,subtract)

-- import           Control.Lens
-- import           Control.Monad            (join)
-- import           Data.GUID                (genText)
import           Data.List                (intercalate)
import           Data.Map                 (Map)
import           Data.Text                (Text)
import qualified Data.Text as Text

import           Unison.Codebase.Causal   (Causal)
import           Unison.Codebase.TermEdit (TermEdit)
import           Unison.Codebase.TypeEdit (TypeEdit)
import           Unison.Hash              (Hash)
import           Unison.Reference         (Reference)
import           Unison.Referent          (Referent)
import           Unison.Util.Relation     (Relation)


newtype NameSegment = NameSegment { toText :: Text } deriving (Eq, Ord, Show)

newtype Path = Path { toList :: [NameSegment] } deriving (Eq, Ord)

instance Show Path where
  show (Path nss) = intercalate "/" $ fmap escape1 nss
    where escape1 ns = escape =<< (Text.unpack . toText $ ns)
          escape = \case '/' -> "\\/"; c -> [c]

data RepoRef
  = Local
  | Github { username :: Text, repo :: Text, commit :: Text }
  deriving (Eq, Ord, Show)

type EditGuid = Text

data RepoLink a = RepoLink RepoRef a
  deriving (Eq, Ord, Show)

type Link = RepoLink Hash
type EditLink = RepoLink EditGuid

data UnisonRepo = UnisonRepo
  { _rootNamespace :: Link
  , _editMap :: EditMap
  , _editNames :: Relation Text EditGuid
  } deriving (Eq, Ord, Show)

data Edits = Edits
  { _termEdits :: Relation Reference TermEdit
  , _typeEdits :: Relation Reference TypeEdit
  } deriving (Eq, Ord, Show)

newtype EditMap =
  EditMap { toMap :: Map EditGuid (Causal Edits) }
  deriving (Eq, Ord, Show)

type FriendlyEditNames = Relation Text EditGuid

newtype Branch = Branch { _history :: Causal Branch0 }

data Branch0 = Branch0
  { _terms :: Relation NameSegment Referent
  , _types :: Relation NameSegment Reference
  , _children :: Map NameSegment Link
  }

-- getLocalBranch :: Hash -> IO Branch
-- getGithubBranch :: RemotePath -> IO Branch
-- getLocalEdit :: GUID -> IO Edits

-- makeLenses ''Namespace
-- makeLenses ''Edits
-- makeLenses ''Branch
-- makeLenses ''Branch0
-- makeLenses ''Causal
