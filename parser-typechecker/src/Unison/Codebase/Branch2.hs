module Unison.Codebase.Branch2 where

import qualified Unison.Codebase.Branch as Branch
import           Data.GUID                (genText)
import           Data.Text                (Text)


newtype NameSegment = NameSegment { toText :: Text } deriving (Eq, Ord, Show)
newtype Path = Path { toList :: [NameSegment] }
type GUID = Text

-- These Branches are positioned in two dimensions:
-- name prefix (tree position), and time.
--
-- When we update one, we create a new point in time,
-- and need to propagate our new time value back up the tree to produce
-- a tree root corresponding to the new point in time.
-- How can we do that?  Need some zipper to pass around instead of just Links.
newtype Branch = Branch { unbranch :: Causal Branch0 } deriving (Eq, Show)

data Branch0 = Branch0
	{ _terms :: Relation NameSegment Referent
  , _types :: Relation NameSegment Reference
  , _children :: Relation NameSegment Link
  } deriving (Eq, Ord, Show)

data RemotePath = Github { username :: Text, repo :: Text, commit :: Text }
            --  | ... future
            deriving (Eq, Ord, Show)

data Link = LocalLink Hash | RemoteLink RemotePath
  deriving (Eq, Ord, Show)

-- want some type that represents a Branch0, with the ability to update its parents if we update the Branch0.
type Foo m = ()

data UnisonRepo = UnisonRepo
  { _rootNamespace :: Link
  , _editMap :: EditMap
  , _editNames :: Relation Text GUID
  }

data Edits = Edits
	{ _termEdits :: Relation Reference TermEdit
	, _typeEdits :: Relation Reference TypeEdit
	}

newtype EditMap = EditMap { toMap :: Map GUID (Causal Edits) }
type FriendlyEditNames = Relation Text GUID

newtype Branch = Branch { _history :: Causal Branch0 }

data Branch0 = Branch0
  { _terms :: Relation NameSegment Referent
  , _types :: Relation NameSegment Reference
  , _children :: Map NameSegment Link
  }

data Link = LocalLink Hash | RemoteLink RemotePath
getLocalBranch :: Hash -> m Branch
getRemoteBranch :: RemotePath -> m Branch

makeLenses ''Namespace
makeLenses ''Edits
makeLenses ''Branch
makeLenses ''Branch0
makeLenses ''Causal
