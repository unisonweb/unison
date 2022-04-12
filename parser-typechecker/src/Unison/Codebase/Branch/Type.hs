{-# LANGUAGE TemplateHaskell #-}

module Unison.Codebase.Branch.Type where

import Control.Lens
import Data.Map (Map)
import Data.Set (Set)
import Unison.Codebase.Branch.Shallow (ShallowBranch)
import Unison.Codebase.Causal.Type (Causal)
import qualified Unison.Codebase.Causal.Type as Causal
import qualified Unison.Codebase.Metadata as Metadata
import Unison.Codebase.Patch (Patch)
import Unison.Codebase.Path (Path)
import qualified Unison.Hash as Hash
import Unison.Name (Name)
import Unison.NameSegment (NameSegment)
import Unison.Reference (Reference)
import Unison.Referent (Referent)
import Unison.Util.Relation (Relation)

-- | A node in the Unison namespace hierarchy
-- along with its history.
newtype Branch m = Branch {_history :: UnwrappedBranch m}
  deriving (Eq, Ord)

type UnwrappedBranch m = Causal m ShallowBranch (Branch0 m)

type Hash = Causal.RawHash ShallowBranch

type EditHash = Hash.Hash

type Star r n = Metadata.Star r n

head :: Branch m -> Branch0 m
head (Branch c) = Causal.head c

headHash :: Branch m -> Hash
headHash (Branch c) = Causal.currentHash c

-- | A node in the Unison namespace hierarchy.
--
-- '_terms' and '_types' are the declarations at this level.
-- '_children' are the nodes one level below us.
-- '_edits' are the 'Patch's stored at this node in the code.
--
-- The @deep*@ fields are derived from the four above.
data Branch0 m = Branch0
  { _terms :: Star Referent NameSegment,
    _types :: Star Reference NameSegment,
    -- | Note the 'Branch' here, not 'Branch0'.
    -- Every level in the tree has a history.
    _children :: Map NameSegment (Branch m),
    _edits :: Map NameSegment (EditHash, m Patch),
    -- names and metadata for this branch and its children
    -- (ref, (name, value)) iff ref has metadata `value` at name `name`
    deepTerms :: Relation Referent Name,
    deepTypes :: Relation Reference Name,
    deepTermMetadata :: Metadata.R4 Referent Name,
    deepTypeMetadata :: Metadata.R4 Reference Name,
    deepPaths :: Set Path,
    deepEdits :: Map Name EditHash
  }

instance Eq (Branch0 m) where
  a == b =
    _terms a == _terms b
      && _types a == _types b
      && _children a == _children b
      && (fmap fst . _edits) a == (fmap fst . _edits) b

history :: Iso' (Branch m) (UnwrappedBranch m)
history = iso _history Branch

edits :: Lens' (Branch0 m) (Map NameSegment (EditHash, m Patch))
edits = lens _edits (\b0 e -> b0 {_edits = e})
