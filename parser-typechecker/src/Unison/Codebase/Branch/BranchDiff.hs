module Unison.Codebase.Branch.BranchDiff
  ( BranchDiff (..),
    diff0,
  )
where

import Control.Lens
import Unison.Codebase.Branch (Branch0)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Metadata qualified as Metadata
import Unison.NameSegment (NameSegment)
import Unison.Reference (Reference)
import Unison.Referent (Referent)
import Unison.Util.Star2 qualified as Star2

type Star r n = Metadata.Star r n

-- Represents a shallow diff of a Branch0.
-- Each of these `Star`s contain metadata as well, so an entry in
-- `added` or `removed` could be an update to the metadata.
data BranchDiff = BranchDiff
  { addedTerms :: Star Referent NameSegment,
    removedTerms :: Star Referent NameSegment,
    addedTypes :: Star Reference NameSegment,
    removedTypes :: Star Reference NameSegment
  }
  deriving (Eq, Ord, Show)

diff0 :: Branch0 m -> Branch0 m -> BranchDiff
diff0 old new = do
  BranchDiff
    { addedTerms = Star2.difference (new ^. Branch.terms) (old ^. Branch.terms),
      removedTerms = Star2.difference (old ^. Branch.terms) (new ^. Branch.terms),
      addedTypes = Star2.difference (new ^. Branch.types) (old ^. Branch.types),
      removedTypes = Star2.difference (old ^. Branch.types) (new ^. Branch.types)
    }

instance Semigroup BranchDiff where
  left <> right =
    BranchDiff
      { addedTerms = addedTerms left <> addedTerms right,
        removedTerms = removedTerms left <> removedTerms right,
        addedTypes = addedTypes left <> addedTypes right,
        removedTypes = removedTypes left <> removedTypes right
      }

instance Monoid BranchDiff where
  mempty = BranchDiff mempty mempty mempty mempty
