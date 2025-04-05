module Unison.Codebase.Branch.BranchDiff where

import Control.Lens
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.Merge.Lazy qualified as MapMerge
import Unison.Codebase.Branch (Branch0)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Metadata qualified as Metadata
import Unison.Codebase.Patch qualified as Patch
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
    removedTypes :: Star Reference NameSegment,
    changedPatches :: Map NameSegment Patch.PatchDiff
  }
  deriving (Eq, Ord, Show)

diff0 :: (Monad m) => Branch0 m -> Branch0 m -> m BranchDiff
diff0 old new = do
  newEdits <- sequenceA $ snd <$> new ^. Branch.edits
  oldEdits <- sequenceA $ snd <$> old ^. Branch.edits
  let diffEdits =
        MapMerge.merge
          (MapMerge.mapMissing $ \_ p -> Patch.diff p mempty)
          (MapMerge.mapMissing $ \_ p -> Patch.diff mempty p)
          (MapMerge.zipWithMatched (const Patch.diff))
          newEdits
          oldEdits
  pure $
    BranchDiff
      { addedTerms = Star2.difference (new ^. Branch.terms) (old ^. Branch.terms),
        removedTerms = Star2.difference (old ^. Branch.terms) (new ^. Branch.terms),
        addedTypes = Star2.difference (new ^. Branch.types) (old ^. Branch.types),
        removedTypes = Star2.difference (old ^. Branch.types) (new ^. Branch.types),
        changedPatches = diffEdits
      }

instance Semigroup BranchDiff where
  left <> right =
    BranchDiff
      { addedTerms = addedTerms left <> addedTerms right,
        removedTerms = removedTerms left <> removedTerms right,
        addedTypes = addedTypes left <> addedTypes right,
        removedTypes = removedTypes left <> removedTypes right,
        changedPatches =
          Map.unionWith (<>) (changedPatches left) (changedPatches right)
      }

instance Monoid BranchDiff where
  mempty = BranchDiff mempty mempty mempty mempty mempty
