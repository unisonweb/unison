module Unison.Codebase.Branch.BranchDiff where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.Merge.Lazy as MapMerge
import Unison.Codebase.Branch.Type (Branch0 (_edits, _terms, _types))
import qualified Unison.Codebase.Metadata as Metadata
import qualified Unison.Codebase.Patch as Patch
import Unison.NameSegment (NameSegment)
import Unison.Reference (Reference)
import Unison.Referent (Referent)
import qualified Unison.Util.Star3 as Star3

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
  newEdits <- sequenceA $ snd <$> _edits new
  oldEdits <- sequenceA $ snd <$> _edits old
  let diffEdits =
        MapMerge.merge
          (MapMerge.mapMissing $ \_ p -> Patch.diff p mempty)
          (MapMerge.mapMissing $ \_ p -> Patch.diff mempty p)
          (MapMerge.zipWithMatched (const Patch.diff))
          newEdits
          oldEdits
  pure $
    BranchDiff
      { addedTerms = Star3.difference (_terms new) (_terms old),
        removedTerms = Star3.difference (_terms old) (_terms new),
        addedTypes = Star3.difference (_types new) (_types old),
        removedTypes = Star3.difference (_types old) (_types new),
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
