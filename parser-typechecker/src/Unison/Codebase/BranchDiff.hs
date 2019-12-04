module Unison.Codebase.BranchDiff where

import Data.Map (Map)
import Unison.Referent (Referent)
import Unison.Reference (Reference)
import qualified Unison.Codebase.Patch as Patch
import qualified Data.Map                      as Map
import Unison.Codebase.Branch (Branch0(..), EditHash)
import Unison.Name (Name)
import Unison.Codebase.Metadata (Star)
import Unison.Util.Star3 as Star3
import qualified Unison.Codebase.Branch as Branch
import Unison.Codebase.Patch (Patch)
import qualified Data.Set as Set
import Control.Monad (foldM)

data DiffType a = Create a | Delete a | Modify a

data BranchDiff = BranchDiff
  { addedTerms :: Star Referent Name
  , removedTerms :: Star Referent Name
  , addedTypes :: Star Reference Name
  , removedTypes :: Star Reference Name
  , changedPatches :: Map Name (DiffType Patch.PatchDiff)
  }

diff0 :: forall m. Monad m => Branch0 m -> Branch0 m -> m BranchDiff
diff0 old new = do
  let oldDeepEdits, newDeepEdits :: Map Name (EditHash, m Patch)
      oldDeepEdits = Branch.deepEdits' old
      newDeepEdits = Branch.deepEdits' new
  diffEdits :: Map Name (DiffType Patch.PatchDiff) <- do
    added <- do
      addedPatches :: Map Name Patch <-
        traverse snd $ Map.difference newDeepEdits oldDeepEdits
      pure $ fmap (\p -> Create (Patch.diff p mempty)) addedPatches
    removed <- do
      removedPatches :: Map Name Patch <-
        traverse snd $ Map.difference oldDeepEdits newDeepEdits
      pure $ fmap (\p -> Delete (Patch.diff mempty p)) removedPatches

    let f acc k = case (Map.lookup k oldDeepEdits, Map.lookup k newDeepEdits) of
          (Just (h1,p1), Just (h2,p2)) ->
            if h1 == h2 then pure acc
            else Map.singleton k . Modify <$> (Patch.diff <$> p2 <*> p1)
          _ -> error "we've done something very wrong"
    modified <- foldM f mempty (Set.intersection (Map.keysSet oldDeepEdits) (Map.keysSet newDeepEdits))
    pure $ added <> removed <> modified

  pure $ BranchDiff
    { addedTerms = Star3.difference (Branch.deepTerms new) (Branch.deepTerms old)
    , removedTerms = Star3.difference (Branch.deepTerms old) (Branch.deepTerms new)
    , addedTypes = Star3.difference (Branch.deepTypes new) (Branch.deepTypes old)
    , removedTypes = Star3.difference (Branch.deepTypes old) (Branch.deepTypes new)
    , changedPatches = diffEdits
    }
