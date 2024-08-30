-- | Combine two diffs together.
module Unison.Merge.CombineDiffs
  ( CombinedDiffOp (..),
    combineDiffs,
  )
where

import Data.Semialign (alignWith)
import Data.These (These (..))
import Unison.Merge.DiffOp (DiffOp (..))
import Unison.Merge.EitherWay (EitherWay (..))
import Unison.Merge.EitherWayI (EitherWayI (..))
import Unison.Merge.Synhashed (Synhashed (..))
import Unison.Merge.TwoDiffOps (TwoDiffOps (..))
import Unison.Merge.TwoDiffOps qualified as TwoDiffOps
import Unison.Merge.TwoWay (TwoWay (..), twoWay)
import Unison.Merge.TwoWay qualified as TwoWay
import Unison.Merge.Updated (Updated (..))
import Unison.Name (Name)
import Unison.Prelude hiding (catMaybes)
import Unison.Reference (TypeReference)
import Unison.Referent (Referent)
import Unison.Util.Defns (DefnsF2, DefnsF3)

-- | The combined result of two diffs on the same thing.
data CombinedDiffOp a
  = CombinedDiffOp'Add !(EitherWayI a)
  | CombinedDiffOp'Delete !(EitherWayI a) -- old value
  | CombinedDiffOp'Update !(EitherWayI (Updated a))
  | -- An add-add or an update-update conflict. We don't consider update-delete a conflict; the delete gets ignored.
    CombinedDiffOp'Conflict !(TwoWay a)
  deriving stock (Functor, Show)

-- | Combine LCA->Alice diff and LCA->Bob diff.
combineDiffs ::
  TwoWay (DefnsF3 (Map Name) DiffOp Synhashed Referent TypeReference) ->
  DefnsF2 (Map Name) CombinedDiffOp Referent TypeReference
combineDiffs =
  bimap f f . TwoWay.sequenceDefns
  where
    f = twoWay (alignWith combine)

combine :: These (DiffOp (Synhashed a)) (DiffOp (Synhashed a)) -> CombinedDiffOp a
combine =
  TwoDiffOps.make >>> combine1 >>> fmap (view #value)

combine1 :: (Eq a) => TwoDiffOps a -> CombinedDiffOp a
combine1 = \case
  TwoDiffOps'Add x -> CombinedDiffOp'Add (xor2ior x)
  TwoDiffOps'Delete x -> CombinedDiffOp'Delete (xor2ior x)
  TwoDiffOps'Update x -> CombinedDiffOp'Update (xor2ior x)
  TwoDiffOps'AddAdd x
    | x.alice /= x.bob -> CombinedDiffOp'Conflict x
    | otherwise -> CombinedDiffOp'Add (AliceAndBob x.alice)
  TwoDiffOps'DeleteDelete x -> CombinedDiffOp'Delete (AliceAndBob x)
  -- These two are not a conflicts, perhaps only temporarily, because it's easier to implement. We just ignore these
  -- deletes and keep the updates.
  TwoDiffOps'DeleteUpdate x -> CombinedDiffOp'Update (OnlyBob x)
  TwoDiffOps'UpdateDelete x -> CombinedDiffOp'Update (OnlyAlice x)
  TwoDiffOps'UpdateUpdate old new
    | new.alice /= new.bob -> CombinedDiffOp'Conflict new
    | otherwise -> CombinedDiffOp'Update (AliceAndBob Updated {old, new = new.alice})

xor2ior :: EitherWay a -> EitherWayI a
xor2ior = \case
  Alice x -> OnlyAlice x
  Bob x -> OnlyBob x
