{-# LANGUAGE OverloadedRecordDot #-}

-- | Combine two diffs together.
module Unison.Merge.CombineDiffs
  ( CombinedDiffOp (..),
    combineDiffs,
  )
where

import Data.Semialign (alignWith)
import Data.These (These (..))
import Unison.Merge.AliceIorBob (AliceIorBob (..))
import Unison.Merge.AliceXorBob (AliceXorBob (..))
import Unison.Merge.DiffOp (DiffOp (..))
import Unison.Merge.Synhashed (Synhashed (..))
import Unison.Merge.TwoDiffOps (TwoDiffOps (..), combineTwoDiffOps)
import Unison.Merge.TwoWay (TwoWay (..), twoWay)
import Unison.Merge.TwoWay qualified as TwoWay
import Unison.Name (Name)
import Unison.Prelude hiding (catMaybes)
import Unison.Reference (TypeReference)
import Unison.Referent (Referent)
import Unison.Util.Defns (DefnsF2, DefnsF3)

-- | The combined result of two diffs on the same thing.
data CombinedDiffOp a
  = CombinedDiffOp'Add !AliceIorBob !a
  | CombinedDiffOp'Delete !AliceIorBob !a -- old value
  | CombinedDiffOp'Update !AliceIorBob !a !a -- old value, new value
  | -- An add-add or an update-update conflict. We don't consider update-delete a conflict; the delete gets ignored.
    CombinedDiffOp'Conflict !(TwoWay a)
  deriving stock (Show)

-- | Combine LCA->Alice diff and LCA->Bob diff.
combineDiffs ::
  TwoWay (DefnsF3 (Map Name) DiffOp Synhashed Referent TypeReference) ->
  DefnsF2 (Map Name) CombinedDiffOp Referent TypeReference
combineDiffs =
  bimap f f . TwoWay.sequenceDefns
  where
    f = twoWay (alignWith combine)

combine :: These (DiffOp (Synhashed ref)) (DiffOp (Synhashed ref)) -> CombinedDiffOp ref
combine =
  combineTwoDiffOps >>> \case
    TwoDiffOps'Add who x -> CombinedDiffOp'Add (xor2ior who) x.value
    TwoDiffOps'Delete who x -> CombinedDiffOp'Delete (xor2ior who) x.value
    TwoDiffOps'Update who old new -> CombinedDiffOp'Update (xor2ior who) old.value new.value
    TwoDiffOps'AddAdd TwoWay {alice, bob}
      | alice /= bob -> CombinedDiffOp'Conflict TwoWay {alice = alice.value, bob = bob.value}
      | otherwise -> CombinedDiffOp'Add AliceAndBob alice.value
    TwoDiffOps'DeleteDelete x -> CombinedDiffOp'Delete AliceAndBob x.value
    -- These two are not a conflicts, perhaps only temporarily, because it's easier to implement. We just ignore these
    -- deletes and keep the updates.
    TwoDiffOps'DeleteUpdate old new -> CombinedDiffOp'Update OnlyBob old.value new.value
    TwoDiffOps'UpdateDelete old new -> CombinedDiffOp'Update OnlyAlice old.value new.value
    TwoDiffOps'UpdateUpdate old TwoWay {alice, bob}
      | alice /= bob -> CombinedDiffOp'Conflict TwoWay {alice = alice.value, bob = bob.value}
      | otherwise -> CombinedDiffOp'Update AliceAndBob old.value alice.value

xor2ior :: AliceXorBob -> AliceIorBob
xor2ior = \case
  Alice -> OnlyAlice
  Bob -> OnlyBob
