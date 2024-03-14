{-# LANGUAGE OverloadedRecordDot #-}

-- | Combine two diffs together.
module Unison.Merge.CombineDiffs
  ( CombinedDiffsOp (..),
    AliceIorBob (..),
    combineDiffs,
  )
where

import Data.Semialign (alignWith)
import Data.These (These (..))
import Unison.Merge.DiffOp (DiffOp (..))
import Unison.Merge.Synhashed (Synhashed (..))
import Unison.Merge.TwoWay (TwoWay (..))
import Unison.Name (Name)
import Unison.Prelude hiding (catMaybes)

-- | The combined result of two diffs on the same thing.
data CombinedDiffsOp v
  = Added2 !AliceIorBob !v
  | Updated2 !AliceIorBob !v -- new value
  | Deleted2 !AliceIorBob !v -- old value
  | -- | An add-add or an update-update conflict. We don't consider update-delete a conflict; the delete gets ignored.
    Conflict !(TwoWay v)

-- | Alice inclusive-or Bob?
data AliceIorBob
  = OnlyAlice
  | OnlyBob
  | AliceAndBob

-- Alice exclusive-or Bob?
data AliceXorBob
  = Alice
  | Bob

combineDiffs :: TwoWay (Map Name (DiffOp (Synhashed v))) -> Map Name (CombinedDiffsOp v)
combineDiffs diffs =
  alignWith (f Alice Bob) diffs.alice diffs.bob
  where
    diffOpToTag :: AliceXorBob -> DiffOp (Synhashed v) -> CombinedDiffsOp v
    diffOpToTag who = \case
      Added x -> Added2 (xor2ior who) x.value
      Deleted x -> Deleted2 (xor2ior who) x.value
      Updated _ x -> Updated2 (xor2ior who) x.value

    f :: AliceXorBob -> AliceXorBob -> These (DiffOp (Synhashed v)) (DiffOp (Synhashed v)) -> CombinedDiffsOp v
    f this that = \case
      This x -> diffOpToTag this x
      That x -> diffOpToTag that x
      These (Added x) (Added y)
        | x /= y -> Conflict (twoWay this x.value y.value)
        | otherwise -> Added2 AliceAndBob x.value
      These (Updated _ x) (Updated _ y)
        | x /= y -> Conflict (twoWay this x.value y.value)
        | otherwise -> Updated2 AliceAndBob x.value
      -- Not a conflict, perhaps only temporarily, because it's easier to implement (we ignore these deletes):
      These (Updated _ x) (Deleted _) -> Updated2 (xor2ior this) x.value
      These (Deleted x) (Deleted _) -> Deleted2 AliceAndBob x.value
      These x@(Deleted _) y -> f that this (These y x)
      -- These don't make sense - e.g. someone can't update something that wasn't there
      These (Updated _ _) (Added _) -> error "impossible"
      These (Added _) (Deleted _) -> error "impossible"
      These (Added _) (Updated _ _) -> error "impossible"

    -- Make a two way, given who is on the left.
    twoWay :: AliceXorBob -> v -> v -> TwoWay v
    twoWay Alice alice bob = TwoWay {alice, bob}
    twoWay Bob bob alice = TwoWay {alice, bob}

xor2ior :: AliceXorBob -> AliceIorBob
xor2ior = \case
  Alice -> OnlyAlice
  Bob -> OnlyBob
