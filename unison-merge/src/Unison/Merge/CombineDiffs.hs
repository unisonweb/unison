{-# LANGUAGE OverloadedRecordDot #-}

-- | Combine two diffs together.
module Unison.Merge.CombineDiffs
  ( AliceIorBob (..),
    Unconflicts (..),
    combineDiffs,
  )
where

import Control.Lens (Lens', view, (%~))
import Data.Map.Strict qualified as Map
import Data.Semialign (alignWith)
import Data.These (These (..))
import Unison.Merge.DiffOp (DiffOp (..))
import Unison.Merge.Synhashed (Synhashed (..))
import Unison.Merge.TwoWay (TwoWay (..))
import Unison.Merge.TwoWayI (TwoWayI (..))
import Unison.Name (Name)
import Unison.Prelude hiding (catMaybes)
import Unison.Reference (TypeReference)
import Unison.Referent (Referent)
import Unison.Util.Defns (Defns (..), DefnsF)

-- | Alice inclusive-or Bob?
data AliceIorBob
  = OnlyAlice
  | OnlyBob
  | AliceAndBob

-- Alice exclusive-or Bob?
data AliceXorBob
  = Alice
  | Bob

-- | The combined result of two diffs on the same thing.
data DiffOp2 v
  = Added2 !AliceIorBob !v
  | Updated2 !AliceIorBob !v -- new value
  | Deleted2 !AliceIorBob !v -- old value
  | -- | An add-add or an update-update conflict. We don't consider update-delete a conflict; the delete gets ignored.
    Conflict !(TwoWay v)

data Flicts v = Flicts
  { conflicts :: !(Map Name (TwoWay v)),
    unconflicts :: !(Unconflicts v)
  }
  deriving stock (Generic)

data Unconflicts v = Unconflicts
  { adds :: !(TwoWayI (Map Name v)),
    deletes :: !(TwoWayI (Map Name v)),
    updates :: !(TwoWayI (Map Name v))
  }
  deriving stock (Foldable, Functor, Generic)

-- | Combine LCA->Alice diff and LCA->Bob diff, then partition into conflicted and unconflicted things.
combineDiffs ::
  TwoWay (DefnsF (Map Name) (DiffOp (Synhashed Referent)) (DiffOp (Synhashed TypeReference))) ->
  ( DefnsF (Map Name) (TwoWay Referent) (TwoWay TypeReference),
    DefnsF Unconflicts Referent TypeReference
  )
combineDiffs diffs =
  let Flicts termConflicts termUnconflicts = partition2 (view #terms <$> diffs)
      Flicts typeConflicts typeUnconflicts = partition2 (view #types <$> diffs)
   in (Defns termConflicts typeConflicts, Defns termUnconflicts typeUnconflicts)

partition2 :: TwoWay (Map Name (DiffOp (Synhashed v))) -> Flicts v
partition2 diffs =
  partition (alignWith (combine Alice Bob) diffs.alice diffs.bob)

partition :: Map Name (DiffOp2 v) -> Flicts v
partition =
  Map.foldlWithKey'
    (\s k v -> insert k v s)
    Flicts
      { unconflicts =
          let empty = TwoWayI Map.empty Map.empty Map.empty
           in Unconflicts empty empty empty,
        conflicts = Map.empty
      }
  where
    insert :: Name -> DiffOp2 v -> Flicts v -> Flicts v
    insert k = \case
      Added2 who v -> #unconflicts . #adds . whoL who %~ Map.insert k v
      Deleted2 who v -> #unconflicts . #deletes . whoL who %~ Map.insert k v
      Updated2 who v -> #unconflicts . #updates . whoL who %~ Map.insert k v
      Conflict v -> #conflicts %~ Map.insert k v

whoL :: AliceIorBob -> Lens' (TwoWayI a) a
whoL = \case
  OnlyAlice -> #alice
  OnlyBob -> #bob
  AliceAndBob -> #both

combine :: AliceXorBob -> AliceXorBob -> These (DiffOp (Synhashed v)) (DiffOp (Synhashed v)) -> DiffOp2 v
combine this that = \case
  This x -> one this x
  That x -> one that x
  These (Added x) (Added y)
    | x /= y -> Conflict (twoWay this x.value y.value)
    | otherwise -> Added2 AliceAndBob x.value
  These (Updated _ x) (Updated _ y)
    | x /= y -> Conflict (twoWay this x.value y.value)
    | otherwise -> Updated2 AliceAndBob x.value
  -- Not a conflict, perhaps only temporarily, because it's easier to implement (we ignore these deletes):
  These (Updated _ x) (Deleted _) -> Updated2 (xor2ior this) x.value
  These (Deleted x) (Deleted _) -> Deleted2 AliceAndBob x.value
  -- Handle delete+update the same as update+delete
  These x@(Deleted _) y -> combine that this (These y x)
  -- These don't make sense - e.g. someone can't update something that wasn't there
  These (Updated _ _) (Added _) -> error "impossible"
  These (Added _) (Deleted _) -> error "impossible"
  These (Added _) (Updated _ _) -> error "impossible"

one :: AliceXorBob -> DiffOp (Synhashed v) -> DiffOp2 v
one who = \case
  Added x -> Added2 (xor2ior who) x.value
  Deleted x -> Deleted2 (xor2ior who) x.value
  Updated _ x -> Updated2 (xor2ior who) x.value

-- Make a two way, given who is on the left.
twoWay :: AliceXorBob -> v -> v -> TwoWay v
twoWay Alice alice bob = TwoWay {alice, bob}
twoWay Bob bob alice = TwoWay {alice, bob}

xor2ior :: AliceXorBob -> AliceIorBob
xor2ior = \case
  Alice -> OnlyAlice
  Bob -> OnlyBob
