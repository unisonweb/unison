{-# LANGUAGE OverloadedRecordDot #-}

module Unison.Merge.Unconflicts
  ( Unconflicts (..),
    deletedAndUpdatedNames,
  )
where

import Control.Lens (view)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Unison.Merge.AliceIorBob (AliceIorBob (..))
import Unison.Merge.AliceIorBob qualified as AliceIorBob
import Unison.Merge.TwoWay (TwoWay (..))
import Unison.Merge.TwoWayI (TwoWayI)
import Unison.Name (Name)
import Unison.Prelude

data Unconflicts v = Unconflicts
  { adds :: !(TwoWayI (Map Name v)),
    deletes :: !(TwoWayI (Map Name v)),
    updates :: !(TwoWayI (Map Name v))
  }
  deriving stock (Foldable, Functor, Generic)

deletedAndUpdatedNames :: Unconflicts v -> TwoWay (Set Name)
deletedAndUpdatedNames unconflicts =
  TwoWay
    { alice = f OnlyAlice,
      bob = f OnlyBob
    }
  where
    f :: AliceIorBob -> Set Name
    f who =
      Set.union
        (Map.keysSet (view (AliceIorBob.whoL who) unconflicts.deletes))
        (Map.keysSet (view (AliceIorBob.whoL who) unconflicts.updates))
