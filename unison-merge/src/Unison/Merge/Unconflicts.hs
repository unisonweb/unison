{-# LANGUAGE OverloadedRecordDot #-}

module Unison.Merge.Unconflicts
  ( Unconflicts (..),
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
