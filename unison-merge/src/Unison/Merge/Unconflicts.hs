{-# LANGUAGE OverloadedRecordDot #-}

module Unison.Merge.Unconflicts
  ( Unconflicts (..),
    empty,
  )
where

import Data.Map.Strict qualified as Map
import Unison.Merge.TwoWayI (TwoWayI (..))
import Unison.Name (Name)
import Unison.Prelude hiding (empty)

data Unconflicts v = Unconflicts
  { adds :: !(TwoWayI (Map Name v)),
    deletes :: !(TwoWayI (Map Name v)),
    updates :: !(TwoWayI (Map Name v))
  }
  deriving stock (Foldable, Functor, Generic)

empty :: Unconflicts v
empty =
  Unconflicts x x x
  where
    x = TwoWayI Map.empty Map.empty Map.empty
