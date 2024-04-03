{-# LANGUAGE OverloadedRecordDot #-}

module Unison.Merge.Unconflicts
  ( Unconflicts (..),
  )
where

import Unison.Merge.TwoWayI (TwoWayI)
import Unison.Name (Name)
import Unison.Prelude

data Unconflicts v = Unconflicts
  { adds :: !(TwoWayI (Map Name v)),
    deletes :: !(TwoWayI (Map Name v)),
    updates :: !(TwoWayI (Map Name v))
  }
  deriving stock (Foldable, Functor, Generic)
