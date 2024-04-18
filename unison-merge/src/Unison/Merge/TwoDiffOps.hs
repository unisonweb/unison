{-# LANGUAGE OverloadedRecordDot #-}

module Unison.Merge.TwoDiffOps
  ( TwoDiffOps (..),
    make,
  )
where

import Data.These (These (..))
import Unison.Merge.DiffOp (DiffOp (..))
import Unison.Merge.EitherWay (EitherWay (..))
import Unison.Merge.TwoWay (TwoWay (..))
import Unison.Merge.Updated (Updated (..))

data TwoDiffOps a
  = TwoDiffOps'Add !(EitherWay a)
  | TwoDiffOps'Delete !(EitherWay a)
  | TwoDiffOps'Update !(EitherWay (Updated a))
  | TwoDiffOps'AddAdd !(TwoWay a)
  | TwoDiffOps'DeleteDelete !a
  | TwoDiffOps'DeleteUpdate !(Updated a) -- old was deleted by Alice
  | TwoDiffOps'UpdateDelete !(Updated a) -- old was deleted by Bob
  | TwoDiffOps'UpdateUpdate !a !(TwoWay a) -- old, new

-- | Combine two aligned @DiffOp@ into one @TwoDiffOps@.
make :: These (DiffOp a) (DiffOp a) -> TwoDiffOps a
make = \case
  This alice -> one Alice alice
  That bob -> one Bob bob
  These (DiffOp'Add alice) (DiffOp'Add bob) -> TwoDiffOps'AddAdd TwoWay {alice, bob}
  These (DiffOp'Delete alice) (DiffOp'Delete _bob) -> TwoDiffOps'DeleteDelete alice
  These (DiffOp'Delete _) (DiffOp'Update bob) -> TwoDiffOps'DeleteUpdate bob
  These (DiffOp'Update alice) (DiffOp'Delete _) -> TwoDiffOps'UpdateDelete alice
  These (DiffOp'Update alice) (DiffOp'Update bob) ->
    TwoDiffOps'UpdateUpdate alice.old TwoWay {alice = alice.new, bob = bob.new}
  -- These don't make sense - e.g. someone can't update or delete something that wasn't there
  These (DiffOp'Add _) (DiffOp'Delete _) -> error "impossible"
  These (DiffOp'Add _) (DiffOp'Update _) -> error "impossible"
  These (DiffOp'Delete _) (DiffOp'Add _) -> error "impossible"
  These (DiffOp'Update _) (DiffOp'Add _) -> error "impossible"
  where
    one :: (forall a. a -> EitherWay a) -> DiffOp a -> TwoDiffOps a
    one who = \case
      DiffOp'Add x -> TwoDiffOps'Add (who x)
      DiffOp'Delete x -> TwoDiffOps'Delete (who x)
      DiffOp'Update x -> TwoDiffOps'Update (who x)
