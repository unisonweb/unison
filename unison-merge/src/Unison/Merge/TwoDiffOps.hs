module Unison.Merge.TwoDiffOps
  ( TwoDiffOps (..),
    combineTwoDiffOps,
  )
where

import Data.These (These (..))
import Unison.Merge.AliceXorBob (AliceXorBob (..))
import Unison.Merge.DiffOp (DiffOp (..))
import Unison.Merge.TwoWay (TwoWay (..))

data TwoDiffOps a
  = TwoDiffOps'Add !AliceXorBob !a
  | TwoDiffOps'Delete !AliceXorBob !a
  | TwoDiffOps'Update !AliceXorBob !a !a -- old, new
  | TwoDiffOps'AddAdd !(TwoWay a)
  | TwoDiffOps'DeleteDelete !a
  | TwoDiffOps'DeleteUpdate !a !a -- old (deleted by Alice), new (updated by Bob)
  | TwoDiffOps'UpdateDelete !a !a -- old (deleted by Bob), new (updated by Alice)
  | TwoDiffOps'UpdateUpdate !a !(TwoWay a) -- old, new

-- | Combine two diff together.
combineTwoDiffOps :: These (DiffOp a) (DiffOp a) -> TwoDiffOps a
combineTwoDiffOps = \case
  This alice -> one Alice alice
  That bob -> one Bob bob
  These (DiffOp'Add alice) (DiffOp'Add bob) -> TwoDiffOps'AddAdd TwoWay {alice, bob}
  These (DiffOp'Delete alice) (DiffOp'Delete _bob) -> TwoDiffOps'DeleteDelete alice
  These (DiffOp'Delete alice) (DiffOp'Update _old bob) -> TwoDiffOps'DeleteUpdate alice bob
  These (DiffOp'Update _ alice) (DiffOp'Delete bob) -> TwoDiffOps'UpdateDelete bob alice
  These (DiffOp'Update old alice) (DiffOp'Update _old bob) -> TwoDiffOps'UpdateUpdate old TwoWay {alice, bob}
  -- These don't make sense - e.g. someone can't update or delete something that wasn't there
  These (DiffOp'Add _) (DiffOp'Delete _) -> error "impossible"
  These (DiffOp'Add _) (DiffOp'Update _ _) -> error "impossible"
  These (DiffOp'Delete _) (DiffOp'Add _) -> error "impossible"
  These (DiffOp'Update _ _) (DiffOp'Add _) -> error "impossible"
  where
    one :: AliceXorBob -> DiffOp a -> TwoDiffOps a
    one who = \case
      DiffOp'Add x -> TwoDiffOps'Add who x
      DiffOp'Delete x -> TwoDiffOps'Delete who x
      DiffOp'Update old new -> TwoDiffOps'Update who old new
