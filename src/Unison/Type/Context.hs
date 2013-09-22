{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Unison.Type.Context where

import Unison.Syntax.Type as T
import Unison.Syntax.Var as V
import Unison.Type.Context.Element as E

-- | An ordered algorithmic context
data Context (t :: E.T) sa a v = Context [Element t sa a v]

-- | Extend this `Context` by one element
extend :: Element t sa a v -> Context t sa a v -> Context t sa a v
extend e (Context ctx) = Context (e : ctx)

-- | Extend this `Context`
extendUniversal :: Context t sa a (Var v) -> Context t sa a (Var v)
extendUniversal (Context ctx) =
  Context (Universal V.bound1 : map (fmap V.succ) ctx)

universals :: Context t sa a v -> [v]
universals (Context ctx) = [v | Universal v <- ctx]

existentials :: Context t sa a v -> [v]
existentials (Context ctx) = ctx >>= go where
  go (Existential v) = [v]
  go (Solved v _) = [v]
  go _ = []

wellformedType :: Eq v => Context t sa a (V.Var v) -> Type t' c k v -> Bool
wellformedType c t = case t of
  Unit -> True
  Var v -> v `elem` universals c
  Exists v -> v `elem` existentials c
  Arrow i o -> wellformedType c i && wellformedType c o
  T.Ann t' _ -> wellformedType c t'
  Constrain t' _ -> wellformedType c t'
  -- if there are no deletes in middle, may be more efficient to weaken t'
  Forall t' -> wellformedType (extendUniversal c) t'



