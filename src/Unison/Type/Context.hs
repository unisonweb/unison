{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Unison.Type.Context where

import Data.List
import Unison.Syntax.Type as T hiding (vars)
import Unison.Syntax.Var as V
import Unison.Type.Context.Element as E

-- | An ordered algorithmic context
data Context (t :: E.T) sa a v = Context [Element t sa a v]

-- | Extend this `Context` by one element
extend :: Element t sa a v -> Context t sa a v -> Context t sa a v
extend e (Context ctx) = Context (e : ctx)

-- | Extend this `Context` with a single universally quantified variable,
-- incrementing the DeBruijn index of all previous variables.
extendUniversal :: Context t sa a (Var v) -> Context t sa a (Var v)
extendUniversal (Context ctx) =
  Context (Universal V.bound1 : map (fmap V.succ) ctx)

universals :: Context t sa a v -> [v]
universals (Context ctx) = [v | Universal v <- ctx]

markers :: Context t sa a v -> [v]
markers (Context ctx) = [v | Marker v <- ctx]

existentials :: Context t sa a v -> [v]
existentials (Context ctx) = ctx >>= go where
  go (Existential v) = [v]
  go (Solved v _) = [v]
  go _ = []

bindings :: Context t sa a v -> [(v, a)]
bindings (Context ctx) = [(v,a) | E.Ann v a <- ctx]

vars :: Context t sa a v -> [v]
vars = fmap fst . bindings

-- | Check that the type is well formed wrt the given `Context`
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

-- | Check that the context is well formed, namely that
-- there are no circular variable references, and any types
-- mentioned in either `Ann` or `Solved` elements must be
-- wellformed with respect to the prefix of the context
-- leading up to these elements.
wellformed :: Eq v => Context t (Polytype c k v) (Monotype c k v) (V.Var v) -> Bool
wellformed (Context ctx) = all go (zip' ctx) where
  go (Universal v, ctx') = v `notElem` universals ctx'
  go (Existential v, ctx') = v `notElem` existentials ctx'
  go (Solved v sa, ctx') = v `notElem` existentials ctx' && wellformedType ctx' sa
  go (E.Ann v t, ctx') = v `notElem` vars ctx' && wellformedType ctx' t
  go (Marker v, ctx') = v `notElem` vars ctx' && v `notElem` existentials ctx'
  -- zip each element with the remaining elements
  zip' ctx' = zip ctx' (map Context $ tail (tails ctx'))




