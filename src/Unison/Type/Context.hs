{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Unison.Type.Context where

import Control.Applicative
import Data.List
import Unison.Syntax.Type as T hiding (vars)
import Unison.Syntax.Var as V
import Unison.Syntax.DeBruijn as D
import Unison.Type.Context.Element as E

-- | An ordered algorithmic context
-- The next fresh variable
data Context (t :: E.T) sa a v = Context v [Element t sa a v]

empty :: Context t sa a (Var v)
empty = Context V.bound1 []

-- | Extend this `Context` by one element
extend :: Element t sa a (Var v) -> Context t sa a (Var v) -> Context t sa a (Var v)
extend e (Context n ctx) = Context (V.succ n) (e : ctx)

-- | Extend this `Context` with a single universally quantified variable,
-- guaranteed to be fresh
extendUniversal :: Context t sa a (Var v) -> Context t sa a (Var v)
extendUniversal (Context n ctx) =
  Context (V.succ n) (E.Universal n : map (fmap V.succ) ctx)

universals :: Context t sa a v -> [v]
universals (Context _ ctx) = [v | E.Universal v <- ctx]

markers :: Context t sa a v -> [v]
markers (Context _ ctx) = [v | Marker v <- ctx]

existentials :: Context t sa a v -> [v]
existentials (Context _ ctx) = ctx >>= go where
  go (E.Existential v) = [v]
  go (E.Solved v _) = [v]
  go _ = []

solved :: Context t sa a v -> [(v, sa)]
solved (Context _ ctx) = [(v, sa) | Solved v sa <- ctx]

bindings :: Context t sa a v -> [(v, a)]
bindings (Context _ ctx) = [(v,a) | E.Ann v a <- ctx]

vars :: Context t sa a v -> [v]
vars = fmap fst . bindings

-- | Check that the type is well formed wrt the given `Context`
wellformedType :: Eq v => Context t sa a (V.Var v) -> Type t' c k v -> Bool
wellformedType c t = case t of
  Unit -> True
  T.Universal v -> v `elem` universals c
  T.Existential v -> v `elem` existentials c
  Arrow i o -> wellformedType c i && wellformedType c o
  T.Ann t' _ -> wellformedType c t'
  Constrain t' _ -> wellformedType c t'
  Forall t' -> wellformedType (extendUniversal c) (mapVar V.succ t')

-- | Check that the context is well formed, namely that
-- there are no circular variable references, and any types
-- mentioned in either `Ann` or `Solved` elements must be
-- wellformed with respect to the prefix of the context
-- leading up to these elements.
wellformed :: Eq v => TContext t c k v -> Bool
wellformed (Context n ctx) = all go (zip' ctx) where
  go (E.Universal v, ctx') = v `notElem` universals ctx'
  go (E.Existential v, ctx') = v `notElem` existentials ctx'
  go (Solved v sa, ctx') = v `notElem` existentials ctx' && wellformedType ctx' sa
  go (E.Ann v t, ctx') = v `notElem` vars ctx' && wellformedType ctx' t
  go (Marker v, ctx') = v `notElem` vars ctx' && v `notElem` existentials ctx'
  -- zip each element with the remaining elements
  zip' ctx' = zip ctx' (map (Context n) $ tail (tails ctx'))

substitute :: Eq v => TContext t c k v -> Polytype c k v -> Polytype c k v
substitute ctx t = case t of
  T.Universal _ -> t
  Unit -> t
  T.Existential v -> maybe t (substitute ctx) (lookup v (solved ctx))
  T.Arrow i o -> T.Arrow (substitute ctx i) (substitute ctx o)
  T.Ann v k -> T.Ann (substitute ctx v) k
  T.Constrain v c -> T.Constrain (substitute ctx v) c
  T.Forall v -> T.Forall (substitute ctx v) -- not sure this is correct
  -- prob need to succ when descending into the forall
  -- basically, need to mirror whatever is done when building up the context

-- data Context t sa a v
--   Empty :: Context t sa a v
--   Extend :: Element t sa a v -> Context t sa a v -> Context t sa a v
--   Bind :: Element t sa a v -> Context t sa a (Maybe v) -> Context t sa a v
type TContext t c k v = Context t (Polytype c k v) (Monotype c k v) (V.Var v)

type Note = String

note :: String -> Note
note = id

-- | `subtype ctx t1 t2` returns successfully if `t1` is a subtype of `t2`.
-- This may have the effect of altering the context.
subtype :: (Eq v, Show c, Show k, Show v)
        => TContext t c k v
        -> Polytype c k v
        -> Polytype c k v
        -> Either Note (TContext t c k v)
subtype ctx = go where
  go Unit Unit = pure ctx
  go (T.Universal v1) (T.Universal v2)
    | v1 == v2 && v1 `elem` universals ctx
    = pure ctx
  go (T.Existential v1) (T.Existential v2)
    | v1 == v2 && v1 `elem` existentials ctx
    = pure ctx
  go t1 t2 = Left $ note "not a subtype " ++ show t1 ++ " " ++ show t2


