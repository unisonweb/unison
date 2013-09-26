{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Unison.Type.Context where

import Control.Applicative
import Data.List
import Data.Maybe
import Unison.Syntax.Type as T hiding (vars)
import Unison.Syntax.Var as V
import Unison.Syntax.DeBruijn as D
import Unison.Type.Context.Element as E

-- | An ordered algorithmic context
-- Context variables will be negative, while 'normal' DeBruijn
-- will be positive, so we don't generally need to worry about
-- getting accidental collisions when applying a context to
-- a type
data Context (t :: E.T) sa a v = Context v [Element t sa a v]

empty :: Context t sa a (Var v)
empty = Context (Bound (DeBruijn 0)) []

-- | Extend this `Context` by one element
extend :: Element t sa a (Var v) -> Context t sa a (Var v) -> Context t sa a (Var v)
extend e (Context n ctx) = Context (V.decr n) (e : ctx)

-- | Extend this `Context` with a single universally quantified variable,
-- guaranteed to be fresh
extendUniversal :: Context t sa a (Var v) -> (Var v, Context t sa a (Var v))
extendUniversal (Context n ctx) =
  let v = V.decr n in (v, Context v (E.Universal n : ctx))

extendMarker :: Context Incomplete sa a (Var v)
             -> (Var v, Context Incomplete sa a (Var v))
extendMarker (Context n ctx) =
  let v = V.decr n in (v, Context v ([E.Existential v, E.Marker v] ++ ctx))

retract :: Ord v
        => Element t sa a (Var v)
        -> Context t sa a (Var v)
        -> Context t sa a (Var v)
retract m (Context _ ctx) =
  let ctx' = tail (dropWhile (isNotMarker m) ctx)
      n' = fromMaybe (Bound (DeBruijn 0))
                     (currentVar ctx') -- ok to recycle our variable supply
      isNotMarker (Marker v)      (Marker v2)      | v == v2 = True
      isNotMarker (E.Universal v) (E.Universal v2) | v == v2 = True
      isNotMarker _ _                                        = False
  in Context n' ctx'

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

allVars :: [Element t sa a v] -> [v]
allVars ctx = ctx >>= go where
  go (E.Solved v _) = [v]
  go (E.Ann v _) = [v]
  go (E.Existential v) = [v]
  go (E.Universal v) = [v]
  go (E.Marker v) = [v]

-- TODO: I suspect this can get away with just examining first few elements
-- perhaps up to first marker
currentVar :: Ord v => [Element t sa a v] -> Maybe v
currentVar ctx | null ctx  = Nothing
currentVar ctx | otherwise = Just $ minimum (allVars ctx)

-- | Check that the type is well formed wrt the given `Context`
wellformedType :: Eq v => Context t sa a (V.Var v) -> Type t' c k v -> Bool
wellformedType c t = case t of
  Unit -> True
  T.Universal v -> v `elem` universals c
  T.Existential v -> v `elem` existentials c
  Arrow i o -> wellformedType c i && wellformedType c o
  T.Ann t' _ -> wellformedType c t'
  Constrain t' _ -> wellformedType c t'
  Forall t' -> let (v,ctx2) = extendUniversal c
               in wellformedType ctx2 (subst1 t' (T.Universal v))

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

-- invariant is that both input types will have been fully freshened
-- before being passed to apply
apply :: Eq v => TContext t c k v -> Polytype c k v -> Polytype c k v
apply ctx t = case t of
  T.Universal _ -> t
  Unit -> t
  T.Existential v -> maybe t (apply ctx) (lookup v (solved ctx))
  T.Arrow i o -> T.Arrow (apply ctx i) (apply ctx o)
  T.Ann v k -> T.Ann (apply ctx v) k
  T.Constrain v c -> T.Constrain (apply ctx v) c
  T.Forall v -> T.Forall (apply ctx v)

type TContext t c k v = Context t (Polytype c k v) (Monotype c k v) (V.Var v)

type Note = String

note :: String -> Note
note = id

-- | `subtype ctx t1 t2` returns successfully if `t1` is a subtype of `t2`.
-- This may have the effect of altering the context.
subtype :: (Ord v, Show c, Show k, Show v)
        => TContext Incomplete c k v
        -> Polytype c k v
        -> Polytype c k v
        -> Either Note (TContext Incomplete c k v)
subtype ctx = go where
  go Unit Unit = pure ctx
  go t1@(T.Universal v1) t2@(T.Universal v2)
    | v1 == v2 && wellformedType ctx t1 && wellformedType ctx t2
    = pure ctx
  go t1@(T.Existential v1) t2@(T.Existential v2)
    | v1 == v2 && wellformedType ctx t1 && wellformedType ctx t2
    = pure ctx
  go (T.Arrow i1 o1) (T.Arrow i2 o2) = do
    ctx' <- subtype ctx i1 i2
    subtype ctx' (apply ctx' o1) (apply ctx' o2)
  go (T.Forall t) t2 =
    let (v, ctx') = extendMarker ctx
        t' = subst1 t (T.Existential v)
    in retract (E.Marker v) <$> subtype ctx' (apply ctx' t') t2
  go t (T.Forall t2) =
    let (v, ctx') = extendUniversal ctx
        t2' = subst1 t2 (T.Universal v)
    in retract (E.Universal v) <$> subtype ctx' t t2'
  -- need to add extra arg to Forall, so we track where var is bound
  --go (T.Existential v) t
  --  | v `elem` existentials ctx && not
  go t1 t2 = Left $ note "not a subtype " ++ show t1 ++ " " ++ show t2


