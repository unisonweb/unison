{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Unison.Type.Context where

import Control.Applicative
import Data.List as L
import Data.Maybe
import qualified Data.Set as S
import Unison.Syntax.Type as T
import qualified Unison.Syntax.Term as Term
import Unison.Syntax.Term (Term)
import Unison.Syntax.Var as V
import Unison.Syntax.DeBruijn as D
import Unison.Type.Context.Element as E

-- | An ordered algorithmic context
-- Context variables will be negative, while 'normal' DeBruijn
-- will be positive, so we don't generally need to worry about
-- getting accidental collisions when applying a context to
-- a type
data Context sa a v = Context v [Element sa a v]

type TContext c k v =
  Context (Monotype c k (V.Var v)) (Type c k (V.Var v)) (V.Var v)

empty :: Context sa a (Var v)
empty = Context (Bound (DeBruijn 0)) []

context :: Ord v => [Element sa a (Var v)] -> Context sa a (Var v)
context xs = let ctx = reverse xs
                 v = fromMaybe (Bound (DeBruijn 0)) (currentVar ctx)
             in Context v ctx

append :: Context sa a (Var v) -> Context sa a (Var v) -> Context sa a (Var v)
append (Context n1 ctx1) (Context n2 ctx2) =
  let n12 = V.minv n1 n2
  in Context n12 (ctx2 ++ ctx1)

fresh :: TContext c k v -> Var v
fresh (Context n _) = V.decr n

fresh' :: Var v -> Var v
fresh' = V.decr

-- | Add an element onto the end of this `Context`
extend :: Ord v => Element sa a (Var v) -> Context sa a (Var v) -> Context sa a (Var v)
extend e ctx = ctx `append` context [e]

-- | Extend this `Context` with a single universally quantified variable,
-- guaranteed to be fresh
extendUniversal :: Context sa a (Var v) -> (Var v, Context sa a (Var v))
extendUniversal (Context n ctx) =
  let v = V.decr n in (v, Context v (E.Universal n : ctx))

-- | Extend this `Context` with a single existentially quantified variable,
-- guaranteed to be fresh
extendExistential :: Context sa a (Var v) -> (Var v, Context sa a (Var v))
extendExistential (Context n ctx) =
  let v = V.decr n in (v, Context v (E.Universal n : ctx))

-- | Extend this `Context` with a marker variable, guaranteed to be fresh
extendMarker :: Context sa a (Var v)
             -> (Var v, Context sa a (Var v))
extendMarker (Context n ctx) =
  let v = V.decr n in (v, Context v ([E.Existential v, E.Marker v] ++ ctx))

retract :: Ord v
        => Element sa a (Var v)
        -> Context sa a (Var v)
        -> Context sa a (Var v)
retract m (Context _ ctx) =
  let ctx' = tail (dropWhile (go m) ctx)
      n' = fromMaybe (Bound (DeBruijn 0))
                     (currentVar ctx') -- ok to recycle our variable supply
      go (Marker v)      (Marker v2)          | v == v2 = False
      go (E.Universal v) (E.Universal v2)     | v == v2 = False
      go (E.Existential v) (E.Existential v2) | v == v2 = False
      go _ _                                            = True
  in Context n' ctx'

universals :: Context sa a v -> [v]
universals (Context _ ctx) = [v | E.Universal v <- ctx]

markers :: Context sa a v -> [v]
markers (Context _ ctx) = [v | Marker v <- ctx]

existentials :: Context sa a v -> [v]
existentials (Context _ ctx) = ctx >>= go where
  go (E.Existential v) = [v]
  go (E.Solved v _) = [v]
  go _ = []

solved :: Context sa a v -> [(v, sa)]
solved (Context _ ctx) = [(v, sa) | Solved v sa <- ctx]

replace :: Ord v => TElement c k v -> TContext c k v -> TContext c k v -> TContext c k v
replace e focus ctx = let (l,r) = breakAt e ctx in l `append` focus `append` r

breakAt :: Ord v => TElement c k v -> TContext c k v -> (TContext c k v, TContext c k v)
breakAt m (Context _ xs) =
  let (r, l) = break (=== m) xs
  in (context (drop 1 l), context r)

-- | ordered Γ α β = True <=> Γ[α^][β^]
ordered :: Ord v => TContext c k v -> V.Var v -> V.Var v -> Bool
ordered ctx v v2 = v `elem` existentials (retract (E.Existential v2) ctx)

-- | solve (ΓL,α^,ΓR) α τ = (ΓL,α = τ,ΓR)
-- If the given existential variable exists in the context,
-- we solve it to the given monotype, otherwise return `Nothing`
solve :: Ord v => TContext c k v -> V.Var v -> Monotype c k (V.Var v) -> Maybe (TContext c k v)
solve ctx v t | wellformedType ctxL (getPolytype t) = Just ctx'
              | otherwise                           = Nothing
    where (ctxL,ctxR) = breakAt (E.Existential v) ctx
          ctx' = ctxL `append` context [E.Solved v t] `append` ctxR

bindings :: Context sa a v -> [(v, a)]
bindings (Context _ ctx) = [(v,a) | E.Ann v a <- ctx]

vars :: Context sa a v -> [v]
vars = fmap fst . bindings

allVars :: [Element sa a v] -> [v]
allVars ctx = ctx >>= go where
  go (E.Solved v _) = [v]
  go (E.Ann v _) = [v]
  go (E.Existential v) = [v]
  go (E.Universal v) = [v]
  go (E.Marker v) = [v]

-- TODO: I suspect this can get away with just examining first few elements
-- perhaps up to first marker
currentVar :: Ord v => [Element sa a v] -> Maybe v
currentVar ctx | L.null ctx  = Nothing
currentVar ctx | otherwise = Just $ minimum (allVars ctx)

-- | Check that the type is well formed wrt the given `Context`
wellformedType :: Eq v => Context sa a (V.Var v) -> Type c k (V.Var v) -> Bool
wellformedType c t = case t of
  Unit -> True
  T.Universal v -> v `elem` universals c
  T.Existential v -> v `elem` existentials c
  Arrow i o -> wellformedType c i && wellformedType c o
  T.Ann t' _ -> wellformedType c t'
  Constrain t' _ -> wellformedType c t'
  Forall _ t' ->
    let (v,ctx2) = extendUniversal c
    in wellformedType ctx2 (subst1 t' (T.Universal v))

-- | Check that the context is well formed, namely that
-- there are no circular variable references, and any types
-- mentioned in either `Ann` or `Solved` elements must be
-- wellformed with respect to the prefix of the context
-- leading up to these elements.
wellformed :: Eq v => TContext c k v -> Bool
wellformed ctx = all go (zipTail ctx) where
  go (E.Universal v, ctx') = v `notElem` universals ctx'
  go (E.Existential v, ctx') = v `notElem` existentials ctx'
  go (Solved v sa, ctx') = v `notElem` existentials ctx' && wellformedType ctx' (getPolytype sa)
  go (E.Ann v t, ctx') = v `notElem` vars ctx' && wellformedType ctx' t
  go (Marker v, ctx') = v `notElem` vars ctx' && v `notElem` existentials ctx'

zipTail :: Context sa a v -> [(Element sa a v, Context sa a v)]
zipTail (Context n ctx) = zip ctx (map (Context n) $ tail (tails ctx))

-- invariant is that both input types will have been fully freshened
-- before being passed to apply
apply :: Eq v
      => Context (Monotype c k v) a v
      -> Type c k v
      -> Type c k v
apply ctx t = case t of
  T.Universal _ -> t
  Unit -> t
  T.Existential v ->
    maybe t (\(Monotype t') -> apply ctx t') (lookup v (solved ctx))
  T.Arrow i o -> T.Arrow (apply ctx i) (apply ctx o)
  T.Ann v k -> T.Ann (apply ctx v) k
  T.Constrain v c -> T.Constrain (apply ctx v) c
  T.Forall v t' -> T.Forall v (apply ctx t')

type Note = [String]

note :: String -> Note
note s = [s]

scope :: String -> Either Note a -> Either Note a
scope s (Left stack) = Left (s : stack)
scope s e = e

-- | `subtype ctx t1 t2` returns successfully if `t1` is a subtype of `t2`.
-- This may have the effect of altering the context.
subtype :: (Ord v, Show c, Show k, Show v)
        => TContext c k v
        -> Type c k (V.Var v)
        -> Type c k (V.Var v)
        -> Either Note (TContext c k v)
subtype ctx = go where -- Rules from figure 9
  go Unit Unit = pure ctx -- `Unit`
  go t1@(T.Universal v1) t2@(T.Universal v2) -- `Var`
    | v1 == v2 && wellformedType ctx t1 && wellformedType ctx t2
    = pure ctx
  go t1@(T.Existential v1) t2@(T.Existential v2) -- `Exvar`
    | v1 == v2 && wellformedType ctx t1 && wellformedType ctx t2
    = pure ctx
  go (T.Arrow i1 o1) (T.Arrow i2 o2) = do -- `-->`
    ctx' <- subtype ctx i1 i2
    subtype ctx' (apply ctx' o1) (apply ctx' o2)
  go (T.Forall _ t) t2 = -- `forall (L)`
    let (v, ctx') = extendMarker ctx
        t' = subst1 t (T.Existential v)
    in retract (E.Marker v) <$> subtype ctx' (apply ctx' t') t2
  go t (T.Forall _ t2) = -- `forall (R)`
    let (v, ctx') = extendUniversal ctx
        t2' = subst1 t2 (T.Universal v)
    in retract (E.Universal v) <$> subtype ctx' t t2'
  go (T.Existential v) t -- `InstantiateL`
    | v `elem` existentials ctx && S.notMember v (freeVars t) =
    instantiateL ctx v t
  go t (T.Existential v) -- `InstantiateR`
    | v `elem` existentials ctx && S.notMember v (freeVars t) =
    instantiateR ctx t v
  go t1 t2 = Left $ note ("not a subtype " ++ show t1 ++ " " ++ show t2)

-- | Instantiate the given existential such that it is
-- a subtype of the given type, updating the context
-- in the process.
instantiateL :: Ord v
             => TContext c k v
             -> Var v
             -> Type c k (V.Var v)
             -> Either Note (TContext c k v)
instantiateL ctx v t = case monotype t >>= solve ctx v of
  Just ctx' -> pure ctx' -- InstLSolve
  Nothing -> case t of
    T.Existential v2 | ordered ctx v v2 -> -- InstLReach (both are existential, set v2 = v)
      maybe (Left $ note "InstLReach failed") pure $
        solve ctx v2 (Monotype (T.Existential v))
    T.Arrow i o -> -- InstLArr
      let i' = fresh ctx
          o' = fresh' i'
          s = E.Solved v (Monotype (T.Arrow (T.Existential i') (T.Existential o')))
      in do
        ctx' <- instantiateR (replace (E.Existential v) (context [E.Existential o', E.Existential i', s]) ctx)
                             i
                             i'
        instantiateL ctx' o' (apply ctx' o)
    T.Forall x body -> -- InstLIIL
      let (v', ctx') = extendUniversal ctx
      in retract (E.Universal v') <$>
         instantiateL ctx' v (T.subst body x (T.Universal v'))
    _ -> Left $ note "could not instantiate left"

-- | Instantiate the given existential such that it is
-- a subtype of the given type, updating the context
-- in the process.
instantiateR :: Ord v
             => TContext c k v
             -> Type c k (V.Var v)
             -> Var v
             -> Either Note (TContext c k v)
instantiateR ctx t v = case monotype t >>= solve ctx v of
  Just ctx' -> pure ctx' -- InstRSolve
  Nothing -> case t of
    T.Existential v2 | ordered ctx v v2 -> -- InstRReach (both are existential, set v2 = v)
      maybe (Left $ note "InstRReach failed") pure $
        solve ctx v2 (Monotype (T.Existential v))
    T.Arrow i o -> -- InstRArrow
      let i' = fresh ctx
          o' = fresh' i'
          s = E.Solved v (Monotype (T.Arrow (T.Existential i') (T.Existential o')))
      in do
        ctx' <- instantiateL (replace (E.Existential v) (context [E.Existential o', E.Existential i', s]) ctx)
                             i'
                             i
        instantiateR ctx' (apply ctx' o) o'
    T.Forall x body -> -- InstRAIIL
      let x' = fresh ctx
      in retract (E.Marker x') <$>
        instantiateR (ctx `append` context [E.Marker x', E.Existential x'])
                     (T.subst body x (T.Existential x'))
                     v
    _ -> Left $ note "could not instantiate right"

-- | Check that under the given context, `e` has type `t`,
-- updating the context in the process
check :: (Ord v, Eq k)
      => TContext c k v
      -> Term k (Type c k (V.Var v)) v
      -> Type c k (V.Var v)
      -> Either Note (TContext c k v)
check ctx _ t | not (wellformedType ctx t) = Left $ note "type not well formed wrt context"
check ctx _ _ = error "todo"
