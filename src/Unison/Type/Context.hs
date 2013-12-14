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
import Unison.Type.Note

-- | An ordered algorithmic context
-- Context variables will be negative, while 'normal' DeBruijn
-- will be positive, so we don't generally need to worry about
-- getting accidental collisions when applying a context to
-- a type
data Context l c = Context V.Var [Element l c]

empty :: Context l c
empty = Context bound0 []

bound0 = V.decr V.bound1

context :: [Element l c] -> Context l c
context xs =
  let ctx = reverse xs
      v = fromMaybe bound0 (currentVar ctx)
  in Context v ctx

append :: Context l c -> Context l c -> Context l c
append (Context n1 ctx1) (Context n2 ctx2) =
  let n12 = V.minv n1 n2
  in Context n12 (ctx2 ++ ctx1)

fresh :: Context l c -> V.Var
fresh (Context n _) = V.decr n

fresh3 :: Context l c -> (V.Var, V.Var, V.Var)
fresh3 (Context n _) = (a,b,c) where
  a = V.decr n
  b = fresh' a
  c = fresh' b

fresh' :: V.Var -> V.Var
fresh' = V.decr

-- | Add an element onto the end of this `Context`
extend :: Element l c -> Context l c -> Context l c
extend e ctx = ctx `append` context [e]

-- | Extend this `Context` with a single universally quantified variable,
-- guaranteed to be fresh
extendUniversal :: Context l c -> (V.Var, Context l c)
extendUniversal (Context n ctx) =
  let v = V.decr n in (v, Context v (E.Universal n : ctx))

-- | Extend this `Context` with a single existentially quantified variable,
-- guaranteed to be fresh
extendExistential :: Context l c -> (V.Var, Context l c)
extendExistential (Context n ctx) =
  let v = V.decr n in (v, Context v (E.Universal n : ctx))

-- | Extend this `Context` with a marker variable, guaranteed to be fresh
extendMarker :: Context l c -> (V.Var, Context l c)
extendMarker (Context n ctx) =
  let v = V.decr n in (v, Context v ([E.Existential v, E.Marker v] ++ ctx))

-- | Delete up to and including the given `Element`
retract :: Element l c -> Context l c -> Context l c
retract m (Context _ ctx) =
  let ctx' = tail (dropWhile (go m) ctx)
      n' = fromMaybe bound0 (currentVar ctx') -- ok to recycle our variable supply
      go (Marker v)      (Marker v2)          | v == v2 = False
      go (E.Universal v) (E.Universal v2)     | v == v2 = False
      go (E.Existential v) (E.Existential v2) | v == v2 = False
      go _ _                                            = True
  in Context n' ctx'

universals :: Context l c -> [V.Var]
universals (Context _ ctx) = [v | E.Universal v <- ctx]

markers :: Context l c -> [V.Var]
markers (Context _ ctx) = [v | Marker v <- ctx]

existentials :: Context l c -> [V.Var]
existentials (Context _ ctx) = ctx >>= go where
  go (E.Existential v) = [v]
  go (E.Solved v _) = [v]
  go _ = []

solved :: Context l c -> [(V.Var, Monotype l c)]
solved (Context _ ctx) = [(v, sa) | Solved v sa <- ctx]

unsolved :: Context l c -> [V.Var]
unsolved (Context _ ctx) = [v | E.Existential v <- ctx]

replace :: Element l c -> Context l c -> Context l c -> Context l c
replace e focus ctx = let (l,r) = breakAt e ctx in l `append` focus `append` r

breakAt :: Element l c -> Context l c -> (Context l c, Context l c)
breakAt m (Context _ xs) =
  let (r, l) = break (=== m) xs
  in (context (drop 1 l), context r)

-- | ordered Γ α β = True <=> Γ[α^][β^]
ordered :: Context l c -> V.Var -> V.Var -> Bool
ordered ctx v v2 = v `elem` existentials (retract (E.Existential v2) ctx)

-- | solve (ΓL,α^,ΓR) α τ = (ΓL,α = τ,ΓR)
-- If the given existential variable exists in the context,
-- we solve it to the given monotype, otherwise return `Nothing`
solve :: Context l c -> V.Var -> Monotype l c -> Maybe (Context l c)
solve ctx v t | wellformedType ctxL (getPolytype t) = Just ctx'
              | otherwise                           = Nothing
    where (ctxL,ctxR) = breakAt (E.Existential v) ctx
          ctx' = ctxL `append` context [E.Solved v t] `append` ctxR

bindings :: Context l c -> [(V.Var, Type l c)]
bindings (Context _ ctx) = [(v,a) | E.Ann v a <- ctx]

lookupType :: Context l c -> V.Var -> Maybe (Type l c)
lookupType ctx v = lookup v (bindings ctx)

vars :: Context l c -> [V.Var]
vars = fmap fst . bindings

allVars :: [Element l c] -> [V.Var]
allVars ctx = ctx >>= go where
  go (E.Solved v _) = [v]
  go (E.Ann v _) = [v]
  go (E.Existential v) = [v]
  go (E.Universal v) = [v]
  go (E.Marker v) = [v]

-- TODO: I suspect this can get away with just examining first few elements
-- perhaps up to first marker
currentVar :: [Element l c] -> Maybe V.Var
currentVar ctx | L.null ctx  = Nothing
currentVar ctx | otherwise = Just $ minimum (allVars ctx)

-- | Check that the type is well formed wrt the given `Context`
wellformedType :: Context l c -> Type l c -> Bool
wellformedType c t = case t of
  T.Unit _ -> True
  T.Universal v -> v `elem` universals c
  T.Existential v -> v `elem` existentials c
  T.Arrow i o -> wellformedType c i && wellformedType c o
  T.Ann t' _ -> wellformedType c t'
  T.Constrain t' _ -> wellformedType c t'
  T.Forall _ t' ->
    let (v,ctx2) = extendUniversal c
    in wellformedType ctx2 (subst1 t' (T.Universal v))

-- | Check that the context is well formed, namely that
-- there are no circular variable references, and any types
-- mentioned in either `Ann` or `Solved` elements must be
-- wellformed with respect to the prefix of the context
-- leading up to these elements.
wellformed :: Context l c -> Bool
wellformed ctx = all go (zipTail ctx) where
  go (E.Universal v, ctx') = v `notElem` universals ctx'
  go (E.Existential v, ctx') = v `notElem` existentials ctx'
  go (Solved v sa, ctx') = v `notElem` existentials ctx' && wellformedType ctx' (getPolytype sa)
  go (E.Ann v t, ctx') = v `notElem` vars ctx' && wellformedType ctx' t
  go (Marker v, ctx') = v `notElem` vars ctx' && v `notElem` existentials ctx'

zipTail :: Context l c -> [(Element l c, Context l c)]
zipTail (Context n ctx) = zip ctx (map (Context n) $ tail (tails ctx))

-- invariant is that both input types will have been fully freshened
-- before being passed to apply
apply :: Context l c -> Type l c -> Type l c
apply ctx t = case t of
  T.Universal _ -> t
  T.Unit _ -> t
  T.Existential v ->
    maybe t (\(Monotype t') -> apply ctx t') (lookup v (solved ctx))
  T.Arrow i o -> T.Arrow (apply ctx i) (apply ctx o)
  T.Ann v k -> T.Ann (apply ctx v) k
  T.Constrain v c -> T.Constrain (apply ctx v) c
  T.Forall v t' -> T.Forall v (apply ctx t')

-- | `subtype ctx t1 t2` returns successfully if `t1` is a subtype of `t2`.
-- This may have the effect of altering the context.
subtype :: Eq l => Context l c -> Type l c -> Type l c -> Either Note (Context l c)
subtype ctx = go where -- Rules from figure 9
  go (Unit l) (Unit l2) | l == l2 = pure ctx -- `Unit`
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
  go _ _ = Left $ note "not a subtype"

-- | Instantiate the given existential such that it is
-- a subtype of the given type, updating the context
-- in the process.
instantiateL :: Context l c -> V.Var -> Type l c -> Either Note (Context l c)
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
instantiateR :: Context l c -> Type l c -> V.Var -> Either Note (Context l c)
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
-- updating the context in the process. Parameterized on
-- a function for synthesizing the type of a literal, `l`.
check :: Eq l'
      => (l -> l')
      -> Context l' c
      -> Term l (Type l' c)
      -> Type l' c
      -> Either Note (Context l' c)
check synthLit ctx e t | wellformedType ctx t = go e t where
  go (Term.Lit l) (T.Unit l') | synthLit l == l' = pure ctx -- 1I
  go _ (T.Forall x body) = -- ForallI
    let (x', ctx') = extendUniversal ctx
    in retract (E.Universal x') <$> check synthLit ctx' e (T.subst body x (T.Universal x'))
  go (Term.Lam body) (T.Arrow i o) = -- =>I
    let x' = fresh ctx
        v = Term.Var x'
        ctx' = extend (E.Ann x' i) ctx
        body' = Term.subst1 body v
    in retract (E.Ann x' i) <$> check synthLit ctx' body' o
  go _ _ = do -- Sub
    (a, ctx') <- synthesize synthLit ctx e
    subtype ctx' (apply ctx' a) (apply ctx' t)
check _ _ _ _ = Left $ note "type not well formed wrt context"

-- | Synthesize the type of the given term, updating the context
-- in the process. Parameterized on a function for synthesizing
-- the type of a literal, `l`.
synthesize :: Eq l'
           => (l -> l')
           -> Context l' c
           -> Term l (Type l' c)
           -> Either Note (Type l' c, Context l' c)
synthesize synthLit ctx e = go e where
  go (Term.Var v) = case lookupType ctx v of -- Var
    Nothing -> Left $ note "type not in scope"
    Just t -> pure (t, ctx)
  go (Term.Ann e' t) = (,) t <$> check synthLit ctx e' t -- Anno
  go (Term.Lit l) = pure (T.Unit $ synthLit l, ctx) -- 1I=>
  go (Term.App f arg) = do -- ->E
    (ft, ctx') <- synthesize synthLit ctx f
    synthesizeApp synthLit ctx' (apply ctx' ft) arg
  go (Term.Lam body) = -- ->I=> (Full Damas Milner rule)
    let (arg, i, o) = fresh3 ctx
        ctxTl = context [E.Marker i, E.Existential i, E.Existential o,
                         E.Ann arg (T.Existential i)]
        freshVars = tail $ iterate fresh' o
    in do
      (ctx1, ctx2) <- breakAt (E.Marker i) <$>
        check synthLit (ctx `append` ctxTl)
                       (Term.subst1 body (Term.Var arg))
                       (T.Existential o)
      pure $ let
        ft = apply ctx2 (T.Arrow (T.Existential i) (T.Existential o))
        existentials = unsolved ctx2
        universals = take (length existentials) freshVars
        ft' = foldr go ft (zip (map T.Universal universals) existentials)
        go (t,v) typ = T.subst typ v t
        ft2 = foldr T.Forall ft' universals
        in (ft2, ctx1)

-- | Synthesize the type of the given term, `arg` given that a function of
-- the given type `ft` is being applied to `arg`. Update the conext in
-- the process.
synthesizeApp :: Eq l'
              => (l -> l')
              -> Context l' c
              -> Type l' c
              -> Term l (Type l' c)
              -> Either Note (Type l' c, Context l' c)
synthesizeApp synthLit ctx ft arg = go ft where
  go (T.Forall x body) = let x' = fresh ctx -- Forall1App
    in synthesizeApp synthLit
                     (ctx `append` context [E.Existential x'])
                     (T.subst body x (T.Existential x'))
                     arg
  go (T.Arrow i o) = (,) o <$> check synthLit ctx arg i -- ->App
  go (T.Existential a) = -- a^App
    let i = fresh ctx
        o = fresh' i
        soln = Monotype (T.Arrow (T.Existential i) (T.Existential o))
        ctxMid = context [E.Existential o, E.Existential i, E.Solved a soln]
    in (,) (T.Existential o) <$>
      check synthLit (replace (E.Existential a) ctxMid ctx)
                      arg
                      (T.Existential i)
  go _ = Left $ note "unable to synthesize type of application"

