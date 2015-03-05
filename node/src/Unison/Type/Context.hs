{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

-- | The Unison language typechecker
module Unison.Type.Context (context, subtype, synthesizeClosed) where

import Control.Applicative
import Data.Traversable
import Data.List as L
import Data.Maybe
import qualified Data.Set as S
import Unison.Syntax.Type as T
import qualified Unison.Syntax.Term as Term
import Unison.Syntax.Term (Term)
import Unison.Syntax.Var as V
import Unison.Type.Context.Element as E
import Unison.Note as N

-- | An ordered algorithmic context
-- Context variables will be negative, while 'normal' DeBruijn
-- will be positive, so we don't generally need to worry about
-- getting accidental collisions when applying a context to
-- a type
data Context = Context V.Var [Element]

instance Show Context where
  show (Context n es) = "Γ " ++ show n ++ ".\n  " ++ (intercalate "\n  " . map show) (reverse es)

bound0 :: V.Var
bound0 = V.decr V.bound1

context :: [Element] -> Context
context xs =
  let ctx = reverse xs
      v = fromMaybe bound0 (currentVar ctx)
  in Context v ctx

append :: Context -> Context -> Context
append (Context n1 ctx1) (Context n2 ctx2) =
  let n12 = V.minv n1 n2
  in Context n12 (ctx2 ++ ctx1)

fresh :: Context -> V.Var
fresh (Context n _) = V.decr n

fresh3 :: Context -> (V.Var, V.Var, V.Var)
fresh3 (Context n _) = (a,b,c) where
  a = V.decr n
  b = fresh' a
  c = fresh' b

fresh' :: V.Var -> V.Var
fresh' = V.decr

-- | Add an element onto the end of this `Context`
extend :: Element -> Context -> Context
extend e ctx = ctx `append` context [e]

-- | Extend this `Context` with a single universally quantified variable,
-- guaranteed to be fresh
extendUniversal :: Context -> (V.Var, Context)
extendUniversal (Context n ctx) =
  let v = V.decr n in (v, Context v (E.Universal v : ctx))

-- | Extend this `Context` with a marker variable, guaranteed to be fresh
extendMarker :: Context -> (V.Var, Context)
extendMarker (Context n ctx) =
  let v = V.decr n in (v, Context v ([E.Existential v, E.Marker v] ++ ctx))

-- | Delete up to and including the given `Element`
-- returns @Left@ if the element is not found
retract :: Element -> Context -> Either Note Context
retract m c@(Context _ ctx) =
  let maybeTail [] = Left $ note ("unable to retract: " ++ show m)
      maybeTail (_:t) = Right t
      ctx' = maybeTail (dropWhile (/= m) ctx)
      n' = case ctx' of
        Left _ -> bound0
        Right c -> fromMaybe bound0 (currentVar c) -- ok to recycle var supply
  in scope ("context: "++show c) (Context n' <$> ctx')

retract' :: Element -> Context -> Context
retract' e ctx = case retract e ctx of
  Left _ -> context []
  Right ctx -> ctx

universals :: Context -> [V.Var]
universals (Context _ ctx) = [v | E.Universal v <- ctx]

existentials :: Context -> [V.Var]
existentials (Context _ ctx) = ctx >>= go where
  go (E.Existential v) = [v]
  go (E.Solved v _) = [v]
  go _ = []

solved :: Context -> [(V.Var, Monotype)]
solved (Context _ ctx) = [(v, sa) | Solved v sa <- ctx]

unsolved :: Context -> [V.Var]
unsolved (Context _ ctx) = [v | E.Existential v <- ctx]

replace :: Element -> Context -> Context -> Context
replace e focus ctx = let (l,r) = breakAt e ctx in l `append` focus `append` r

breakAt :: Element -> Context -> (Context, Context)
breakAt m (Context _ xs) =
  let (r, l) = break (=== m) xs
  in (context (reverse $ drop 1 l), context $ reverse r)

-- | ordered Γ α β = True <=> Γ[α^][β^]
ordered :: Context -> V.Var -> V.Var -> Bool
ordered ctx v v2 = v `elem` existentials (retract' (E.Existential v2) ctx)

-- | solve (ΓL,α^,ΓR) α τ = (ΓL,α = τ,ΓR)
-- If the given existential variable exists in the context,
-- we solve it to the given monotype, otherwise return `Nothing`
solve :: Context -> V.Var -> Monotype -> Maybe Context
solve ctx v t | wellformedType ctxL (getPolytype t) = Just ctx'
              | otherwise                           = Nothing
    where (ctxL,ctxR) = breakAt (E.Existential v) ctx
          ctx' = ctxL `append` context [E.Solved v t] `append` ctxR

bindings :: Context -> [(V.Var, Type)]
bindings (Context _ ctx) = [(v,a) | E.Ann v a <- ctx]

lookupType :: Context -> V.Var -> Maybe Type
lookupType ctx v = lookup v (bindings ctx)

vars :: Context -> [V.Var]
vars = fmap fst . bindings

allVars :: [Element] -> [V.Var]
allVars ctx = ctx >>= go where
  go (E.Solved v _) = [v]
  go (E.Ann v _) = [v]
  go (E.Existential v) = [v]
  go (E.Universal v) = [v]
  go (E.Marker v) = [v]

-- TODO: I suspect this can get away with just examining first few elements
-- perhaps up to first marker
currentVar :: [Element] -> Maybe V.Var
currentVar ctx | L.null ctx  = Nothing
currentVar ctx | otherwise = Just $ minimum (allVars ctx)

-- | Check that the type is well formed wrt the given `Context`
wellformedType :: Context -> Type -> Bool
wellformedType c t = wellformed c && case t of
  T.Unit _ -> True
  T.Universal v -> v `elem` universals c
  T.Existential v -> v `elem` existentials c
  T.Arrow i o -> wellformedType c i && wellformedType c o
  T.Ann t' _ -> wellformedType c t'
  T.Constrain t' _ -> wellformedType c t'
  T.Forall v t' ->
    let (v',ctx2) = extendUniversal c
    in wellformedType ctx2 (subst t' v (T.Universal v'))

-- | Check that the context is well formed, namely that
-- there are no circular variable references, and any types
-- mentioned in either `Ann` or `Solved` elements must be
-- wellformed with respect to the prefix of the context
-- leading up to these elements.
wellformed :: Context -> Bool
wellformed ctx = all go (zipTail ctx) where
  go (E.Universal v, ctx') = v `notElem` universals ctx'
  go (E.Existential v, ctx') = v `notElem` existentials ctx'
  go (Solved v sa, ctx') = v `notElem` existentials ctx' && wellformedType ctx' (getPolytype sa)
  go (E.Ann v t, ctx') = v `notElem` vars ctx' && wellformedType ctx' t
  go (Marker v, ctx') = v `notElem` vars ctx' && v `notElem` existentials ctx'

zipTail :: Context -> [(Element, Context)]
zipTail (Context n ctx) = zip ctx (map (Context n) $ tail (tails ctx))

-- invariant is that both input types will have been fully freshened
-- before being passed to apply
apply :: Context -> Type -> Type
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
subtype :: Context -> Type -> Type -> Either Note Context
subtype ctx tx ty = scope (show tx++" <: "++show ty) (go tx ty) where -- Rules from figure 9
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
  go (T.Forall v t) t2 = scope "forall (L)" $
    let (v', ctx') = extendMarker ctx
        t' = subst t v (T.Existential v')
    in scope (show t') $
       subtype ctx' (apply ctx' t') t2 >>= retract (E.Marker v')
  go t (T.Forall v t2) = scope "forall (R)" $
    let (v', ctx') = extendUniversal ctx
        t2' = subst t2 v (T.Universal v')
    in subtype ctx' t t2' >>= retract (E.Universal v')
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
instantiateL :: Context -> V.Var -> Type -> Either Note Context
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
      in instantiateL ctx' v (T.subst body x (T.Universal v'))
         >>= retract (E.Universal v')
    _ -> Left $ note "could not instantiate left"

-- | Instantiate the given existential such that it is
-- a supertype of the given type, updating the context
-- in the process.
instantiateR :: Context -> Type -> V.Var -> Either Note Context
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
      in
        instantiateR (ctx `append` context [E.Marker x', E.Existential x'])
                     (T.subst body x (T.Existential x'))
                     v
        >>= retract (E.Marker x')
    _ -> Left $ note "could not instantiate right"

-- | Check that under the given context, `e` has type `t`,
-- updating the context in the process.
check :: Context -> Term -> Type -> Either Note Context
check ctx e t | wellformedType ctx t = scope (show e ++ " : " ++ show t) $ go e t where
  go (Term.Lit l) _ = subtype ctx (synthLit l) t -- 1I
  go _ (T.Forall x body) = -- ForallI
    let (x', ctx') = extendUniversal ctx
    in check ctx' e (T.subst body x (T.Universal x'))
       >>= retract (E.Universal x')
  go fn@(Term.Lam _) (T.Arrow i o) = -- =>I
    let x' = fresh ctx
        v = Term.Var x'
        ctx' = extend (E.Ann x' i) ctx
        body' = Term.betaReduce (fn `Term.App` v)
    in check ctx' body' o >>= retract (E.Ann x' i)
  -- go Term.Blank _ = Right ctx -- possible hack to workaround lack of impredicative instantiation
  go _ _ = do -- Sub
    (a, ctx') <- synthesize ctx e
    subtype ctx' (apply ctx' a) (apply ctx' t)
check _ _ _ = Left $ note "type not well formed wrt context"

-- | Infer the type of a literal
synthLit :: Term.Literal -> Type
synthLit lit = T.Unit $ case lit of
  Term.Number _ -> T.Number
  Term.String _ -> T.String
  Term.Distance _ -> T.Distance

-- | Synthesize the type of the given term, updating the context in the process.
synthesize :: Context -> Term -> Either Note (Type, Context)
synthesize ctx e = scope ("infer: " ++ show e) $ go e where
  go (Term.Var v) = case lookupType ctx v of -- Var
    Nothing -> Left $ note "type not in scope"
    Just t -> pure (t, ctx)
  go Term.Blank = pure (T.forall1 $ \x -> x, ctx)
  go (Term.Ann (Term.Ref _) t) =
    pure (t, ctx) -- innermost Ref annotation assumed to be correctly provided by `synthesizeClosed`
  go (Term.Ref h) = Left . note $ "unannotated reference: " ++ show h
  go (Term.Ann e' t) = (,) t <$> check ctx e' t -- Anno
  go (Term.Lit l) = pure (synthLit l, ctx) -- 1I=>
  go (Term.Vector v) = pure (T.Unit T.Vector, ctx) -- todo, if vector is empty, return forall a . Vector a, otherwise take lub of elements in vec
  go (Term.App f arg) = do -- ->E
    (ft, ctx') <- synthesize ctx f
    synthesizeApp ctx' (apply ctx' ft) arg
  go fn@(Term.Lam _) = -- ->I=> (Full Damas Milner rule)
    let (arg, i, o) = fresh3 ctx
        ctxTl = context [E.Marker i, E.Existential i, E.Existential o,
                         E.Ann arg (T.Existential i)]
    in do
      ctx' <- check (ctx `append` ctxTl)
                    (Term.betaReduce $ fn `Term.App` Term.Var arg)
                    (T.Existential o)
      pure $ let
        (ctx1, ctx2) = breakAt (E.Marker i) ctx'
        -- unsolved existentials get generalized to universals
        ft = apply ctx2 (T.Arrow (T.Existential i) (T.Existential o))
        existentials' = unsolved ctx2
        ft2 = foldr gen ft existentials'
        gen e ft = T.forall1 $ \v -> subst ft e v
        in (ft2, ctx1)

-- | Synthesize the type of the given term, `arg` given that a function of
-- the given type `ft` is being applied to `arg`. Update the conext in
-- the process.
synthesizeApp :: Context -> Type -> Term -> Either Note (Type, Context)
synthesizeApp ctx ft arg = go ft where
  go (T.Forall x body) = let x' = fresh ctx -- Forall1App
    in synthesizeApp (ctx `append` context [E.Existential x'])
                     (T.subst body x (T.Existential x'))
                     arg
  go (T.Arrow i o) = (,) o <$> check ctx arg i -- ->App
  go (T.Existential a) = -- a^App
    let i = fresh ctx
        o = fresh' i
        soln = Monotype (T.Arrow (T.Existential i) (T.Existential o))
        ctxMid = context [E.Existential o, E.Existential i, E.Solved a soln]
    in (,) (T.Existential o) <$>
      check (replace (E.Existential a) ctxMid ctx)
                      arg
                      (T.Existential i)
  go _ = Left $ note "unable to synthesize type of application"

synthesizeClosed :: Applicative f => T.Env f -> Term -> Noted f Type
synthesizeClosed synthRef term = Noted $ synth <$> N.unnote (annotate term)
  where
    synth :: Either Note Term -> Either Note Type
    synth (Left e) = Left e
    synth (Right a) = go <$> synthesize (context []) a
    go (t, ctx) = apply ctx t
    annotate term' = case term' of
      Term.Ref h -> Term.Ann (Term.Ref h) <$> synthRef h
      Term.App f arg -> Term.App <$> annotate f <*> annotate arg
      Term.Ann body t -> Term.Ann <$> annotate body <*> pure t
      Term.Lam body -> Term.Lam <$> annotate body
      Term.Vector terms -> Term.Vector <$> traverse annotate terms
      _ -> pure term'
