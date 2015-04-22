{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | The Unison language typechecker
module Unison.Typechecker.A_Context where
-- (context, subtype, synthesizeClosed) where

import Control.Applicative
import Data.List
import Data.Maybe
import Data.Set (Set)
import Data.Traversable
import Unison.Note (Note)
import Unison.Term (Term)
import Unison.A_Type (Type, Monotype)
import qualified Unison.ABT as ABT
import qualified Data.Foldable as Foldable
import qualified Unison.Note as Note
import qualified Data.Set as Set
import qualified Unison.Term as Term
import qualified Unison.A_Type as Type

-- | Elements of an algorithmic context
data Element
  = Universal ABT.V       -- | ^ `v` is universally quantified
  | Existential ABT.V     -- | ^ `v` existential and unsolved
  | Solved ABT.V Monotype -- | ^ `v` is solved to some monotype
  | Ann ABT.V Type        -- | ^ `v` has type `a`, which may be quantified
  | Marker ABT.V          -- | ^ used for scoping
  deriving Eq

instance Show Element where
  show (Universal v) = show v
  show (Existential v) = "'"++show v
  show (Solved v t) = "'"++show v++" = "++show t
  show (Ann v t) = show v++" : "++show t
  show (Marker v) = "|"++show v++"|"

varUsedBy :: Element -> ABT.V
varUsedBy e = case e of
  Existential v -> v
  Universal v -> v
  Marker v -> v
  Ann v _ -> v
  Solved v _ -> v

(===) :: Element -> Element -> Bool
Existential v === Existential v2 | v == v2 = True
Universal v   === Universal v2 | v == v2 = True
Marker v      === Marker v2 | v == v2 = True
_ === _ = False

-- | An ordered algorithmic context, with the set of used variables cached
-- Context !Int [Element]
data Context = Context (Set ABT.V) [Element]

instance Show Context where
  show (Context n es) = "Γ " ++ show n ++ ".\n  " ++ (intercalate "\n  " . map show) (reverse es)

v0 :: ABT.V
v0 = ABT.v' "#"

context :: [Element] -> Context
context xs =
  let ctx = reverse xs
  in Context (Set.fromList (map varUsedBy ctx)) ctx

append :: Context -> Context -> Context
append (Context n1 ctx1) (Context n2 ctx2) =
  Context (Set.union n1 n2) (ctx2 ++ ctx1)

fresh :: ABT.V -> Context -> ABT.V
fresh v (Context vs _) = ABT.fresh' vs v

fresh3 :: ABT.V -> ABT.V -> ABT.V -> Context -> (ABT.V, ABT.V, ABT.V)
fresh3 va vb vc ctx = (fresh va ctx, fresh vb ctx, fresh vc ctx)

-- | Add an element onto the end of this `Context`
extend :: Element -> Context -> Context
extend e ctx = ctx `append` context [e]

-- | Extend this `Context` with a single universally quantified variable,
-- guaranteed to be fresh
extendUniversal :: ABT.V -> Context -> (ABT.V, Context)
extendUniversal v ctx = case fresh v ctx of
  v -> (v, extend (Universal v) ctx)

-- | Extend this `Context` with a marker variable, guaranteed to be fresh
extendMarker :: ABT.V -> Context -> (ABT.V, Context)
extendMarker v ctx = case fresh v ctx of
  v -> (v, ctx `append` context [Marker v, Existential v])

-- | Delete from the end of this context up to and including
-- the given `Element`. Returns `Left` if the element is not found.
retract :: Element -> Context -> Either Note Context
retract m (Context _ ctx) =
  let maybeTail [] = Left $ Note.note ("unable to retract: " ++ show m)
      maybeTail (_:t) = Right t
      ctx' = maybeTail (dropWhile (/= m) ctx)
      vs = case ctx' of
        Left _ -> Set.empty
        Right ctx -> Set.fromList (map varUsedBy ctx)
  in (Context vs <$> ctx')

retract' :: Element -> Context -> Context
retract' e ctx = case retract e ctx of
  Left _ -> context []
  Right ctx -> ctx

universals :: Context -> [ABT.V]
universals (Context _ ctx) = [v | Universal v <- ctx]

existentials :: Context -> [ABT.V]
existentials (Context _ ctx) = ctx >>= go where
  go (Existential v) = [v]
  go (Solved v _) = [v]
  go _ = []

solved :: Context -> [(ABT.V, Monotype)]
solved (Context _ ctx) = [(v, sa) | Solved v sa <- ctx]

unsolved :: Context -> [ABT.V]
unsolved (Context _ ctx) = [v | Existential v <- ctx]

replace :: Element -> Context -> Context -> Context
replace e focus ctx = let (l,r) = breakAt e ctx in l `append` focus `append` r

breakAt :: Element -> Context -> (Context, Context)
breakAt m (Context _ xs) =
  let (r, l) = break (=== m) xs
  in (context (reverse $ drop 1 l), context $ reverse r)

-- | ordered Γ α β = True <=> Γ[α^][β^]
ordered :: Context -> ABT.V -> ABT.V -> Bool
ordered ctx v v2 = v `elem` existentials (retract' (Existential v2) ctx)

-- | Check that the context is well formed, namely that
-- there are no circular variable references, and any types
-- mentioned in either `Ann` or `Solved` elements must be
-- wellformed with respect to the prefix of the context
-- leading up to these elements.
wellformed :: Context -> Bool
wellformed ctx = all go (zipTail ctx) where
  go (Universal v, ctx') = v `notElem` universals ctx'
  go (Existential v, ctx') = v `notElem` existentials ctx'
  go (Solved v sa, ctx') = v `notElem` existentials ctx' && wellformedType ctx' (Type.getPolytype sa)
  go (Ann v t, ctx') = v `notElem` vars ctx' && wellformedType ctx' t
  go (Marker v, ctx') = v `notElem` vars ctx' && v `notElem` existentials ctx'

zipTail :: Context -> [(Element, Context)]
zipTail (Context n ctx) = zip ctx (map (Context n) $ tail (tails ctx))

-- | Check that the type is well formed wrt the given `Context`
wellformedType :: Context -> Type -> Bool
wellformedType c t = wellformed c && case t of
  Type.Existential' v -> v `elem` existentials c
  Type.Universal' v -> v `elem` universals c
  Type.Lit' _ -> True
  Type.Arrow' i o -> wellformedType c i && wellformedType c o
  Type.Ann' t' _ -> wellformedType c t'
  Type.App' x y -> wellformedType c x && wellformedType c y
  Type.Constrain' t' _ -> wellformedType c t'
  Type.Forall' v t' ->
    let (v',ctx2) = extendUniversal v c
    in wellformedType ctx2 (ABT.subst (ABT.var v') v t')
  _ -> error $ "Context.wellformedType - ill formed type - " ++ show t

bindings :: Context -> [(ABT.V, Type)]
bindings (Context _ ctx) = [(v,a) | Ann v a <- ctx]

lookupType :: Context -> ABT.V -> Maybe Type
lookupType ctx v = lookup v (bindings ctx)

vars :: Context -> [ABT.V]
vars = fmap fst . bindings

-- | solve (ΓL,α^,ΓR) α τ = (ΓL,α = τ,ΓR)
-- If the given existential variable exists in the context,
-- we solve it to the given monotype, otherwise return `Nothing`
solve :: Context -> ABT.V -> Monotype -> Maybe Context
solve ctx v t | wellformedType ctxL (Type.getPolytype t) = Just ctx'
              | otherwise                           = Nothing
    where (ctxL,ctxR) = breakAt (Existential v) ctx
          ctx' = ctxL `append` context [Solved v t] `append` ctxR

-- | Replace any existentials with their solution in the context
apply :: Context -> Type -> Type
apply ctx t = case t of
  Type.Universal' _ -> t
  Type.Lit' _ -> t
  Type.Existential' v ->
    maybe t (\(Type.Monotype t') -> apply ctx t') (lookup v (solved ctx))
  Type.Arrow' i o -> Type.arrow (apply ctx i) (apply ctx o)
  Type.App' x y -> Type.app (apply ctx x) (apply ctx y)
  Type.Ann' v k -> Type.ann (apply ctx v) k
  Type.Constrain' v c -> Type.constrain (apply ctx v) c
  Type.Forall' v t' -> Type.forall v (apply ctx t')
  _ -> error $ "Context.apply ill formed type - " ++ show t

-- todo: am here, should figure out what dependencies are among all these fns
-- uncomment them in order

{-
-- | `subtype ctx t1 t2` returns successfully if `t1` is a subtype of `t2`.
-- This may have the effect of altering the context.
subtype :: Context -> Type -> Type -> Either Note Context
subtype ctx tx ty = scope (show tx++" <: "++show ty) (go tx ty) where -- Rules from figure 9
  go (Type.Unit l) (Type.Unit l2) | l == l2 = pure ctx -- `Unit`
  go t1@(Type.Universal v1) t2@(Type.Universal v2) -- `Var`
    | v1 == v2 && wellformedType ctx t1 && wellformedType ctx t2
    = pure ctx
  go t1@(Type.Existential v1) t2@(Type.Existential v2) -- `Exvar`
    | v1 == v2 && wellformedType ctx t1 && wellformedType ctx t2
    = pure ctx
  go (Type.Arrow i1 o1) (Type.Arrow i2 o2) = do -- `-->`
    ctx' <- subtype ctx i1 i2
    subtype ctx' (apply ctx' o1) (apply ctx' o2)
  go (Type.App x1 y1) (Type.App x2 y2) = do -- analogue of `-->`
    ctx' <- subtype ctx x1 x2
    subtype ctx' (apply ctx' y1) (apply ctx' y2)
  go (Type.Forall v t) t2 = scope "forall (L)" $
    let (v', ctx') = extendMarker ctx
        t' = Type.subst t v (Type.Existential v')
    in scope (show t') $
       subtype ctx' (apply ctx' t') t2 >>= retract (Marker v')
  go t (Type.Forall v t2) = scope "forall (R)" $
    let (v', ctx') = extendUniversal ctx
        t2' = Type.subst t2 v (Type.Universal v')
    in subtype ctx' t t2' >>= retract (Universal v')
  go (Type.Existential v) t -- `InstantiateL`
    | v `elem` existentials ctx && S.notMember v (Type.freeVars t) =
    instantiateL ctx v t
  go t (Type.Existential v) -- `InstantiateR`
    | v `elem` existentials ctx && S.notMember v (Type.freeVars t) =
    instantiateR ctx t v
  go _ _ = Left $ note "not a subtype"

-- | Instantiate the given existential such that it is
-- a subtype of the given type, updating the context
-- in the process.
instantiateL :: Context -> ABT.V -> Type -> Either Note Context
instantiateL ctx v t = case Type.monotype t >>= solve ctx v of
  Just ctx' -> pure ctx' -- InstLSolve
  Nothing -> case t of
    Type.Existential v2 | ordered ctx v v2 -> -- InstLReach (both are existential, set v2 = v)
      maybe (Left $ note "InstLReach failed") pure $
        solve ctx v2 (Type.Monotype (Type.Existential v))
    Type.Arrow i o -> -- InstLArr
      let i' = fresh ctx
          o' = fresh' i'
          s = Solved v (Type.Monotype (Type.Arrow (Type.Existential i') (Type.Existential o')))
      in do
        ctx' <- instantiateR (replace (Existential v) (context [Existential o', Existential i', s]) ctx)
                             i
                             i'
        instantiateL ctx' o' (apply ctx' o)
    Type.App x y -> -- analogue of InstLArr
      let x' = fresh ctx
          y' = fresh' x'
          s = Solved v (Type.Monotype (Type.App (Type.Existential x') (Type.Existential y')))
          ctx0 = replace (Existential v)
                         (context [Existential y', Existential x', s])
                         ctx
      in do
        ctx' <- instantiateL ctx0 x' (apply ctx0 x)
        instantiateL ctx' y' (apply ctx' y)
    Type.Forall x body -> -- InstLIIL
      let (v', ctx') = extendUniversal ctx
      in instantiateL ctx' v (Type.subst body x (Type.Universal v'))
         >>= retract (Universal v')
    _ -> Left $ note "could not instantiate left"

-- | Instantiate the given existential such that it is
-- a supertype of the given type, updating the context
-- in the process.
instantiateR :: Context -> Type -> ABT.V -> Either Note Context
instantiateR ctx t v = case Type.monotype t >>= solve ctx v of
  Just ctx' -> pure ctx' -- InstRSolve
  Nothing -> case t of
    Type.Existential v2 | ordered ctx v v2 -> -- InstRReach (both are existential, set v2 = v)
      maybe (Left $ note "InstRReach failed") pure $
        solve ctx v2 (Type.Monotype (Type.Existential v))
    Type.Arrow i o -> -- InstRArrow
      let i' = fresh ctx
          o' = fresh' i'
          s = Solved v (Type.Monotype (Type.Arrow (Type.Existential i') (Type.Existential o')))
      in do
        ctx' <- instantiateL (replace (Existential v) (context [Existential o', Existential i', s]) ctx)
                             i'
                             i
        instantiateR ctx' (apply ctx' o) o'
    Type.App x y -> -- analogue of InstRArr
      let x' = fresh ctx
          y' = fresh' x'
          s = Solved v (Type.Monotype (Type.App (Type.Existential x') (Type.Existential y')))
          ctx0 = replace (Existential v)
                         (context [Existential y', Existential x', s])
                         ctx
      in do
        ctx' <- instantiateR ctx0 (apply ctx0 x) x'
        instantiateR ctx' (apply ctx' y) y'
    Type.Forall x body -> -- InstRAIIL
      let x' = fresh ctx
      in
        instantiateR (ctx `append` context [Marker x', Existential x'])
                     (Type.subst body x (Type.Existential x'))
                     v
        >>= retract (Marker x')
    _ -> Left $ note "could not instantiate right"

-- | Check that under the given context, `e` has type `t`,
-- updating the context in the process.
check :: Context -> Term -> Type -> Either Note Context
check ctx e t | wellformedType ctx t = scope (show e ++ " : " ++ show t) $ go e t where
  go (Term.Lit l) _ = subtype ctx (synthLit l) t -- 1I
  go _ (Type.Forall x body) = -- ForallI -- this is key, use existential
    let (x', ctx') = extendUniversal ctx
    in check ctx' e (Type.subst body x (Type.Universal x'))
       >>= retract (Universal x')
  go fn@(Term.Lam _) (Type.Arrow i o) = -- =>I
    let x' = fresh ctx
        v = Term.Var x'
        ctx' = extend (Ann x' i) ctx
        body' = Term.betaReduce (fn `Term.App` v)
    in check ctx' body' o >>= retract (Ann x' i)
  -- go Term.Blank _ = Right ctx -- possible hack to workaround lack of impredicative instantiation
  go _ _ = do -- Sub
    (a, ctx') <- synthesize ctx e
    subtype ctx' (apply ctx' a) (apply ctx' t)
check _ _ _ = Left $ note "type not well formed wrt context"

-- | Infer the type of a literal
synthLit :: Term.Literal -> Type
synthLit lit = Type.Unit $ case lit of
  Term.Number _ -> Type.Number
  Term.Text _ -> Type.Text
  Term.Distance _ -> Type.Distance

-- | Synthesize the type of the given term, updating the context in the process.
synthesize :: Context -> Term -> Either Note (Type, Context)
synthesize ctx e = scope ("infer: " ++ show e) $ go e where
  go (Term.Var v) = case lookupType ctx v of -- Var
    Nothing -> Left $ note "type not in scope"
    Just t -> pure (t, ctx)
  go Term.Blank = pure (Type.forall1 $ \x -> x, ctx)
  go (Term.Ann (Term.Ref _) t) =
    pure (t, ctx) -- innermost Ref annotation assumed to be correctly provided by `synthesizeClosed`
  go (Term.Ref h) = Left . note $ "unannotated reference: " ++ show h
  go (Term.Ann e' t) = (,) t <$> check ctx e' t -- Anno
  go (Term.Lit l) = pure (synthLit l, ctx) -- 1I=>
  go (Term.App f arg) = do -- ->E
    (ft, ctx') <- synthesize ctx f
    synthesizeApp ctx' (apply ctx' ft) arg
  go (Term.Vector v) =
    let e = fresh ctx
        ctxTl = context [Marker e, Existential e]
        step term ctx = check ctx term (Type.Existential e)
    in Foldable.foldrM step (ctx `append` ctxTl) v >>=
       \ctx' -> pure $
         let
           (ctx1, ctx2) = breakAt (Marker e) ctx'
           -- unsolved existentials get generalized to universals
           vt = apply ctx2 (Type.Unit Type.Vector `Type.App` Type.Existential e)
           existentials' = unsolved ctx2
           vt2 = foldr gen vt existentials'
           gen e vt = Type.forall1 $ \v -> Type.subst vt e v
         in (vt2, ctx1)
  go fn@(Term.Lam _) = -- ->I=> (Full Damas Milner rule)
    let (arg, i, o) = fresh3 ctx
        ctxTl = context [Marker i, Existential i, Existential o,
                         Ann arg (Type.Existential i)]
    in do
      ctx' <- check (ctx `append` ctxTl)
                    (Term.betaReduce $ fn `Term.App` Term.Var arg)
                    (Type.Existential o)
      pure $ let
        (ctx1, ctx2) = breakAt (Marker i) ctx'
        -- unsolved existentials get generalized to universals
        ft = apply ctx2 (Type.Arrow (Type.Existential i) (Type.Existential o))
        existentials' = unsolved ctx2
        ft2 = foldr gen ft existentials'
        gen e ft = Type.forall1 $ \v -> Type.subst ft e v
        in (ft2, ctx1)

-- | Synthesize the type of the given term, `arg` given that a function of
-- the given type `ft` is being applied to `arg`. Update the conext in
-- the process.
synthesizeApp :: Context -> Type -> Term -> Either Note (Type, Context)
synthesizeApp ctx ft arg = go ft where
  go (Type.Forall x body) = let x' = fresh ctx -- Forall1App
    in synthesizeApp (ctx `append` context [Existential x'])
                     (Type.subst body x (Type.Existential x'))
                     arg
  go (Type.Arrow i o) = (,) o <$> check ctx arg i -- ->App
  go (Type.Existential a) = -- a^App
    let i = fresh ctx
        o = fresh' i
        soln = Monotype (Type.Arrow (Type.Existential i) (Type.Existential o))
        ctxMid = context [Existential o, Existential i, Solved a soln]
    in (,) (Type.Existential o) <$>
      check (replace (Existential a) ctxMid ctx)
                      arg
                      (Type.Existential i)
  go _ = Left $ note "unable to synthesize type of application"

annotateRefs :: Applicative f => Type.Env f -> Term -> Noted f Term
annotateRefs synth term' = case term' of
  Term.Ref h -> Term.Ann (Term.Ref h) <$> synth h
  Term.App f arg -> Term.App <$> annotateRefs synth f <*> annotateRefs synth arg
  Term.Ann body t -> Term.Ann <$> annotateRefs synth body <*> pure t
  Term.Lam body -> Term.Lam <$> annotateRefs synth body
  Term.Vector terms -> Term.Vector <$> traverse (annotateRefs synth) terms
  _ -> pure term'

synthesizeClosed :: Applicative f => Type.Env f -> Term -> Noted f Type
synthesizeClosed synthRef term = Noted $ synth <$> N.unnote (annotateRefs synthRef term)
  where
    synth :: Either Note Term -> Either Note Type
    synth (Left e) = Left e
    synth (Right a) = go <$> synthesize (context []) a
    go (t, ctx) = apply ctx t
-}
