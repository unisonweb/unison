{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ViewPatterns #-}
{-# Language OverloadedStrings #-}
{-# Language PatternGuards #-}
{-# Language PatternSynonyms #-}
{-# Language ScopedTypeVariables #-}

module Unison.Runtime.ANF
  ( optimize
  , fromTerm
  , fromTerm'
  , term
  , minimizeCyclesOrCrash
  , pattern TVar
  , pattern TLit
  , pattern TApp
  , pattern TApv
  , pattern TCom
  , pattern TCon
  , pattern TReq
  , pattern TPrm
  , pattern THnd
  , pattern TLet
  , pattern TMatch
  , Lit(..)
  , SuperNormal(..)
  , POp(..)
  , ANormalF(..)
  , ANormal
  , Branched(..)
  , Func(..)
  , toSuperNormal
  , anfTerm
  ) where

import Unison.Prelude

import Data.Bifunctor (second)
import Data.Bifoldable (Bifoldable(..))
import Data.List hiding (and,or)
import Prelude hiding (abs,and,or,seq)
import Unison.Term
import Unison.Var (Var, typed)
import Data.IntMap (IntMap)
import qualified Data.Map as Map
import qualified Data.IntMap.Strict as IMap
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Unison.ABT as ABT
import Unison.ABT.Normalized (pattern TVar)
import qualified Unison.ABT.Normalized as ABTN
import qualified Unison.Term as Term
import qualified Unison.Var as Var
import Unison.Typechecker.Components (minimize')
import Unison.Pattern (PatternP(..))
import Unison.Reference (Reference)
-- import Debug.Trace
-- import qualified Unison.TermPrinter as TP
-- import qualified Unison.Util.Pretty as P

newtype ANF v a = ANF_ { term :: Term v a }

-- Replace all lambdas with free variables with closed lambdas.
-- Works by adding a parameter for each free variable. These
-- synthetic parameters are added before the existing lambda params.
-- For example, `(x -> x + y + z)` becomes `(y z x -> x + y + z) y z`.
-- As this replacement has the same type as the original lambda, it
-- can be done as a purely local transformation, without updating any
-- call sites of the lambda.
--
-- The transformation is shallow and doesn't transform the body of
-- lambdas it finds inside of `t`.
lambdaLift :: (Var v, Semigroup a) => (v -> v) -> Term v a -> Term v a
lambdaLift liftVar t = result where
  result = ABT.visitPure go t
  go t@(LamsNamed' vs body) = Just $ let
    fvs = ABT.freeVars t
    fvsLifted = [ (v, liftVar v) | v <- toList fvs ]
    a = ABT.annotation t
    subs = [(v, var a v') | (v,v') <- fvsLifted ]
    in if Set.null fvs then lam' a vs body -- `lambdaLift body` would make transform deep
       else apps' (lam' a (map snd fvsLifted ++ vs) (ABT.substs subs body))
                  (snd <$> subs)
  go _ = Nothing

optimize :: forall a v . (Semigroup a, Var v) => Term v a -> Term v a
optimize t = go t where
  ann = ABT.annotation
  go (Let1' b body) | canSubstLet b body = go (ABT.bind body b)
  go e@(App' f arg) = case go f of
    Lam' f -> go (ABT.bind f arg)
    f -> app (ann e) f (go arg)
  go (If' (Boolean' False) _ f) = go f
  go (If' (Boolean' True) t _) = go t
  -- todo: can simplify match expressions
  go e@(ABT.Var' _) = e
  go e@(ABT.Tm' f) = case e of
    Lam' _ -> e -- optimization is shallow - don't descend into lambdas
    _ -> ABT.tm' (ann e) (go <$> f)
  go e@(ABT.out -> ABT.Cycle body) = ABT.cycle' (ann e) (go body)
  go e@(ABT.out -> ABT.Abs v body) = ABT.abs' (ann e) v (go body)
  go e = e

  -- test for whether an expression `let x = y in body` can be
  -- reduced by substituting `y` into `body`. We only substitute
  -- when `y` is a variable or a primitive, otherwise this might
  -- end up duplicating evaluation or changing the order that
  -- effects are evaluated
  canSubstLet expr _body
    | isLeaf expr = True
    -- todo: if number of occurrences of the binding is 1 and the
    -- binding is pure, okay to substitute
    | otherwise   = False

isLeaf :: ABT.Term (F typeVar typeAnn patternAnn) v a -> Bool
isLeaf (Var' _) = True
isLeaf (Int' _) = True
isLeaf (Float' _) = True
isLeaf (Nat' _) = True
isLeaf (Text' _) = True
isLeaf (Boolean' _) = True
isLeaf (Constructor' _ _) = True
isLeaf (TermLink' _) = True
isLeaf (TypeLink' _) = True
isLeaf _ = False

minimizeCyclesOrCrash :: Var v => Term v a -> Term v a
minimizeCyclesOrCrash t = case minimize' t of
  Right t -> t
  Left e -> error $ "tried to minimize let rec with duplicate definitions: "
                 ++ show (fst <$> toList e)

fromTerm' :: (Monoid a, Var v) => (v -> v) -> Term v a -> Term v a
fromTerm' liftVar t = term (fromTerm liftVar t)

fromTerm :: forall a v . (Monoid a, Var v) => (v -> v) -> Term v a -> ANF v a
fromTerm liftVar t = ANF_ (go $ lambdaLift liftVar t) where
  ann = ABT.annotation
  isRef (Ref' _) = True
  isRef _ = False
  fixup :: Set v -- if we gotta create new vars, avoid using these
       -> ([Term v a] -> Term v a) -- do this with ANF'd args
       -> [Term v a] -- the args (not all in ANF already)
       -> Term v a -- the ANF'd term
  fixup used f args = let
    args' = Map.fromList $ toVar =<< (args `zip` [0..])
    toVar (b, i) | isLeaf b   = []
                 | otherwise = [(i, Var.freshIn used (Var.named . Text.pack $ "arg" ++ show i))]
    argsANF = map toANF (args `zip` [0..])
    toANF (b,i) = maybe b (var (ann b)) $ Map.lookup i args'
    addLet (b,i) body = maybe body (\v -> let1' False [(v,go b)] body) (Map.lookup i args')
    in foldr addLet (f argsANF) (args `zip` [(0::Int)..])
  go :: Term v a -> Term v a
  go e@(Apps' f args)
    | (isRef f || isLeaf f) && all isLeaf args = e
    | not (isRef f || isLeaf f) =
      let f' = ABT.fresh e (Var.named "f")
      in let1' False [(f', go f)] (go $ apps' (var (ann f) f') args)
    | otherwise = fixup (ABT.freeVars e) (apps' f) args
  go e@(Handle' h body)
    | isLeaf h = handle (ann e) h (go body)
    | otherwise = let h' = ABT.fresh e (Var.named "handler")
                  in let1' False [(h', go h)] (handle (ann e) (var (ann h) h') (go body))
  go e@(If' cond t f)
    | isLeaf cond = iff (ann e) cond (go t) (go f)
    | otherwise = let cond' = ABT.fresh e (Var.named "cond")
                  in let1' False [(cond', go cond)] (iff (ann e) (var (ann cond) cond') (go t) (go f))
  go e@(Match' scrutinee cases)
    | isLeaf scrutinee = match (ann e) scrutinee (fmap go <$> cases)
    | otherwise = let scrutinee' = ABT.fresh e (Var.named "scrutinee")
                  in let1' False [(scrutinee', go scrutinee)]
                      (match (ann e)
                             (var (ann scrutinee) scrutinee')
                             (fmap go <$> cases))
  -- MatchCase RHS, shouldn't conflict with LetRec
  go (ABT.Abs1NA' avs t) = ABT.absChain' avs (go t)
  go e@(And' x y)
    | isLeaf x = and (ann e) x (go y)
    | otherwise =
        let x' = ABT.fresh e (Var.named "argX")
        in let1' False [(x', go x)] (and (ann e) (var (ann x) x') (go y))
  go e@(Or' x y)
    | isLeaf x = or (ann e) x (go y)
    | otherwise =
        let x' = ABT.fresh e (Var.named "argX")
        in let1' False [(x', go x)] (or (ann e) (var (ann x) x') (go y))
  go e@(Var' _) = e
  go e@(Int' _) = e
  go e@(Nat' _) = e
  go e@(Float' _) = e
  go e@(Boolean' _) = e
  go e@(Text' _) = e
  go e@(Char' _) = e
  go e@(Blank' _) = e
  go e@(Ref' _) = e
  go e@(TermLink' _) = e
  go e@(TypeLink' _) = e
  go e@(RequestOrCtor' _ _) = e
  go e@(Lam' _) = e -- ANF conversion is shallow -
                     -- don't descend into closed lambdas
  go (Let1Named' v b e) = let1' False [(v, go b)] (go e)
  -- top = False because we don't care to emit typechecker notes about TLDs
  go (LetRecNamed' bs e) = letRec' False (fmap (second go) bs) (go e)
  go e@(Sequence' vs) =
    if all isLeaf vs then e
    else fixup (ABT.freeVars e) (seq (ann e)) (toList vs)
  go e@(Ann' tm typ) = Term.ann (ann e) (go tm) typ
  go e = error $ "ANF.term: I thought we got all of these\n" <> show e

data ANormalF v e
  = ALit Lit
  | ALet e e
  | AMatch v (Branched e)
  | AHnd v e
  | AApp (Func v) [v]

instance Bifoldable ANormalF where
  bifoldMap _ _ (ALit _) = mempty
  bifoldMap _ g (ALet b e) = g b <> g e
  bifoldMap f g (AMatch v br) = f v <> foldMap g br
  bifoldMap f g (AHnd v e) = f v <> g e
  bifoldMap f _ (AApp func args) = foldMap f func <> foldMap f args

matchLit :: Term v a -> Maybe Lit
matchLit (Int' i) = Just $ I i
matchLit (Nat' n) = Just $ N n
matchLit (Float' f) = Just $ F f
matchLit (Boolean' b) = Just $ B b
matchLit (Text' t) = Just $ T t
matchLit (Char' c) = Just $ C c
matchLit _ = Nothing

pattern Lit' l <- (matchLit -> Just l)
pattern TLit l = ABTN.TTm (ALit l)

pattern TApp f args = ABTN.TTm (AApp f args)
pattern TApv v args = TApp (FVar v) args
pattern TCom r args = TApp (FComb r) args
pattern TCon r t args = TApp (FCon r t) args
pattern TReq r t args = TApp (FReq r t) args
pattern TPrm p args = TApp (FPrim p) args

pattern THnd v b = ABTN.TTm (AHnd v b)
pattern TLet v bn bo = ABTN.TTm (ALet bn (ABTN.TAbs v bo))
pattern TMatch v cs = ABTN.TTm (AMatch v cs)

{-# complete TVar, TApp, TLit, THnd, TLet, TMatch #-}
{-# complete
      TVar, TApv, TCom, TCon, TReq, TPrm, TLit, THnd, TLet, TMatch
  #-}

data Branched e
  = MatchIntegral { cases :: IntMap e }
  | MatchData { ref :: Reference, cases :: IntMap e }
  | MatchAbility { ref :: Reference, cases :: IntMap e, other :: e }
  deriving (Functor, Foldable, Traversable)

data Func v
  -- variable
  = FVar v
  -- top-level combinator
  | FComb !Int
  -- data constructor
  | FCon !Int !Int
  -- ability request
  | FReq !Int !Int
  -- prim op
  | FPrim POp
  deriving (Functor, Foldable, Traversable)

data Lit
  = I Int64
  | N Word64
  | F Double
  | B Bool
  | T Text
  | C Char

data POp
  = PAND | POR | PNOT
  -- Int
  | PADI | PSUI | PMUI | PDII
  | PGTI | PLTI | PGEI | PLEI | PEQI
  | PSGI | PNEI | PTRI | PMDI
  -- Nat
  | PADN | PSUN | PMUN | PDIN
  | PGTN | PLTN | PGEN | PLEN | PEQN
  | PSGN | PNEN | PTRN | PMDN
  -- Float
  | PADF | PSUF | PMUF | PDIF
  | PGTF | PLTF | PGEF | PLEF | PEQF

type ANormal = ABTN.Term ANormalF

-- Should be a completely closed term
data SuperNormal v = Lambda { bound :: ANormal v }

toSuperNormal
  :: Var v
  => (Reference -> Int)
  -> Term v a
  -> SuperNormal v
toSuperNormal resolve tm
  | not . Set.null $ freeVars tm
  = error "free variables in supercombinators"
  | otherwise = Lambda bound
  where
  (vs, body) = fromMaybe ([], tm) $ unLams' tm
  bound = foldr ABTN.TAbs anfBody vs
  anfBody = anfTerm (Set.fromList vs) resolve body

anfTerm
  :: Var v
  => Set v              -- variable choices to avoid
  -> (Reference -> Int) -- resolve a reference to a supercombinator
  -> Term v a
  -> ANormal v
anfTerm avoid resolve tm = case anfBlock avoid resolve tm of
  (ctx, body) -> foldr le body ctx
  where le (v, b) e = TLet v b e

anfBlock
  :: Var v
  => Set v
  -> (Reference -> Int)
  -> Term v a
  -> ([(v, ANormal v)], ANormal v)
anfBlock _     _       (Var' v) = ([], TVar v)
anfBlock avoid resolve (If' c t f)
  | TVar cv <- cc = (cctx, TMatch cv cases)
  | otherwise          = (cctx ++ [(fcv,cc)], TMatch fcv cases)
  where
  (cctx, cc) = anfBlock avoid resolve c
  fcv = freshANF avoid (ABTN.freeVars cc)
  cases = MatchIntegral $ IMap.fromList
    [ (0, anfTerm avoid resolve f)
    , (1, anfTerm avoid resolve t)
    ]
anfBlock avoid resolve (And' l r) = (lctx ++ rctx, TPrm PAND [vl, vr])
  where
  (lctx, vl) = anfArg avoid resolve l
  (rctx, vr) = anfArg (addAvoid avoid lctx) resolve r
anfBlock avoid resolve (Or' l r) = (lctx ++ rctx, TPrm POR [vl, vr])
  where
  (lctx, vl) = anfArg avoid resolve l
  (rctx, vr) = anfArg (addAvoid avoid lctx) resolve r
anfBlock avoid resolve (Handle' h body) = (hctx ++ bctx, THnd vh cb)
  where
  (hctx, vh) = anfArg avoid resolve h
  (bctx, cb) = anfBlock (addAvoid avoid hctx) resolve body
anfBlock avoid resolve (Match' scrut cas)
  | TVar sv <- sc = (sctx, TMatch sv cases)
  | otherwise = (sctx ++ [(fsv, sc)], TMatch fsv cases)
  where
  (sctx, sc) = anfBlock avoid resolve scrut
  fsv = freshANF avoid $ ABTN.freeVars sc
  cases = MatchIntegral $ anfCases avoid resolve cas
anfBlock avoid resolve (Let1Named' v b e)
  = (bctx ++ (v, cb) : ectx, ce)
  where
  avoid' = Set.insert v avoid
  (bctx, cb) = anfBlock avoid' resolve b
  (ectx, ce) = anfBlock avoid' resolve e
anfBlock avoid resolve (Apps' f args)
  = (fctx ++ actx, TApp cf cas)
  where
  (fctx, cf) = anfFunc avoid resolve f
  (actx, cas) = foldr acc (const ([], [])) args avoid
  acc tm k av
    | (cctx, cv) <- anfArg av resolve tm
    , (ctx, cas) <- k (addAvoid av cctx)
    = (cctx ++ ctx, cv : cas)
anfBlock _     resolve (Constructor' r t)
  = ([], TCon (resolve r) t [])
anfBlock _     resolve (Request' r t)
  = ([], TReq (resolve r) t [])
anfBlock _     _       (Lit' l) = ([], TLit l)
anfBlock _     _       (Blank' _) = error "tried to compile Blank"
anfBlock _     _       _ = error "anf: unhandled term"

addAvoid :: Var v => Set v -> [(v,x)] -> Set v
addAvoid av ctx = Set.union av . Set.fromList . map fst $ ctx

-- Note: this assumes that patterns have already been translated
-- to a state in which every case matches a single layer of data,
-- with no guards, and no variables ignored. This is not checked
-- completely.
anfCases
  :: Var v
  => Set v
  -> (Reference -> Int)
  -> [MatchCase p (Term v a)]
  -> IntMap (ANormal v)
anfCases avoid resolve = IMap.fromList . map mkCase
  where
  -- TODO: Int64, Word64, ...
  patDecode (IntP _ i) = fromIntegral $ i
  patDecode (NatP _ n) = fromIntegral $ n
  patDecode (ConstructorP _ _ t _) = fromIntegral $ t
  patDecode (EffectBindP _ _ t _ _) = fromIntegral $ t
  patDecode _ = error "unexpected pattern for ANF"

  mkCase (MatchCase p Nothing (ABT.AbsN' vs body))
    = (patDecode p, anfTerm avoid' resolve body)
    where avoid' = Set.union avoid $ Set.fromList vs
  mkCase _ = error "unexpected guard for ANF"

anfFunc
  :: Var v
  => Set v              -- variable choices to avoid
  -> (Reference -> Int) -- resolve a reference to a supercombinator
  -> Term v a
  -> ([(v, ANormal v)], Func v)
anfFunc _     _       (Var' v) = ([], FVar v)
anfFunc _     resolve (Ref' r) = ([], FComb $ resolve r)
anfFunc _     resolve (Constructor' r t) = ([], FCon (resolve r) t)
anfFunc _     resolve (Request' r t) = ([], FReq (resolve r) t)
anfFunc avoid resolve tm
  = case anfBlock (Set.insert v avoid) resolve tm of
      (fctx, anfTm) -> (fctx ++ [(v, anfTm)], FVar v)
  where
  fvs = ABT.freeVars tm
  v = freshANF avoid fvs

anfArg
  :: Var v
  => Set v
  -> (Reference -> Int)
  -> Term v a
  -> ([(v, ANormal v)], v)
anfArg avoid resolve tm = case anfBlock avoid resolve tm of
  (ctx, TVar v) -> (ctx, v)
  (ctx, tm) -> (ctx ++ [(fv, tm)], fv)
    where fv = freshANF avoid $ ABTN.freeVars tm

freshANF :: Var v => Set v -> Set v -> v
freshANF avoid free
  = ABT.freshIn (Set.union avoid free) (typed Var.ANFBlank)
