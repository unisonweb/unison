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
  , pattern TName
  , pattern TBind
  , pattern TTm
  , pattern TBinds
  , pattern TBinds'
  , pattern TMatch
  , Lit(..)
  , SuperNormal(..)
  , POp(..)
  , ANormalBF(..)
  , ANormalTF(.., AApv, ACom, ACon, AReq, APrm)
  , ANormal
  , ANormalT
  , Branched(..)
  , Handler(..)
  , Func(..)
  , toSuperNormal
  , anfTerm
  , sink
  ) where

import Unison.Prelude

import Data.Bifunctor (Bifunctor(..))
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

-- Context entries with evaluation strategy
data CTE v s
  = ST { cvar :: v, _sbnd :: s }
  | LZ { cvar :: v, _fbnd :: Int, _abnd :: [v] }

data ANormalBF v e
  = ALet (ANormalTF v e) e
  | AName Int [v] e
  | ATm (ANormalTF v e)

data ANormalTF v e
  = ALit Lit
  | AMatch v (Branched e)
  | AHnd (Handler e) e
  | AApp (Func v) [v]
  | AVar v

instance Functor (ANormalBF v) where
  fmap f (ALet bn bo) = ALet (f <$> bn) $ f bo
  fmap f (AName n as bo) = AName n as $ f bo
  fmap f (ATm tm) = ATm $ f <$> tm

instance Bifunctor ANormalBF where
  bimap f g (ALet bn bo) = ALet (bimap f g bn) $ g bo
  bimap f g (AName n as bo) = AName n (f <$> as) $ g bo
  bimap f g (ATm tm) = ATm (bimap f g tm)

instance Bifoldable ANormalBF where
  bifoldMap f g (ALet b e) = bifoldMap f g b <> g e
  bifoldMap f g (AName _ as e) = foldMap f as <> g e
  bifoldMap f g (ATm e) = bifoldMap f g e

instance Functor (ANormalTF v) where
  fmap _ (AVar v) = AVar v
  fmap _ (ALit l) = ALit l
  fmap f (AMatch v br) = AMatch v $ f <$> br
  fmap f (AHnd h e) = AHnd (f <$> h) $ f e
  fmap _ (AApp f args) = AApp f args

instance Bifunctor ANormalTF where
  bimap f _ (AVar v) = AVar (f v)
  bimap _ _ (ALit l) = ALit l
  bimap f g (AMatch v br) = AMatch (f v) $ fmap g br
  bimap _ g (AHnd v e) = AHnd (g <$> v) $ g e
  bimap f _ (AApp fu args) = AApp (fmap f fu) $ fmap f args

instance Bifoldable ANormalTF where
  bifoldMap f _ (AVar v) = f v
  bifoldMap _ _ (ALit _) = mempty
  bifoldMap f g (AMatch v br) = f v <> foldMap g br
  bifoldMap _ g (AHnd h e) = foldMap g h <> g e
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
pattern TLet v bn bo = ABTN.TTm (ALet bn (ABTN.TAbs v bo))
pattern TName v f as bo = ABTN.TTm (AName f as (ABTN.TAbs v bo))
pattern TTm e = ABTN.TTm (ATm e)
{-# complete TLet, TName, TTm #-}

pattern TLit l = TTm (ALit l)

pattern TApp f args = TTm (AApp f args)
pattern AApv v args = AApp (FVar v) args
pattern TApv v args = TApp (FVar v) args
pattern ACom r args = AApp (FComb r) args
pattern TCom r args = TApp (FComb r) args
pattern ACon r t args = AApp (FCon r t) args
pattern TCon r t args = TApp (FCon r t) args
pattern AReq r t args = AApp (FReq r t) args
pattern TReq r t args = TApp (FReq r t) args
pattern APrm p args = AApp (FPrim p) args
pattern TPrm p args = TApp (FPrim p) args

pattern THnd h b = TTm (AHnd h b)
pattern TMatch v cs = TTm (AMatch v cs)
pattern TVar v = TTm (AVar v)

{-# complete TLet, TName, TVar, TApp, TLit, THnd, TMatch #-}
{-# complete TLet, TName,
      TVar, TApv, TCom, TCon, TReq, TPrm, TLit, THnd, TMatch
  #-}

directVars :: Var v => ANormalT v -> Set.Set v
directVars = bifoldMap Set.singleton (const mempty)

freeVarsT :: Var v => ANormalT v -> Set.Set v
freeVarsT = bifoldMap Set.singleton ABTN.freeVars

bind :: Var v => Cte v -> ANormal v -> ANormal v
bind (ST u bu) = TLet u bu
bind (LZ u f as) = TName u f as

unbind :: Var v => ANormal v -> Maybe (Cte v, ANormal v)
unbind (TLet  u bu bd) = Just (ST u bu, bd)
unbind (TName u f as bd) = Just (LZ u f as, bd)
unbind _ = Nothing

unbinds :: Var v => ANormal v -> (Ctx v, ANormal v)
unbinds (TLet  u bu (unbinds -> (ctx, bd))) = (ST u bu:ctx, bd)
unbinds (TName u f as (unbinds -> (ctx, bd))) = (LZ u f as:ctx, bd)
unbinds tm = ([], tm)

unbinds' :: Var v => ANormal v -> (Ctx v, ANormalT v)
unbinds' (TLet  u bu (unbinds' -> (ctx, bd))) = (ST u bu:ctx, bd)
unbinds' (TName u f as (unbinds' -> (ctx, bd))) = (LZ u f as:ctx, bd)
unbinds' (TTm tm) = ([], tm)

pattern TBind bn bd <- (unbind -> Just (bn, bd))
  where TBind bn bd = bind bn bd

pattern TBinds :: Var v => Ctx v -> ANormal v -> ANormal v
pattern TBinds ctx bd <- (unbinds -> (ctx, bd))
  where TBinds ctx bd = foldr bind bd ctx

pattern TBinds' :: Var v => Ctx v -> ANormalT v -> ANormal v
pattern TBinds' ctx bd <- (unbinds' -> (ctx, bd))
  where TBinds' ctx bd = foldr bind (TTm bd) ctx

data Branched e
  = MatchIntegral { cases :: IntMap e }
  | MatchData { ref :: Reference, cases :: IntMap e }
  | MatchEmpty
  deriving (Functor, Foldable, Traversable)

instance Semigroup (Branched e) where
  MatchEmpty <> r = r
  l <> MatchEmpty = l
  MatchIntegral cl <> MatchIntegral cr = MatchIntegral $ cl <> cr
  MatchData rl cl <> MatchData rr cr
    | rl == rr  = MatchData rl $ cl <> cr
  _ <> _ = error "cannot merge data cases for different types"

instance Monoid (Branched e) where
  mempty = MatchEmpty

data Handler e
  = Hndl
  { hcases :: IntMap (IntMap e)
  , dflt :: Maybe e
  } deriving (Functor, Foldable, Traversable)

instance Semigroup (Handler e) where
  Hndl cl dl <> Hndl cr dr = Hndl cm $ mplus dl dr
    where
    cm = IMap.unionWith (<>) cl cr

instance Monoid (Handler e) where
  mempty = Hndl IMap.empty Nothing

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
  deriving (Show)

type ANormal = ABTN.Term ANormalBF
type ANormalT v = ANormalTF v (ANormal v)

type ABranched v = Branched (ANormal v)
type AHandler v = Handler (ANormal v)

type Cte v = CTE v (ANormalT v)
type Ctx v = [Cte v]

data ACases v
  = DCase (ABranched v)
  | HCase (AHandler v)

instance Semigroup (ACases v) where
  DCase MatchEmpty <> cr = cr
  cl <> DCase MatchEmpty = cl
  DCase dl <> DCase dr = DCase $ dl <> dr
  HCase hl <> HCase hr = HCase $ hl <> hr
  _ <> _ = error "cannot merge data and ability cases"

instance Monoid (ACases v) where
  mempty = DCase MatchEmpty

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
  (ctx, body) -> TBinds' ctx body

anfBlock
  :: Var v
  => Set v
  -> (Reference -> Int)
  -> Term v a
  -> (Ctx v, ANormalT v)
anfBlock _     _       (Var' v) = ([], AVar v)
anfBlock avoid resolve (If' c t f)
  | AVar cv <- cc = (cctx, AMatch cv cases)
  | otherwise          = (cctx ++ [ST fcv cc], AMatch fcv cases)
  where
  (cctx, cc) = anfBlock avoid resolve c
  fcv = freshANF avoid $ freeVarsT cc
  cases = MatchIntegral $ IMap.fromList
    [ (0, anfTerm avoid resolve f)
    , (1, anfTerm avoid resolve t)
    ]
anfBlock avoid resolve (And' l r) = (lctx ++ rctx, APrm PAND [vl, vr])
  where
  (lctx, vl) = anfArg avoid resolve l
  (rctx, vr) = anfArg (addAvoid avoid lctx) resolve r
anfBlock avoid resolve (Or' l r) = (lctx ++ rctx, APrm POR [vl, vr])
  where
  (lctx, vl) = anfArg avoid resolve l
  (rctx, vr) = anfArg (addAvoid avoid lctx) resolve r
anfBlock avoid resolve (Handle' h body)
  = case anfBlock avoid' resolve body of
      ([], ACom f as) -> (hctx ++ [LZ fcv f as], AApp (FVar vh) [fcv])
        where fcv = freshANF avoid' $ Set.fromList as
      (_, _) ->
        error "handle body should be a call to a top-level combinator"
  where
  (hctx, vh) = anfArg avoid resolve h
  avoid' = addAvoid avoid hctx
anfBlock avoid resolve (Match' scrut cas) = case cases of
  HCase hn -> (sctx, AHnd hn (TTm sc))
  DCase cs
    | AVar sv <- sc -> (sctx, AMatch sv cs)
    | otherwise -> (sctx ++ [(ST fsv sc)], AMatch fsv cs)
  where
  (sctx, sc) = anfBlock avoid resolve scrut
  fsv = freshANF avoid $ freeVarsT sc
  cases = anfCases avoid resolve cas
anfBlock avoid resolve (Let1Named' v b e)
  = (bctx ++ (ST v cb) : ectx, ce)
  where
  avoid' = Set.insert v avoid
  (bctx, cb) = anfBlock avoid' resolve b
  (ectx, ce) = anfBlock avoid' resolve e
anfBlock avoid resolve (Apps' f args)
  = (fctx ++ actx, AApp cf cas)
  where
  (fctx, cf) = anfFunc avoid resolve f
  (actx, cas) = foldr acc (const ([], [])) args avoid
  acc tm k av
    | (cctx, cv) <- anfArg av resolve tm
    , (ctx, cas) <- k (addAvoid av cctx)
    = (cctx ++ ctx, cv : cas)
anfBlock _     resolve (Constructor' r t)
  = ([], ACon (resolve r) t [])
anfBlock _     resolve (Request' r t)
  = ([], AReq (resolve r) t [])
anfBlock _     _       (Lit' l) = ([], ALit l)
anfBlock _     _       (Blank' _) = error "tried to compile Blank"
anfBlock _     _       _ = error "anf: unhandled term"

addAvoid :: Var v => Set v -> Ctx v -> Set v
addAvoid av ctx = Set.union av . Set.fromList . map cvar $ ctx

-- Note: this assumes that patterns have already been translated
-- to a state in which every case matches a single layer of data,
-- with no guards, and no variables ignored. This is not checked
-- completely.
anfInitCase
  :: Var v
  => Set v
  -> (Reference -> Int)
  -> MatchCase p (Term v a)
  -> ACases v
anfInitCase avoid resolve (MatchCase p guard (ABT.AbsN' vs bd))
  | Just _ <- guard = error "anfInitCase: unexpected guard"
  | IntP _ (fromIntegral -> i) <- p
  = DCase . MatchIntegral . IMap.singleton i $ anfTerm avoid' resolve bd
  | NatP _ (fromIntegral -> i) <- p
  = DCase . MatchIntegral . IMap.singleton i $ anfTerm avoid' resolve bd
  | ConstructorP _ r t ps <- p
  , us <- expandBindings avoid' ps vs
  = DCase . MatchData r
  . IMap.singleton t . ABTN.TAbss us
  $ anfTerm avoid' resolve bd
  | EffectPureP _ q <- p
  , us <- expandBindings avoid' [q] vs
  = HCase . Hndl IMap.empty . Just
  . ABTN.TAbss us $ anfTerm avoid' resolve bd
  | EffectBindP _ r t ps pk <- p
  , us <- expandBindings avoid' (ps ++ [pk]) vs
  = HCase . flip Hndl Nothing
  . IMap.singleton (resolve r) . IMap.singleton t
  . ABTN.TAbss us $ anfTerm avoid' resolve bd
  where avoid' = Set.union avoid $ Set.fromList vs
anfInitCase _ _ _ = error "anfInitCase: unexpected pattern"

expandBindings
  :: Var v
  => Set v
  -> [PatternP p]
  -> [v]
  -> [v]
expandBindings _ [] [] = []
expandBindings avoid (UnboundP _:ps) vs
  = u : expandBindings (Set.insert u avoid) ps vs
  where u = ABT.freshIn avoid $ typed Var.ANFBlank
expandBindings avoid (VarP _:ps) (v:vs)
  = v : expandBindings avoid ps vs
expandBindings _ [] (_:_)
  = error "expandBindings: more bindings than expected"
expandBindings _ (VarP _:_) []
  = error "expandBindings: more patterns than expected"
expandBindings _ _ _
  = error "expandBindings: unexpected pattern"

anfCases
  :: Var v
  => Set v
  -> (Reference -> Int)
  -> [MatchCase p (Term v a)]
  -> ACases v
anfCases avoid resolve = foldMap (anfInitCase avoid resolve)

anfFunc
  :: Var v
  => Set v              -- variable choices to avoid
  -> (Reference -> Int) -- resolve a reference to a supercombinator
  -> Term v a
  -> (Ctx v, Func v)
anfFunc _     _       (Var' v) = ([], FVar v)
anfFunc _     resolve (Ref' r) = ([], FComb $ resolve r)
anfFunc _     resolve (Constructor' r t) = ([], FCon (resolve r) t)
anfFunc _     resolve (Request' r t) = ([], FReq (resolve r) t)
anfFunc avoid resolve tm
  = case anfBlock (Set.insert v avoid) resolve tm of
      (fctx, anfTm) -> (fctx ++ [(ST v anfTm)], FVar v)
  where
  fvs = ABT.freeVars tm
  v = freshANF avoid fvs

anfArg
  :: Var v
  => Set v
  -> (Reference -> Int)
  -> Term v a
  -> (Ctx v, v)
anfArg avoid resolve tm = case anfBlock avoid resolve tm of
  (ctx, AVar v) -> (ctx, v)
  (ctx, tm) -> (ctx ++ [(ST fv tm)], fv)
    where fv = freshANF avoid $ freeVarsT tm

sink :: Var v => v -> ANormalT v -> ANormal v -> ANormal v
sink v tm = dive $ freeVarsT tm
  where
  dive _ exp | v `Set.notMember` ABTN.freeVars exp = exp
  dive avoid exp@(TName u f as bo)
    | v `elem` as
    = let w = freshANF avoid (ABTN.freeVars exp)
       in TLet w tm $ ABTN.rename v w exp
    | otherwise
    = TName u f as (dive avoid' bo)
    where avoid' = Set.insert u avoid
  dive avoid exp@(TLet u bn bo)
    | v `Set.member` directVars bn -- we need to stop here
    = let w = freshANF avoid (ABTN.freeVars exp)
       in TLet w tm $ ABTN.rename v w exp
    | otherwise
    = TLet u bn' $ dive avoid' bo
    where
    avoid' = Set.insert u avoid
    bn' | v `Set.notMember` freeVarsT bn = bn
        | otherwise = dive avoid' <$> bn
  dive avoid exp@(TTm tm)
    | v `Set.member` directVars tm -- same as above
    = let w = freshANF avoid (ABTN.freeVars exp)
       in TLet w tm $ ABTN.rename v w exp
    | otherwise = TTm $ dive avoid <$> tm

freshANF :: Var v => Set v -> Set v -> v
freshANF avoid free
  = ABT.freshIn (Set.union avoid free) (typed Var.ANFBlank)


