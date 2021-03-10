{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ViewPatterns #-}
{-# Language OverloadedStrings #-}
{-# Language PatternGuards #-}
{-# Language PatternSynonyms #-}
{-# Language ScopedTypeVariables #-}
{-# Language GeneralizedNewtypeDeriving #-}

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
  , pattern TKon
  , pattern TReq
  , pattern TPrm
  , pattern TFOp
  , pattern THnd
  , pattern TLet
  , pattern TLetD
  , pattern TFrc
  , pattern TLets
  , pattern TName
  , pattern TBind
  , pattern TBinds
  , pattern TShift
  , pattern TMatch
  , Mem(..)
  , Lit(..)
  , Direction(..)
  , SuperNormal(..)
  , SuperGroup(..)
  , POp(..)
  , FOp
  , close
  , saturate
  , float
  , lamLift
  , ANormalF(.., AApv, ACom, ACon, AKon, AReq, APrm, AFOp)
  , ANormal
  , RTag
  , CTag
  , Tag(..)
  , GroupRef(..)
  , Value(..)
  , Cont(..)
  , BLit(..)
  , packTags
  , unpackTags
  , ANFM
  , Branched(..)
  , Func(..)
  , superNormalize
  , anfTerm
  , valueTermLinks
  , valueLinks
  , groupTermLinks
  , groupLinks
  , normalLinks
  , prettyGroup
  ) where

import Unison.Prelude

import Control.Monad.Reader (ReaderT(..), ask, local)
import Control.Monad.State (State, runState, MonadState(..), modify, gets)
import Control.Lens (snoc, unsnoc)

import Data.Bifunctor (Bifunctor(..))
import Data.Bifoldable (Bifoldable(..))
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Functor.Compose (Compose(..))
import Data.List hiding (and,or)
import Prelude hiding (abs,and,or,seq)
import qualified Prelude
import Unison.Term hiding (resolve, fresh, float, Text, Ref, List)
import Unison.Var (Var, typed)
import Unison.Util.EnumContainers as EC
import Unison.Util.Bytes (Bytes)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Unison.ABT as ABT
import qualified Unison.ABT.Normalized as ABTN
import qualified Unison.Term as Term
import qualified Unison.Type as Ty
import qualified Unison.Builtin.Decls as Ty (unitRef,seqViewRef)
import qualified Unison.Var as Var
import Unison.Typechecker.Components (minimize')
import Unison.Pattern (SeqOp(..))
import qualified Unison.Pattern as P
import Unison.Reference (Reference(..))
import Unison.Referent (Referent, pattern Ref, pattern Con)

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

closure :: Var v => Map v (Set v, Set v) -> Map v (Set v)
closure m0 = trace (snd <$> m0)
  where
  refs = fst <$> m0

  expand acc fvs rvs
    = fvs <> foldMap (\r -> Map.findWithDefault mempty r acc) rvs

  trace acc
    | acc == acc' = acc
    | otherwise = trace acc'
    where
    acc' = Map.intersectionWith (expand acc) acc refs

expandRec
  :: (Var v, Monoid a)
  => Set v
  -> [(v, Term v a)]
  -> [(v, Term v a)]
expandRec keep vbs = mkSub <$> fvl
  where
  mkSub (v, fvs) = (v, apps' (var mempty v) (var mempty <$> fvs))

  fvl = Map.toList
      . fmap (Set.toList)
      . closure
      $ Set.partition (`Set.member` keep)
      . ABT.freeVars
     <$> Map.fromList vbs

expandSimple
  :: (Var v, Monoid a)
  => Set v
  -> (v, Term v a)
  -> (v, Term v a)
expandSimple keep (v, bnd) = (v, apps' (var a v) evs)
  where
  a = ABT.annotation bnd
  fvs = ABT.freeVars bnd
  evs = map (var a) . Set.toList $ Set.difference fvs keep


abstract :: (Var v) => Set v -> Term v a -> Term v a
abstract keep bnd = lam' a evs bnd
  where
  a = ABT.annotation bnd
  fvs = ABT.freeVars bnd
  evs = Set.toList $ Set.difference fvs keep

enclose
  :: (Var v, Monoid a)
  => Set v
  -> (Set v -> Term v a -> Term v a)
  -> Term v a
  -> Maybe (Term v a)
enclose keep rec (LetRecNamedTop' top vbs bd)
  = Just $ letRec' top lvbs lbd
  where
  xpnd = expandRec keep' vbs
  keep' = Set.union keep . Set.fromList . map fst $ vbs
  lvbs = (map.fmap) (rec keep' . abstract keep' . ABT.substs xpnd) vbs
  lbd = rec keep' . ABT.substs xpnd $ bd
-- will be lifted, so keep this variable
enclose keep rec (Let1NamedTop' top v b@(LamsNamed' vs bd) e)
  = Just . let1' top [(v, lamb)] . rec (Set.insert v keep)
  $ ABT.subst v av e
  where
  (_, av) = expandSimple keep (v, b)
  keep' = Set.difference keep $ Set.fromList vs
  fvs = ABT.freeVars b
  evs = Set.toList $ Set.difference fvs keep
  a = ABT.annotation b
  lbody = rec keep' bd
  lamb = lam' a (evs ++ vs) lbody
enclose keep rec t@(LamsNamed' vs body)
  = Just $ if null evs then lamb else apps' lamb $ map (var a) evs
  where
  -- remove shadowed variables
  keep' = Set.difference keep $ Set.fromList vs
  fvs = ABT.freeVars t
  evs = Set.toList $ Set.difference fvs keep
  a = ABT.annotation t
  lbody = rec keep' body
  lamb = lam' a (evs ++ vs) lbody
enclose keep rec t@(Handle' h body)
  | isStructured body
  = Just . handle (ABT.annotation t) (rec keep h) $ apps' lamb args
  where
  fvs = ABT.freeVars body
  evs = Set.toList $ Set.difference fvs keep
  a = ABT.annotation body
  lbody = rec keep body
  fv = Var.freshIn fvs $ typed Var.Eta
  args | null evs = [constructor a Ty.unitRef 0]
       | otherwise = var a <$> evs
  lamb | null evs = lam' a [fv] lbody
       | otherwise = lam' a evs lbody
enclose _ _ _ = Nothing

isStructured :: Var v => Term v a -> Bool
isStructured (Var' _) = False
isStructured (Lam' _) = False
isStructured (Nat' _) = False
isStructured (Int' _) = False
isStructured (Float' _) = False
isStructured (Text' _) = False
isStructured (Char' _) = False
isStructured (Constructor' _ _) = False
isStructured (Apps' Constructor'{} args) = any isStructured args
isStructured (If' b t f)
  = isStructured b || isStructured t || isStructured f
isStructured (And' l r) = isStructured l || isStructured r
isStructured (Or' l r) = isStructured l || isStructured r
isStructured _ = True

close :: (Var v, Monoid a) => Set v -> Term v a -> Term v a
close keep tm = ABT.visitPure (enclose keep close) tm

type FloatM v a r = State (Set v, [(v, Term v a)]) r

freshFloat :: Var v => Set v -> v -> v
freshFloat avoid (Var.freshIn avoid -> v0)
  = case Var.typeOf v0 of
      Var.User nm
        | v <- typed (Var.User $ nm <> w) , v `Set.notMember` avoid
        -> v
        | otherwise
        -> freshFloat (Set.insert v0 avoid) v0
      _ -> v0
  where
  w = Text.pack . show $ Var.freshId v0

letFloater
  :: (Var v, Monoid a)
  => (Term v a -> FloatM v a (Term v a))
  -> [(v, Term v a)] -> Term v a
  -> FloatM v a (Term v a)
letFloater rec vbs e = do
  cvs <- gets fst
  let shadows = [ (v, freshFloat cvs v)
                | (v, _) <- vbs, Set.member v cvs ]
      shadowMap = Map.fromList shadows
      rn v = Map.findWithDefault v v shadowMap
      shvs = Set.fromList $ map (rn.fst) vbs
  modify (first $ (<>shvs))
  fvbs <- traverse (\(v, b) -> (,) (rn v) <$> rec' (ABT.changeVars shadowMap b)) vbs
  modify (second (++ fvbs))
  pure $ ABT.changeVars shadowMap e
  where
  rec' b@(LamsNamed' vs bd) = lam' (ABT.annotation b) vs <$> rec bd
  rec' b = rec b

lamFloater
  :: (Var v, Monoid a)
  => Maybe v -> a -> [v] -> Term v a -> FloatM v a v
lamFloater mv a vs bd
  = state $ \(cvs, ctx) ->
      let v = ABT.freshIn cvs $ fromMaybe (typed Var.Float) mv
       in (v, (Set.insert v cvs, ctx <> [(v, lam' a vs bd)]))

floater
  :: (Var v, Monoid a)
  => (Term v a -> FloatM v a (Term v a))
  -> Term v a -> Maybe (FloatM v a (Term v a))
floater rec (LetRecNamed' vbs e) = Just $ letFloater rec vbs e >>= rec
floater rec (Let1Named' v b e)
  | LamsNamed' vs bd <- b
  = Just $ rec bd
       >>= lamFloater (Just v) a vs
       >>= \lv -> rec $ ABT.changeVars (Map.singleton v lv) e
  where a = ABT.annotation b
floater rec tm@(LamsNamed' vs bd) = Just $ do
  bd <- rec bd
  lv <- lamFloater Nothing a vs bd
  pure $ var a lv
  where a = ABT.annotation tm
floater _ _ = Nothing

float :: (Var v, Monoid a) => Term v a -> Term v a
float tm = case runState (go tm) (Set.empty, []) of
  (bd, (_, ctx)) -> letRec' True ctx bd
  where
  go = ABT.visit $ floater go
  -- tm | LetRecNamedTop' _ vbs e <- tm0
  --    , (pre, rec, post) <- reduceCycle vbs
  --    = let1' False pre . letRec' False rec . let1' False post $ e
  --    | otherwise = tm0

deannotate :: Var v => Term v a -> Term v a
deannotate = ABT.visitPure $ \case
  Ann' c _ -> Just $ deannotate c
  _ -> Nothing

lamLift :: (Var v, Monoid a) => Term v a -> Term v a
lamLift = float . close Set.empty . deannotate

saturate
  :: (Var v, Monoid a)
  => Map (Reference,Int) Int -> Term v a -> Term v a
saturate dat = ABT.visitPure $ \case
  Apps' f@(Constructor' r t) args -> sat r t f args
  Apps' f@(Request' r t) args -> sat r t f args
  f@(Constructor' r t) -> sat r t f []
  f@(Request' r t) -> sat r t f []
  _ -> Nothing
  where
  frsh avoid _ =
    let v = Var.freshIn avoid $ typed Var.Eta
    in (Set.insert v avoid, v)
  sat r t f args = case Map.lookup (r,t) dat of
      Just n
        | m < n
        , vs <- snd $ mapAccumL frsh fvs [1..n-m]
        , nargs <- var mempty <$> vs
        -> Just . lam' mempty vs . apps' f $ args' ++ nargs
        | m > n
        , (sargs, eargs) <- splitAt n args'
        , sv <- Var.freshIn fvs $ typed Var.Eta
        -> Just
        . let1' False [(sv,apps' f sargs)]
        $ apps' (var mempty sv) eargs
      _ -> Just (apps' f args')
    where
    m = length args
    fvs = foldMap freeVars args
    args' = saturate dat <$> args

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
  go e@(List' vs) =
    if all isLeaf vs then e
    else fixup (ABT.freeVars e) (list (ann e)) (toList vs)
  go e@(Ann' tm typ) = Term.ann (ann e) (go tm) typ
  go e = error $ "ANF.term: I thought we got all of these\n" <> show e

data Mem = UN | BX deriving (Eq,Ord,Show,Enum)

-- Context entries with evaluation strategy
data CTE v s
  = ST (Direction Word16) [v] [Mem] s
  | LZ v (Either Reference v) [v]
  deriving (Show)

pattern ST1 d v m s = ST d [v] [m] s

data ANormalF v e
  = ALet (Direction Word16) [Mem] e e
  | AName (Either Reference v) [v] e
  | ALit Lit
  | AMatch v (Branched e)
  | AShift Reference e
  | AHnd [Reference] v e
  | AApp (Func v) [v]
  | AFrc v
  | AVar v
  deriving (Show)

-- Types representing components that will go into the runtime tag of
-- a data type value. RTags correspond to references, while CTags
-- correspond to constructors.
newtype RTag = RTag Word64 deriving (Eq,Ord,Show,Read,EC.EnumKey)
newtype CTag = CTag Word16 deriving (Eq,Ord,Show,Read,EC.EnumKey)

class Tag t where rawTag :: t -> Word64
instance Tag RTag where rawTag (RTag w) = w
instance Tag CTag where rawTag (CTag w) = fromIntegral w

packTags :: RTag -> CTag -> Word64
packTags (RTag rt) (CTag ct) = ri .|. ci
  where
  ri = rt `shiftL` 16
  ci = fromIntegral ct

unpackTags :: Word64 -> (RTag, CTag)
unpackTags w = (RTag $ w `shiftR` 16, CTag . fromIntegral $ w .&. 0xFFFF)

ensureRTag :: (Ord n, Show n, Num n) => String -> n -> r -> r
ensureRTag s n x
  | n > 0xFFFFFFFFFFFF = error $ s ++ "@RTag: too large: " ++ show n
  | otherwise = x

ensureCTag :: (Ord n, Show n, Num n) => String -> n -> r -> r
ensureCTag s n x
  | n > 0xFFFF = error $ s ++ "@CTag: too large: " ++ show n
  | otherwise = x

instance Enum RTag where
  toEnum i = ensureRTag "toEnum" i . RTag $ toEnum i
  fromEnum (RTag w) = fromEnum w

instance Enum CTag where
  toEnum i = ensureCTag "toEnum" i . CTag $ toEnum i
  fromEnum (CTag w) = fromEnum w

instance Num RTag where
  fromInteger i = ensureRTag "fromInteger" i . RTag $ fromInteger i
  (+) = error "RTag: +"
  (*) = error "RTag: *"
  abs = error "RTag: abs"
  signum = error "RTag: signum"
  negate = error "RTag: negate"

instance Num CTag where
  fromInteger i = ensureCTag "fromInteger" i . CTag $ fromInteger i
  (+) = error "CTag: +"
  (*) = error "CTag: *"
  abs = error "CTag: abs"
  signum = error "CTag: signum"
  negate = error "CTag: negate"

instance Functor (ANormalF v) where
  fmap _ (AVar v) = AVar v
  fmap _ (ALit l) = ALit l
  fmap f (ALet d m bn bo) = ALet d m (f bn) (f bo)
  fmap f (AName n as bo) = AName n as $ f bo
  fmap f (AMatch v br) = AMatch v $ f <$> br
  fmap f (AHnd rs h e) = AHnd rs h $ f e
  fmap f (AShift i e) = AShift i $ f e
  fmap _ (AFrc v) = AFrc v
  fmap _ (AApp f args) = AApp f args

instance Bifunctor ANormalF where
  bimap f _ (AVar v) = AVar (f v)
  bimap _ _ (ALit l) = ALit l
  bimap _ g (ALet d m bn bo) = ALet d m (g bn) (g bo)
  bimap f g (AName n as bo) = AName (f <$> n) (f <$> as) $ g bo
  bimap f g (AMatch v br) = AMatch (f v) $ fmap g br
  bimap f g (AHnd rs v e) = AHnd rs (f v) $ g e
  bimap _ g (AShift i e) = AShift i $ g e
  bimap f _ (AFrc v) = AFrc (f v)
  bimap f _ (AApp fu args) = AApp (fmap f fu) $ fmap f args

instance Bifoldable ANormalF where
  bifoldMap f _ (AVar v) = f v
  bifoldMap _ _ (ALit _) = mempty
  bifoldMap _ g (ALet _ _ b e) = g b <> g e
  bifoldMap f g (AName n as e) = foldMap f n <> foldMap f as <> g e
  bifoldMap f g (AMatch v br) = f v <> foldMap g br
  bifoldMap f g (AHnd _ h e) = f h <> g e
  bifoldMap _ g (AShift _ e) = g e
  bifoldMap f _ (AFrc v) = f v
  bifoldMap f _ (AApp func args) = foldMap f func <> foldMap f args

matchLit :: Term v a -> Maybe Lit
matchLit (Int' i) = Just $ I i
matchLit (Nat' n) = Just $ N n
matchLit (Float' f) = Just $ F f
matchLit (Text' t) = Just $ T t
matchLit (Char' c) = Just $ C c
matchLit _ = Nothing

pattern TLet d v m bn bo = ABTN.TTm (ALet d [m] bn (ABTN.TAbs v bo))
pattern TLetD v m bn bo = ABTN.TTm (ALet Direct [m] bn (ABTN.TAbs v bo))
pattern TLets d vs ms bn bo = ABTN.TTm (ALet d ms bn (ABTN.TAbss vs bo))
pattern TName v f as bo = ABTN.TTm (AName f as (ABTN.TAbs v bo))

pattern Lit' l <- (matchLit -> Just l)

pattern TLit l = ABTN.TTm (ALit l)
pattern TApp f args = ABTN.TTm (AApp f args)
pattern AApv v args = AApp (FVar v) args
pattern TApv v args = TApp (FVar v) args
pattern ACom r args = AApp (FComb r) args
pattern TCom r args = TApp (FComb r) args
pattern ACon r t args = AApp (FCon r t) args
pattern TCon r t args = TApp (FCon r t) args
pattern AKon v args = AApp (FCont v) args
pattern TKon v args = TApp (FCont v) args
pattern AReq r t args = AApp (FReq r t) args
pattern TReq r t args = TApp (FReq r t) args
pattern APrm p args = AApp (FPrim (Left p)) args
pattern TPrm p args = TApp (FPrim (Left p)) args
pattern AFOp p args = AApp (FPrim (Right p)) args
pattern TFOp p args = TApp (FPrim (Right p)) args

pattern THnd rs h b = ABTN.TTm (AHnd rs h b)
pattern TShift i v e = ABTN.TTm (AShift i (ABTN.TAbs v e))
pattern TMatch v cs = ABTN.TTm (AMatch v cs)
pattern TFrc v = ABTN.TTm (AFrc v)
pattern TVar v = ABTN.TTm (AVar v)

{-# complete
    TLet, TName, TVar, TApp, TFrc, TLit, THnd, TShift, TMatch
  #-}
{-# complete
      TLet, TName,
      TVar, TFrc,
      TApv, TCom, TCon, TKon, TReq, TPrm, TFOp,
      TLit, THnd, TShift, TMatch
  #-}

bind :: Var v => Cte v -> ANormal v -> ANormal v
bind (ST d us ms bu) = TLets d us ms bu
bind (LZ u f as) = TName u f as

unbind :: Var v => ANormal v -> Maybe (Cte v, ANormal v)
unbind (TLets d us ms bu bd) = Just (ST d us ms bu, bd)
unbind (TName u f as bd) = Just (LZ u f as, bd)
unbind _ = Nothing

unbinds :: Var v => ANormal v -> ([Cte v], ANormal v)
unbinds (TLets d us ms bu (unbinds -> (ctx, bd)))
  = (ST d us ms bu:ctx, bd)
unbinds (TName u f as (unbinds -> (ctx, bd))) = (LZ u f as:ctx, bd)
unbinds tm = ([], tm)

pattern TBind bn bd <- (unbind -> Just (bn, bd))
  where TBind bn bd = bind bn bd

pattern TBinds :: Var v => [Cte v] -> ANormal v -> ANormal v
pattern TBinds ctx bd <- (unbinds -> (ctx, bd))
  where TBinds ctx bd = foldr bind bd ctx
{-# complete TBinds #-}

data SeqEnd = SLeft | SRight
  deriving (Eq, Ord, Enum, Show)

data Branched e
  = MatchIntegral (EnumMap Word64 e) (Maybe e)
  | MatchText (Map.Map Text e) (Maybe e)
  | MatchRequest (Map Reference (EnumMap CTag ([Mem], e))) e
  | MatchEmpty
  | MatchData Reference (EnumMap CTag ([Mem], e)) (Maybe e)
  | MatchSum (EnumMap Word64 ([Mem], e))
  deriving (Show, Functor, Foldable, Traversable)

data BranchAccum v
  = AccumEmpty
  | AccumIntegral
      Reference
      (Maybe (ANormal v))
      (EnumMap Word64 (ANormal v))
  | AccumText
      (Maybe (ANormal v))
      (Map.Map Text (ANormal v))
  | AccumDefault (ANormal v)
  | AccumPure (ANormal v)
  | AccumRequest
      (Map Reference (EnumMap CTag ([Mem],ANormal v)))
      (Maybe (ANormal v))
  | AccumData
      Reference
      (Maybe (ANormal v))
      (EnumMap CTag ([Mem],ANormal v))
  | AccumSeqEmpty (ANormal v)
  | AccumSeqView
      SeqEnd
      (Maybe (ANormal v)) -- empty
      (ANormal v) -- cons/snoc
  | AccumSeqSplit
      SeqEnd
      Int -- split at
      (Maybe (ANormal v)) -- default
      (ANormal v) -- split

instance Semigroup (BranchAccum v) where
  AccumEmpty <> r = r
  l <> AccumEmpty = l
  AccumIntegral rl dl cl <> AccumIntegral rr dr cr
    | rl == rr = AccumIntegral rl (dl <|> dr) $ cl <> cr
  AccumText dl cl <> AccumText dr cr
    = AccumText (dl <|> dr) (cl <> cr)
  AccumData rl dl cl <> AccumData rr dr cr
    | rl == rr = AccumData rl (dl <|> dr) (cl <> cr)
  AccumDefault dl <> AccumIntegral r _ cr
    = AccumIntegral r (Just dl) cr
  AccumDefault dl <> AccumText _ cr
    = AccumText (Just dl) cr
  AccumDefault dl <> AccumData rr _ cr
    = AccumData rr (Just dl) cr
  AccumIntegral r dl cl <> AccumDefault dr
    = AccumIntegral r (dl <|> Just dr) cl
  AccumText dl cl <> AccumDefault dr
    = AccumText (dl <|> Just dr) cl
  AccumData rl dl cl <> AccumDefault dr
    = AccumData rl (dl <|> Just dr) cl
  l@(AccumPure _) <> AccumPure _ = l
  AccumPure dl <> AccumRequest hr _ = AccumRequest hr (Just dl)
  AccumRequest hl dl <> AccumPure dr
    = AccumRequest hl (dl <|> Just dr)
  AccumRequest hl dl <> AccumRequest hr dr
    = AccumRequest hm $ dl <|> dr
    where
    hm = Map.unionWith (<>) hl hr
  l@(AccumSeqEmpty _) <> AccumSeqEmpty _ = l
  AccumSeqEmpty eml <> AccumSeqView er _ cnr
    = AccumSeqView er (Just eml) cnr
  AccumSeqView el eml cnl <> AccumSeqEmpty emr
    = AccumSeqView el (eml <|> Just emr) cnl
  AccumSeqView el eml cnl <> AccumSeqView er emr _
    | el /= er
    = error "AccumSeqView: trying to merge views of opposite ends"
    | otherwise = AccumSeqView el (eml <|> emr) cnl
  AccumSeqView _ _ _ <> AccumDefault _
    = error "seq views may not have defaults"
  AccumDefault _ <> AccumSeqView _ _ _
    = error "seq views may not have defaults"
  AccumSeqSplit el nl dl bl <> AccumSeqSplit er nr dr _
    | el /= er
    = error "AccumSeqSplit: trying to merge splits at opposite ends"
    | nl /= nr
    = error
        "AccumSeqSplit: trying to merge splits at different positions"
    | otherwise
    = AccumSeqSplit el nl (dl <|> dr) bl
  AccumDefault dl <> AccumSeqSplit er nr _ br
    = AccumSeqSplit er nr (Just dl) br
  AccumSeqSplit el nl dl bl <> AccumDefault dr
    = AccumSeqSplit el nl (dl <|> Just dr) bl
  _ <> _ = error $ "cannot merge data cases for different types"

instance Monoid (BranchAccum e) where
  mempty = AccumEmpty

-- Foreign operation, indexed by words
type FOp = Word64

data Func v
  -- variable
  = FVar v
  -- top-level combinator
  | FComb !Reference
  -- continuation jump
  | FCont v
  -- data constructor
  | FCon !Reference !CTag
  -- ability request
  | FReq !Reference !CTag
  -- prim op
  | FPrim (Either POp FOp)
  deriving (Show, Functor, Foldable, Traversable)

data Lit
  = I Int64
  | N Word64
  | F Double
  | T Text
  | C Char
  | LM Referent
  | LY Reference
  deriving (Show)

litRef :: Lit -> Reference
litRef (I _) = Ty.intRef
litRef (N _) = Ty.natRef
litRef (F _) = Ty.floatRef
litRef (T _) = Ty.textRef
litRef (C _) = Ty.charRef
litRef (LM _) = Ty.termLinkRef
litRef (LY _) = Ty.typeLinkRef

data POp
  -- Int
  = ADDI | SUBI | MULI | DIVI -- +,-,*,/
  | SGNI | NEGI | MODI        -- sgn,neg,mod
  | POWI | SHLI | SHRI        -- pow,shiftl,shiftr
  | INCI | DECI | LEQI | EQLI -- inc,dec,<=,==
  -- Nat
  | ADDN | SUBN | MULN | DIVN -- +,-,*,/
  | MODN | TZRO | LZRO | POPC -- mod,trailing/leadingZeros,popCount
  | POWN | SHLN | SHRN        -- pow,shiftl,shiftr
  | ANDN | IORN | XORN | COMN -- and,or,xor,complement
  | INCN | DECN | LEQN | EQLN -- inc,dec,<=,==
  -- Float
  | ADDF | SUBF | MULF | DIVF -- +,-,*,/
  | MINF | MAXF | LEQF | EQLF -- min,max,<=,==
  | POWF | EXPF | SQRT | LOGF -- pow,exp,sqrt,log
  | LOGB                      -- logBase
  | ABSF | CEIL | FLOR | TRNF -- abs,ceil,floor,truncate
  | RNDF                      -- round
  -- Trig
  | COSF | ACOS | COSH | ACSH -- cos,acos,cosh,acosh
  | SINF | ASIN | SINH | ASNH -- sin,asin,sinh,asinh
  | TANF | ATAN | TANH | ATNH -- tan,atan,tanh,atanh
  | ATN2                      -- atan2
  -- Text
  | CATT | TAKT | DRPT | SIZT -- ++,take,drop,size
  | UCNS | USNC | EQLT | LEQT -- uncons,unsnoc,==,<=
  | PAKT | UPKT               -- pack,unpack
  -- Sequence
  | CATS | TAKS | DRPS | SIZS -- ++,take,drop,size
  | CONS | SNOC | IDXS | BLDS -- cons,snoc,at,build
  | VWLS | VWRS | SPLL | SPLR -- viewl,viewr,splitl,splitr
  -- Bytes
  | PAKB | UPKB | TAKB | DRPB -- pack,unpack,take,drop
  | IDXB | SIZB | FLTB | CATB -- index,size,flatten,append
  -- Conversion
  | ITOF | NTOF | ITOT | NTOT
  | TTOI | TTON | TTOF | FTOT
  -- Concurrency
  | FORK
  -- Universal operations
  | EQLU | CMPU | EROR
  -- Code
  | MISS | CACH | LKUP | LOAD -- isMissing,cache_,lookup,load
  | VALU                      -- value
  -- Debug
  | PRNT | INFO
  -- STM
  | ATOM
  deriving (Show,Eq,Ord)

type ANormal = ABTN.Term ANormalF

type Cte v = CTE v (ANormal v)
type Ctx v = Directed () [Cte v]

data Direction a = Indirect a | Direct
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

directed :: Foldable f => f (Cte v) -> Directed () (f (Cte v))
directed x = (foldMap f x, x)
  where
  f (ST d _ _ _) = () <$ d
  f _ = Direct

instance Semigroup a => Semigroup (Direction a) where
  Indirect l <> Indirect r = Indirect $ l <> r
  Direct <> r = r
  l <> Direct = l

instance Semigroup a => Monoid (Direction a) where
  mempty = Direct

type Directed a = (,) (Direction a)

type DNormal v = Directed () (ANormal v)

-- Should be a completely closed term
data SuperNormal v
  = Lambda { conventions :: [Mem], bound :: ANormal v }
  deriving (Show)
data SuperGroup v
  = Rec
  { group :: [(v, SuperNormal v)]
  , entry :: SuperNormal v
  } deriving (Show)

type ANFM v
  = ReaderT (Set v)
      (State (Word64, Word16, [(v, SuperNormal v)]))

type ANFD v = Compose (ANFM v) (Directed ())

data GroupRef = GR Reference Word64
  deriving (Show)

data Value
  = Partial GroupRef [Word64] [Value]
  | Data Reference Word64 [Word64] [Value]
  | Cont [Word64] [Value] Cont
  | BLit BLit
  deriving (Show)

data Cont
  = KE
  | Mark [Reference] (Map Reference Value) Cont
  | Push Word64 Word64 Word64 Word64 GroupRef Cont
  deriving (Show)

data BLit
  = Text Text
  | List (Seq Value)
  | TmLink Referent
  | TyLink Reference
  | Bytes Bytes
  deriving (Show)

groupVars :: ANFM v (Set v)
groupVars = ask

bindLocal :: Ord v => [v] -> ANFM v r -> ANFM v r
bindLocal vs = local (Set.\\ Set.fromList vs)

freshANF :: Var v => Word64 -> v
freshANF fr = Var.freshenId fr $ typed Var.ANFBlank

fresh :: Var v => ANFM v v
fresh = state $ \(fr, bnd, cs) -> (freshANF fr, (fr+1, bnd, cs))

contextualize :: Var v => DNormal v -> ANFM v (Ctx v, v)
contextualize (_, TVar cv) = do
  gvs <- groupVars
  if cv `Set.notMember` gvs
    then pure (pure [], cv)
    else do bv <- fresh
            d <- Indirect <$> binder
            pure (directed [ST1 d bv BX $ TApv cv []], bv)
contextualize (d0, tm) = do
  fv <- fresh
  d <- bindDirection d0
  pure ((d0, [ST1 d fv BX tm]), fv)

binder :: ANFM v Word16
binder = state $ \(fr, bnd, cs) -> (bnd, (fr, bnd+1, cs))

bindDirection :: Direction a -> ANFM v (Direction Word16)
bindDirection = traverse (const binder)

record :: Var v => (v, SuperNormal v) -> ANFM v ()
record p = modify $ \(fr, bnd, to) -> (fr, bnd, p:to)

superNormalize :: Var v => Term v a -> SuperGroup v
superNormalize tm = Rec l c
  where
  (bs, e) | LetRecNamed' bs e <- tm = (bs, e)
          | otherwise = ([], tm)
  grp = Set.fromList $ fst <$> bs
  comp = traverse_ superBinding bs *> toSuperNormal e
  subc = runReaderT comp grp
  (c, (_,_,l)) = runState subc (0, 1, [])

superBinding :: Var v => (v, Term v a) -> ANFM v ()
superBinding (v, tm) = do
  nf <- toSuperNormal tm
  modify $ \(cvs, bnd, ctx) -> (cvs, bnd, (v,nf):ctx)

toSuperNormal :: Var v => Term v a -> ANFM v (SuperNormal v)
toSuperNormal tm = do
  grp <- groupVars
  if not . Set.null . (Set.\\ grp) $ freeVars tm
    then error $ "free variables in supercombinator: " ++ show tm
    else Lambda (BX<$vs) . ABTN.TAbss vs . snd
           <$> bindLocal vs (anfTerm body)
  where
  (vs, body) = fromMaybe ([], tm) $ unLams' tm

anfTerm :: Var v => Term v a -> ANFM v (DNormal v)
anfTerm tm = f <$> anfBlock tm
  where
  -- f = uncurry (liftA2 TBinds)
  f ((_,[]), dtm) = dtm
  f ((_,cx),(_,tm)) = (Indirect (), TBinds cx tm)

floatableCtx :: Var v => Ctx v -> Bool
floatableCtx = all p . snd
  where
  p (LZ _ _ _) = True
  p (ST _ _ _ tm) = q tm
  q (TLit _) = True
  q (TVar _) = True
  q (TCon _ _ _) = True
  q _ = False

anfHandled :: Var v => Term v a -> ANFM v (Ctx v, DNormal v)
anfHandled body = anfBlock body >>= \case
  (ctx, (_, t@TCon{}))
      -> fresh <&> \v ->
           (ctx <> pure [ST1 Direct v BX t], pure $ TVar v)
  (ctx, (_, t@(TLit l)))
      -> fresh <&> \v ->
           (ctx <> pure [ST1 Direct v cc t], pure $ TVar v)
    where
    cc = case l of T{} -> BX ; LM{} -> BX ; LY{} -> BX ; _ -> UN
  p -> pure p

fls, tru :: Var v => ANormal v
fls = TCon Ty.booleanRef 0 []
tru = TCon Ty.booleanRef 0 []

anfBlock :: Var v => Term v a -> ANFM v (Ctx v, DNormal v)
anfBlock (Var' v) = pure (mempty, pure $ TVar v)
anfBlock (If' c t f) = do
  (cctx, cc) <- anfBlock c
  (df, cf) <- anfTerm f
  (dt, ct) <- anfTerm t
  (cx, v) <- contextualize cc
  let cases = MatchData
                (Builtin $ Text.pack "Boolean")
                (EC.mapSingleton 0 ([], cf))
                (Just ct)
  pure (cctx <> cx, (Indirect () <> df <> dt, TMatch v cases))
anfBlock (And' l r) = do
  (lctx, vl) <- anfArg l
  (d, tmr) <- anfTerm r
  let tree = TMatch vl . flip (MatchData Ty.booleanRef) Nothing
           $ mapFromList
           [ (0, ([], fls))
           , (1, ([], tmr))
           ]
  pure (lctx, (Indirect () <> d, tree))
anfBlock (Or' l r) = do
  (lctx, vl) <- anfArg l
  (d, tmr) <- anfTerm r
  let tree = TMatch vl . flip (MatchData Ty.booleanRef) Nothing
           $ mapFromList
           [ (1, ([], tru))
           , (0, ([], tmr))
           ]
  pure (lctx, (Indirect () <> d, tree))
anfBlock (Handle' h body)
  = anfArg h >>= \(hctx, vh) ->
    anfHandled body >>= \case
      (ctx, (_, TCom f as)) | floatableCtx ctx -> do
        v <- fresh
        pure ( hctx <> ctx <> pure [LZ v (Left f) as]
             , (Indirect (), TApp (FVar vh) [v]))
      (ctx, (_, TApv f as)) | floatableCtx ctx -> do
        v <- fresh
        pure ( hctx <> ctx <> pure [LZ v (Right f) as]
             , (Indirect (), TApp (FVar vh) [v]))
      (ctx, (_, TVar v)) | floatableCtx ctx -> do
        pure (hctx <> ctx, (Indirect (), TApp (FVar vh) [v]))
      p@(_, _) ->
        error $ "handle body should be a simple call: " ++ show p
anfBlock (Match' scrut cas) = do
  (sctx, sc) <- anfBlock scrut
  (cx, v) <- contextualize sc
  (d, brn) <- anfCases v cas
  fmap (first ((Indirect () <> d) <>)) <$> case brn of
    AccumDefault (TBinds (directed -> dctx) df) -> do
      pure (sctx <> cx <> dctx, pure df)
    AccumRequest _ Nothing ->
      error "anfBlock: AccumRequest without default"
    AccumPure (ABTN.TAbss us bd)
      | [u] <- us
      , TBinds (directed -> bx) bd <- bd
     -> case cx of
          (_, []) -> do
            d0 <- Indirect <$> binder
            pure (sctx <> pure [ST1 d0 u BX (TFrc v)] <> bx, pure bd)
          (d0, [ST1 d1 _ BX tm]) ->
            pure (sctx <> (d0, [ST1 d1 u BX tm]) <> bx, pure bd)
          _ -> error "anfBlock|AccumPure: impossible"
      | otherwise -> error "pure handler with too many variables"
    AccumRequest abr (Just df) -> do
      (r, vs) <- do
        r <- fresh
        v <- fresh
        gvs <- groupVars
        let hfb = ABTN.TAbs v . TMatch v $ MatchRequest abr df
            hfvs = Set.toList $ ABTN.freeVars hfb `Set.difference` gvs
        record (r, Lambda (BX <$ hfvs ++ [v]) . ABTN.TAbss hfvs $ hfb)
        pure (r, hfvs)
      hv <- fresh
      let (d, msc)
            | (d, [ST1 _ _ BX tm]) <- cx = (d, tm)
            | (_, [ST _ _ _ _]) <- cx = error "anfBlock: impossible"
            | otherwise = (Indirect (), TFrc v)
      pure ( sctx <> pure [LZ hv (Right r) vs]
           , (d, THnd (Map.keys abr) hv msc)
           )
    AccumText df cs ->
      pure (sctx <> cx, pure . TMatch v $ MatchText cs df)
    AccumIntegral r df cs -> do
      i <- fresh
      let dcs = MatchData r
                  (EC.mapSingleton 0 ([UN], ABTN.TAbss [i] ics))
                  Nothing
          ics = TMatch i $ MatchIntegral cs df
      pure (sctx <> cx, pure $ TMatch v dcs)
    AccumData r df cs ->
      pure (sctx <> cx, pure . TMatch v $ MatchData r cs df)
    AccumSeqEmpty _ ->
      error "anfBlock: non-exhaustive AccumSeqEmpty"
    AccumSeqView en (Just em) bd -> do
      r <- fresh
      let op | SLeft <- en = Builtin "List.viewl"
             | otherwise   = Builtin "List.viewr"
      b <- binder
      pure ( sctx <> cx
               <> (Indirect (), [ST1 (Indirect b) r BX (TCom op [v])])
           , pure . TMatch r
           $ MatchData Ty.seqViewRef
               (EC.mapFromList
                  [ (0, ([], em))
                  , (1, ([BX,BX], bd))
                  ]
               )
               Nothing
           )
    AccumSeqView {} ->
      error "anfBlock: non-exhaustive AccumSeqView"
    AccumSeqSplit en n mdf bd -> do
        i <- fresh
        r <- fresh
        t <- fresh
        pure ( sctx <> cx <> directed [lit i, split i r]
             , pure . TMatch r . MatchSum $ mapFromList
             [ (0, ([], df t))
             , (1, ([BX,BX], bd))
             ])
      where
      op | SLeft <- en = SPLL
         | otherwise = SPLR
      lit i = ST1 Direct i UN (TLit . N $ fromIntegral n)
      split i r = ST1 Direct r UN (TPrm op [i,v])
      df t
        = fromMaybe
            ( TLet Direct t BX (TLit (T "non-exhaustive split"))
            $ TPrm EROR [t])
            mdf
    AccumEmpty -> pure (sctx <> cx, pure $ TMatch v MatchEmpty)
anfBlock (Let1Named' v b e)
  = anfBlock b >>= \(bctx, (d0, cb)) -> bindLocal [v] $ do
      (ectx, ce) <- anfBlock e
      d <- bindDirection d0
      let octx = bctx <> directed [ST1 d v BX cb] <> ectx
      pure (octx, ce)
anfBlock (Apps' f args) = do
  (fctx, (d, cf)) <- anfFunc f
  (actx, cas) <- anfArgs args
  pure (fctx <> actx, (d, TApp cf cas))
anfBlock (Constructor' r t)
  = pure (mempty, pure $ TCon r (toEnum t) [])
anfBlock (Request' r t)
  = pure (mempty, (Indirect (), TReq r (toEnum t) []))
anfBlock (Boolean' b)
  = pure (mempty, pure $ TCon Ty.booleanRef (if b then 1 else 0) [])
anfBlock (Lit' l@(T _)) =
  pure (mempty, pure $ TLit l)
anfBlock (Lit' l) = do
  lv <- fresh
  pure ( directed [ST1 Direct lv UN $ TLit l]
       , pure $ TCon (litRef l) 0 [lv])
anfBlock (Ref' r) = pure (mempty, (Indirect (), TCom r []))
anfBlock (Blank' _) = do
  ev <- fresh
  pure ( pure [ST1 Direct ev BX (TLit (T "Blank"))]
       , pure $ TPrm EROR [ev])
anfBlock (TermLink' r) = pure (mempty, pure . TLit $ LM r)
anfBlock (TypeLink' r) = pure (mempty, pure . TLit $ LY r)
anfBlock (List' as) = fmap (pure . TPrm BLDS) <$> anfArgs tms
  where
  tms = toList as
anfBlock t = error $ "anf: unhandled term: " ++ show t

-- Note: this assumes that patterns have already been translated
-- to a state in which every case matches a single layer of data,
-- with no guards, and no variables ignored. This is not checked
-- completely.
anfInitCase
  :: Var v
  => v
  -> MatchCase p (Term v a)
  -> ANFD v (BranchAccum v)
anfInitCase u (MatchCase p guard (ABT.AbsN' vs bd))
  | Just _ <- guard = error "anfInitCase: unexpected guard"
  | P.Unbound _ <- p
  , [] <- vs
  = AccumDefault <$> anfBody bd
  | P.Var _ <- p
  , [v] <- vs
  = AccumDefault . ABTN.rename v u <$> anfBody bd
  | P.Var _ <- p
  = error $ "vars: " ++ show (length vs)
  | P.Int _ (fromIntegral -> i) <- p
  = AccumIntegral Ty.intRef Nothing . EC.mapSingleton i <$> anfBody bd
  | P.Nat _ i <- p
  = AccumIntegral Ty.natRef Nothing . EC.mapSingleton i <$> anfBody bd
  | P.Char _ c <- p
  , w <- fromIntegral $ fromEnum c
  = AccumIntegral Ty.charRef Nothing . EC.mapSingleton w <$> anfBody bd
  | P.Boolean _ b <- p
  , t <- if b then 1 else 0
  = AccumData Ty.booleanRef Nothing
  . EC.mapSingleton t . ([],) <$> anfBody bd
  | P.Text _ t <- p
  , [] <- vs
  = AccumText Nothing . Map.singleton t <$> anfBody bd
  | P.Constructor _ r t ps <- p = do
    (,) <$> expandBindings ps vs <*> anfBody bd <&> \(us,bd)
      -> AccumData r Nothing
       . EC.mapSingleton (toEnum t)
       . (BX<$us,)
       . ABTN.TAbss us
       $ bd
  | P.EffectPure _ q <- p =
    (,) <$> expandBindings [q] vs <*> anfBody bd <&> \(us,bd) ->
      AccumPure $ ABTN.TAbss us bd
  | P.EffectBind _ r t ps pk <- p = do
    (,,) <$> expandBindings (snoc ps pk) vs
         <*> Compose (pure <$> fresh)
         <*> anfBody bd
      <&> \(exp,kf,bd) ->
        let (us, uk)
              = maybe (error "anfInitCase: unsnoc impossible") id
              $ unsnoc exp
            jn = Builtin "jumpCont"
         in flip AccumRequest Nothing
          . Map.singleton r
          . EC.mapSingleton (toEnum t)
          . (BX<$us,)
          . ABTN.TAbss us
          . TShift r kf
          . TName uk (Left jn) [kf]
          $ bd
  | P.SequenceLiteral _ [] <- p
  = AccumSeqEmpty <$> anfBody bd
  | P.SequenceOp _ l op r <- p
  , Concat <- op
  , P.SequenceLiteral p ll <- l = do
    AccumSeqSplit SLeft (length ll) Nothing
      <$> (ABTN.TAbss <$> expandBindings [P.Var p, r] vs <*> anfBody bd)
  | P.SequenceOp _ l op r <- p
  , Concat <- op
  , P.SequenceLiteral p rl <- r =
    AccumSeqSplit SLeft (length rl) Nothing
      <$> (ABTN.TAbss <$> expandBindings [l, P.Var p] vs <*> anfBody bd)
  | P.SequenceOp _ l op r <- p
  , dir <- case op of Cons -> SLeft ; _ -> SRight
  = AccumSeqView dir Nothing
      <$> (ABTN.TAbss <$> expandBindings [l,r] vs <*> anfBody bd)
  where
  anfBody tm = Compose . bindLocal vs $ anfTerm tm
anfInitCase _ (MatchCase p _ _)
  = error $ "anfInitCase: unexpected pattern: " ++ show p

valueTermLinks :: Value -> [Reference]
valueTermLinks = Set.toList . valueLinks f
  where
  f False r = Set.singleton r
  f _ _ = Set.empty

valueLinks :: Monoid a => (Bool -> Reference -> a) -> Value -> a
valueLinks f (Partial (GR cr _) _ bs)
  = f False cr <> foldMap (valueLinks f) bs
valueLinks f (Data dr _ _ bs)
  = f True dr <> foldMap (valueLinks f) bs
valueLinks f (Cont _ bs k)
  = foldMap (valueLinks f) bs <> contLinks f k
valueLinks f (BLit l) = litLinks f l

contLinks :: Monoid a => (Bool -> Reference -> a) -> Cont -> a
contLinks f (Push _ _ _ _ (GR cr _) k)
  = f False cr <> contLinks f k
contLinks f (Mark ps de k)
  = foldMap (f True) ps
  <> Map.foldMapWithKey (\k c -> f True k <> valueLinks f c) de
  <> contLinks f k
contLinks _ KE = mempty

litLinks :: Monoid a => (Bool -> Reference -> a) -> BLit -> a
litLinks _ (Text _) = mempty
litLinks _ (Bytes _) = mempty
litLinks f (List s) = foldMap (valueLinks f) s
litLinks f (TmLink (Ref r)) = f False r
litLinks f (TmLink (Con r _ _)) = f True r
litLinks f (TyLink r) = f True r

groupTermLinks :: SuperGroup v -> [Reference]
groupTermLinks = Set.toList . groupLinks f
  where
  f False r = Set.singleton r
  f _ _ = Set.empty

groupLinks :: Monoid a => (Bool -> Reference -> a) -> SuperGroup v -> a
groupLinks f (Rec bs e)
  = foldMap (foldMap (normalLinks f)) bs <> normalLinks f e

normalLinks
  :: Monoid a => (Bool -> Reference -> a) -> SuperNormal v -> a
normalLinks f (Lambda _ e) = anfLinks f e

anfLinks :: Monoid a => (Bool -> Reference -> a) -> ANormal v -> a
anfLinks f (ABTN.Term _ (ABTN.Abs _ e)) = anfLinks f e
anfLinks f (ABTN.Term _ (ABTN.Tm e)) = anfFLinks f (anfLinks f) e

anfFLinks
  :: Monoid a
  => (Bool -> Reference -> a)
  -> (e -> a)
  -> ANormalF v e
  -> a
anfFLinks _ g (ALet _ _ b e) = g b <> g e
anfFLinks f g (AName er _ e)
  = bifoldMap (f False) (const mempty) er <> g e
anfFLinks f g (AMatch _ bs) = branchLinks (f True) g bs
anfFLinks f g (AShift r e) = f True r <> g e
anfFLinks f g (AHnd rs _ e) = foldMap (f True) rs <> g e
anfFLinks f _ (AApp fu _) = funcLinks (f False) fu
anfFLinks _ _ _ = mempty

branchLinks
  :: Monoid a
  => (Reference -> a)
  -> (e -> a)
  -> Branched e
  -> a
branchLinks f g bs = tyRefs f bs <> foldMap g bs

tyRefs :: Monoid a => (Reference -> a) -> Branched e -> a
tyRefs f (MatchRequest m _) = foldMap f (Map.keys m)
tyRefs f (MatchData r _ _) = f r
tyRefs _ _ = mempty

funcLinks
  :: Monoid a
  => (Reference -> a)
  -> Func v -> a
funcLinks f (FComb r) = f r
funcLinks _ _ = mempty

expandBindings'
  :: Var v
  => Word64
  -> [P.Pattern p]
  -> [v]
  -> Either String (Word64, [v])
expandBindings' fr [] [] = Right (fr, [])
expandBindings' fr (P.Unbound _:ps) vs
  = fmap (u :) <$> expandBindings' (fr+1) ps vs
  where u = freshANF fr
expandBindings' fr (P.Var _:ps) (v:vs)
  = fmap (v :) <$> expandBindings' fr ps vs
expandBindings' _ [] (_:_)
  = Left "expandBindings': more bindings than expected"
expandBindings' _ (_:_) []
  = Left "expandBindings': more patterns than expected"
expandBindings' _ _ _
  = Left $ "expandBindings': unexpected pattern"

expandBindings :: Var v => [P.Pattern p] -> [v] -> ANFD v [v]
expandBindings ps vs
  = Compose . state $ \(fr,bnd,co) -> case expandBindings' fr ps vs of
      Left err -> error $ err ++ " " ++ show (ps, vs)
      Right (fr,l) -> (pure l, (fr,bnd,co))

anfCases
  :: Var v
  => v
  -> [MatchCase p (Term v a)]
  -> ANFM v (Directed () (BranchAccum v))
anfCases u = getCompose . fmap fold . traverse (anfInitCase u)

anfFunc :: Var v => Term v a -> ANFM v (Ctx v, Directed () (Func v))
anfFunc (Var' v) = pure (mempty, (Indirect (), FVar v))
anfFunc (Ref' r) = pure (mempty, (Indirect (), FComb r))
anfFunc (Constructor' r t) = pure (mempty, (Direct, FCon r $ toEnum t))
anfFunc (Request' r t) = pure (mempty, (Indirect (), FReq r $ toEnum t))
anfFunc tm = do
  (fctx, ctm) <- anfBlock tm
  (cx, v) <- contextualize ctm
  pure (fctx <> cx, (Indirect (), FVar v))

anfArg :: Var v => Term v a -> ANFM v (Ctx v, v)
anfArg tm = do
  (ctx, ctm) <- anfBlock tm
  (cx, v) <- contextualize ctm
  pure (ctx <> cx, v)

anfArgs :: Var v => [Term v a] -> ANFM v (Ctx v, [v])
anfArgs tms = first fold . unzip <$> traverse anfArg tms

indent :: Int -> ShowS
indent ind = showString (replicate (ind*2) ' ')

prettyGroup :: Var v => String -> SuperGroup v -> ShowS
prettyGroup s (Rec grp ent)
  = showString ("let rec[" ++ s ++ "]\n")
  . foldr f id grp
  . showString "entry"
  . prettySuperNormal 1 ent
  where
  f (v,sn) r = indent 1 . pvar v
             . prettySuperNormal 2 sn . showString "\n" . r

pvar :: Var v => v -> ShowS
pvar v = showString . Text.unpack $ Var.name v

prettyVars :: Var v => [v] -> ShowS
prettyVars
  = foldr (\v r -> showString " " . pvar v . r) id

prettyLVars :: Var v => [Mem] -> [v] -> ShowS
prettyLVars [] [] = showString " "
prettyLVars (c:cs) (v:vs)
  = showString " "
  . showParen True (pvar v . showString ":" . shows c)
  . prettyLVars cs vs

prettyLVars [] (_:_) = error "more variables than conventions"
prettyLVars (_:_) [] = error "more conventions than variables"

prettyRBind :: Var v => [v] -> ShowS
prettyRBind [] = showString "()"
prettyRBind [v] = pvar v
prettyRBind (v:vs)
  = showParen True
  $ pvar v . foldr (\v r -> shows v . showString "," . r) id vs

prettySuperNormal :: Var v => Int -> SuperNormal v -> ShowS
prettySuperNormal ind (Lambda ccs (ABTN.TAbss vs tm))
  = prettyLVars ccs vs
  . showString "="
  . prettyANF False (ind+1) tm

reqSpace :: Var v => Bool -> ANormal v -> Bool
reqSpace _ TLets{} = True
reqSpace _ TName{} = True
reqSpace b _ = b

prettyANF :: Var v => Bool -> Int -> ANormal v -> ShowS
prettyANF m ind tm = prettySpace (reqSpace m tm) ind . case tm of
  TLets _ vs _ bn bo
    -> prettyRBind vs
     . showString " ="
     . prettyANF False (ind+1) bn
     . prettyANF True ind bo
  TName v f vs bo
    -> prettyRBind [v]
     . showString " := "
     . prettyLZF f
     . prettyVars vs
     . prettyANF True ind bo
  TLit l -> shows l
  TFrc v -> showString "!" . pvar v
  TVar v -> pvar v
  TApp f vs -> prettyFunc f . prettyVars vs
  TMatch v bs
    -> showString "match "
     . pvar v . showString " with"
     . prettyBranches (ind+1) bs
  TShift r v bo
    -> showString "shift[" . shows r . showString "]"
     . prettyVars [v] . showString "."
     . prettyANF False (ind+1) bo
  THnd rs v bo
    -> showString "handle" . prettyRefs rs
     . prettyANF False (ind+1) bo
     . showString " with " . pvar v
  _ -> shows tm

prettySpace :: Bool -> Int -> ShowS
prettySpace False _   = showString " "
prettySpace True  ind = showString "\n" . indent ind

prettyLZF :: Var v => Either Reference v -> ShowS
prettyLZF (Left w) = showString "ENV(" . shows w . showString ") "
prettyLZF (Right v) = pvar v . showString " "

prettyRefs :: [Reference] -> ShowS
prettyRefs [] = showString "{}"
prettyRefs (r:rs)
  = showString "{" . shows r
  . foldr (\t r -> shows t . showString "," . r) id rs
  . showString "}"

prettyFunc :: Var v => Func v -> ShowS
prettyFunc (FVar v) = pvar v . showString " "
prettyFunc (FCont v) = pvar v . showString " "
prettyFunc (FComb w) = showString "ENV(" . shows w . showString ")"
prettyFunc (FCon r t)
  = showString "CON("
  . shows r . showString "," . shows t
  . showString ")"
prettyFunc (FReq r t)
  = showString "REQ("
  . shows r . showString "," . shows t
  . showString ")"
prettyFunc (FPrim op) = either shows shows op . showString " "

prettyBranches :: Var v => Int -> Branched (ANormal v) -> ShowS
prettyBranches ind bs = case bs of
  MatchEmpty -> showString "{}"
  MatchIntegral bs df
    -> maybe id (\e -> prettyCase ind (showString "_") e id)  df
     . foldr (uncurry $ prettyCase ind . shows) id (mapToList bs)
  MatchText bs df
    -> maybe id (\e -> prettyCase ind (showString "_") e id)  df
     . foldr (uncurry $ prettyCase ind . shows) id (Map.toList bs)
  MatchData _ bs df
    -> maybe id (\e -> prettyCase ind (showString "_") e id)  df
     . foldr (uncurry $ prettyCase ind . shows) id
         (mapToList $ snd <$> bs)
  MatchRequest bs df
    -> foldr (\(r,m) s ->
         foldr (\(c,e) -> prettyCase ind (prettyReq r c) e)
           s (mapToList $ snd <$> m))
         (prettyCase ind (prettyReq 0 0) df id) (Map.toList bs)
  MatchSum bs
    -> foldr (uncurry $ prettyCase ind . shows) id
         (mapToList $ snd <$> bs)
  -- _ -> error "prettyBranches: todo"
  where
  prettyReq r c
    = showString "REQ("
    . shows r . showString "," . shows c
    . showString ")"

prettyCase :: Var v => Int -> ShowS -> ANormal v -> ShowS -> ShowS
prettyCase ind sc (ABTN.TAbss vs e) r
  = showString "\n" . indent ind . sc . prettyVars vs
  . showString " ->" . prettyANF False (ind+1) e . r
