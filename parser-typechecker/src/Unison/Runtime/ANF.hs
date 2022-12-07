{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Runtime.ANF
  ( minimizeCyclesOrCrash,
    pattern TVar,
    pattern TLit,
    pattern TApp,
    pattern TApv,
    pattern TCom,
    pattern TCon,
    pattern TKon,
    pattern TReq,
    pattern TPrm,
    pattern TFOp,
    pattern THnd,
    pattern TLet,
    pattern TLetD,
    pattern TFrc,
    pattern TLets,
    pattern TName,
    pattern TBind,
    pattern TBinds,
    pattern TShift,
    pattern TMatch,
    CompileExn (..),
    internalBug,
    Mem (..),
    Lit (..),
    Direction (..),
    SuperNormal (..),
    SuperGroup (..),
    POp (..),
    FOp,
    close,
    saturate,
    float,
    lamLift,
    inlineAlias,
    addDefaultCases,
    ANormalF (.., AApv, ACom, ACon, AKon, AReq, APrm, AFOp),
    ANormal,
    RTag,
    CTag,
    Tag (..),
    GroupRef (..),
    Value (..),
    Cont (..),
    BLit (..),
    packTags,
    unpackTags,
    maskTags,
    ANFM,
    Branched (.., MatchDataCover),
    Func (..),
    superNormalize,
    anfTerm,
    valueTermLinks,
    valueLinks,
    groupTermLinks,
    groupLinks,
    normalLinks,
    prettyGroup,
  )
where

import Control.Exception (throw)
import Control.Lens (snoc, unsnoc)
import Control.Monad.Reader (ReaderT (..), ask, local)
import Control.Monad.State (MonadState (..), State, gets, modify, runState)
import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Functor.Compose (Compose (..))
import Data.List hiding (and, or)
import qualified Data.Map as Map
import qualified Data.Primitive as PA
import qualified Data.Set as Set
import qualified Data.Text as Data.Text
import GHC.Stack (CallStack, callStack)
import qualified Unison.ABT as ABT
import qualified Unison.ABT.Normalized as ABTN
import Unison.Blank (nameb)
import qualified Unison.Builtin.Decls as Ty
import Unison.ConstructorReference (ConstructorReference, GConstructorReference (..))
import Unison.Hashing.V2.Convert (hashTermComponentsWithoutTypes)
import Unison.Pattern (SeqOp (..))
import qualified Unison.Pattern as P
import Unison.Prelude hiding (Text)
import Unison.Reference (Reference (..))
import Unison.Referent (Referent)
import Unison.Symbol (Symbol)
import Unison.Term hiding (List, Ref, Text, float, fresh, resolve)
import qualified Unison.Type as Ty
import Unison.Typechecker.Components (minimize')
import Unison.Util.Bytes (Bytes)
import Unison.Util.EnumContainers as EC
import qualified Unison.Util.Pretty as Pretty
import qualified Unison.Util.Text as Util.Text
import Unison.Var (Var, typed)
import qualified Unison.Var as Var
import Prelude hiding (abs, and, or, seq)
import qualified Prelude

-- For internal errors
data CompileExn = CE CallStack (Pretty.Pretty Pretty.ColorText)
  deriving (Show)

instance Exception CompileExn

internalBug :: HasCallStack => String -> a
internalBug = throw . CE callStack . Pretty.lit . fromString

closure :: Var v => Map v (Set v, Set v) -> Map v (Set v)
closure m0 = trace (snd <$> m0)
  where
    refs = fst <$> m0

    expand acc fvs rvs =
      fvs <> foldMap (\r -> Map.findWithDefault mempty r acc) rvs

    trace acc
      | acc == acc' = acc
      | otherwise = trace acc'
      where
        acc' = Map.intersectionWith (expand acc) acc refs

expandRec ::
  (Var v, Monoid a) =>
  Set v ->
  [(v, Term v a)] ->
  [(v, Term v a)]
expandRec keep vbs = mkSub <$> fvl
  where
    mkSub (v, fvs) = (v, apps' (var mempty v) (var mempty <$> fvs))

    fvl =
      Map.toList
        . fmap (Set.toList)
        . closure
        $ Set.partition (`Set.member` keep)
          . ABT.freeVars
          <$> Map.fromList vbs

expandSimple ::
  (Var v, Monoid a) =>
  Set v ->
  (v, Term v a) ->
  (v, Term v a)
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

enclose ::
  (Var v, Monoid a) =>
  Set v ->
  (Set v -> Term v a -> Term v a) ->
  Term v a ->
  Maybe (Term v a)
enclose keep rec (LetRecNamedTop' top vbs bd) =
  Just $ letRec' top lvbs lbd
  where
    xpnd = expandRec keep' vbs
    keep' = Set.union keep . Set.fromList . map fst $ vbs
    lvbs = (map . fmap) (rec keep' . abstract keep' . ABT.substs xpnd) vbs
    lbd = rec keep' . ABT.substs xpnd $ bd
-- will be lifted, so keep this variable
enclose keep rec (Let1NamedTop' top v b@(unAnn -> LamsNamed' vs bd) e) =
  Just . let1' top [(v, lamb)] . rec (Set.insert v keep) $
    ABT.subst v av e
  where
    (_, av) = expandSimple keep (v, b)
    keep' = Set.difference keep $ Set.fromList vs
    fvs = ABT.freeVars b
    evs = Set.toList $ Set.difference fvs keep
    a = ABT.annotation b
    lbody = rec keep' bd
    annotate tm
      | Ann' _ ty <- b = ann a tm ty
      | otherwise = tm
    lamb = lam' a evs (annotate $ lam' a vs lbody)
enclose keep rec t@(unLamsAnnot -> Just (vs0, mty, vs1, body)) =
  Just $ if null evs then lamb else apps' lamb $ map (var a) evs
  where
    -- remove shadowed variables
    keep' = Set.difference keep $ Set.fromList (vs0 ++ vs1)
    fvs = ABT.freeVars t
    evs = Set.toList $ Set.difference fvs keep
    a = ABT.annotation t
    lbody = rec keep' body
    annotate tm
      | Just ty <- mty = ann a tm ty
      | otherwise = tm
    lamb = lam' a (evs ++ vs0) . annotate . lam' a vs1 $ lbody
enclose keep rec t@(Handle' h body)
  | isStructured body =
      Just . handle (ABT.annotation t) (rec keep h) $ apps' lamb args
  where
    fvs = ABT.freeVars body
    evs = Set.toList $ Set.difference fvs keep
    a = ABT.annotation body
    lbody = rec keep body
    fv = Var.freshIn fvs $ typed Var.Eta
    args
      | null evs = [constructor a (ConstructorReference Ty.unitRef 0)]
      | otherwise = var a <$> evs
    lamb
      | null evs = lam' a [fv] lbody
      | otherwise = lam' a evs lbody
enclose _ _ _ = Nothing

newtype Prefix v x = Pfx (Map v [v]) deriving (Show)

instance Functor (Prefix v) where
  fmap _ (Pfx m) = Pfx m

instance Ord v => Applicative (Prefix v) where
  pure _ = Pfx Map.empty
  Pfx ml <*> Pfx mr = Pfx $ Map.unionWith common ml mr

common :: Eq v => [v] -> [v] -> [v]
common (u : us) (v : vs)
  | u == v = u : common us vs
common _ _ = []

splitPfx :: v -> [Term v a] -> (Prefix v x, [Term v a])
splitPfx v = first (Pfx . Map.singleton v) . split
  where
    split (Var' u : as) = first (u :) $ split as
    split rest = ([], rest)

-- Finds the common variable prefixes that function variables are
-- applied to, so that they can be reduced.
prefix :: Ord v => Term v a -> Prefix v (Term v a)
prefix = ABT.visit \case
  Apps' (Var' u) as -> case splitPfx u as of
    (pf, rest) -> Just $ traverse prefix rest *> pf
  Var' u -> Just . Pfx $ Map.singleton u []
  _ -> Nothing

appPfx :: Ord v => Prefix v a -> v -> [v] -> [v]
appPfx (Pfx m) v = maybe (const []) common $ Map.lookup v m

-- Rewrites a term by dropping the first n arguments to every
-- application of `v`. This just assumes such a thing makes sense, as
-- in `beta`, where we've calculated how many arguments to drop by
-- looking at every occurrence of `v`.
dropPrefix :: Ord v => Semigroup a => v -> Int -> Term v a -> Term v a
dropPrefix _ 0 = id
dropPrefix v n = ABT.visitPure rw
  where
    rw (Apps' f@(Var' u) as)
      | v == u = Just (apps' (var (ABT.annotation f) u) (drop n as))
    rw _ = Nothing

dropPrefixes ::
  Ord v => Semigroup a => Map v Int -> Term v a -> Term v a
dropPrefixes m = ABT.visitPure rw
  where
    rw (Apps' f@(Var' u) as)
      | Just n <- Map.lookup u m =
          Just (apps' (var (ABT.annotation f) u) (drop n as))
    rw _ = Nothing

-- Performs opposite transformations to those in enclose. Named after
-- the lambda case, which is beta reduction.
beta :: Var v => Monoid a => (Term v a -> Term v a) -> Term v a -> Maybe (Term v a)
beta rec (LetRecNamedTop' top (fmap (fmap rec) -> vbs) (rec -> bd)) =
  Just $ letRec' top lvbs lbd
  where
    -- Avoid completely reducing a lambda expression, because recursive
    -- lets must be guarded.
    args (v, LamsNamed' vs Ann' {}) = (v, vs)
    args (v, LamsNamed' vs _) = (v, init vs)
    args (v, _) = (v, [])

    Pfx m0 = traverse_ (prefix . snd) vbs *> prefix bd

    f ls rs = case common ls rs of
      [] -> Nothing
      vs -> Just vs

    m = Map.map length $ Map.differenceWith f (Map.fromList $ map args vbs) m0
    lvbs =
      vbs <&> \(v, b0) -> (,) v $ case b0 of
        LamsNamed' vs b
          | Just n <- Map.lookup v m ->
              lam' (ABT.annotation b0) (drop n vs) (dropPrefixes m b)
        -- shouldn't happen
        b -> dropPrefixes m b

    lbd = dropPrefixes m bd
beta rec (Let1NamedTop' top v l@(LamsNamed' vs bd) (rec -> e))
  | n > 0 = Just $ let1' top [(v, lamb)] (dropPrefix v n e)
  | otherwise = Nothing
  where
    lamb = lam' al (drop n vs) (bd)
    al = ABT.annotation l
    -- Calculate a maximum number of arguments to drop.
    -- Enclosing doesn't create let-bound lambdas, so we
    -- should never reduce a lambda to a non-lambda, as that
    -- could affect evaluation order.
    m
      | Ann' _ _ <- bd = length vs
      | otherwise = length vs - 1
    n = min m . length $ appPfx (prefix e) v vs
beta rec (Apps' l@(LamsNamed' vs body) as)
  | n <- matchVars 0 vs as,
    n > 0 =
      Just $ apps' (lam' al (drop n vs) (rec body)) (drop n as)
  | otherwise = Nothing
  where
    al = ABT.annotation l
    matchVars !n (u : us) (Var' v : as) | u == v = matchVars (1 + n) us as
    matchVars n _ _ = n
beta _ _ = Nothing

isStructured :: Var v => Term v a -> Bool
isStructured (Var' _) = False
isStructured (Lam' _) = False
isStructured (Nat' _) = False
isStructured (Int' _) = False
isStructured (Float' _) = False
isStructured (Text' _) = False
isStructured (Char' _) = False
isStructured (Constructor' _) = False
isStructured (Apps' Constructor' {} args) = any isStructured args
isStructured (If' b t f) =
  isStructured b || isStructured t || isStructured f
isStructured (And' l r) = isStructured l || isStructured r
isStructured (Or' l r) = isStructured l || isStructured r
isStructured _ = True

close :: (Var v, Monoid a) => Set v -> Term v a -> Term v a
close keep tm = ABT.visitPure (enclose keep close) tm

-- Attempts to undo what was done in `close`. Useful for decompiling.
open :: (Var v, Monoid a) => Term v a -> Term v a
open x = ABT.visitPure (beta open) x

type FloatM v a r = State (Set v, [(v, Term v a)], [(v, Term v a)]) r

freshFloat :: Var v => Set v -> v -> v
freshFloat avoid (Var.freshIn avoid -> v0) =
  case Var.typeOf v0 of
    Var.User nm
      | v <- typed (Var.User $ nm <> w),
        v `Set.notMember` avoid ->
          v
      | otherwise ->
          freshFloat (Set.insert v0 avoid) v0
    _ -> v0
  where
    w = Data.Text.pack . show $ Var.freshId v0

letFloater ::
  (Var v, Monoid a) =>
  (Term v a -> FloatM v a (Term v a)) ->
  [(v, Term v a)] ->
  Term v a ->
  FloatM v a (Term v a)
letFloater rec vbs e = do
  cvs <- gets (\(vs, _, _) -> vs)
  let shadows =
        [ (v, freshFloat cvs v)
          | (v, _) <- vbs,
            Set.member v cvs
        ]
      shadowMap = Map.fromList shadows
      rn v = Map.findWithDefault v v shadowMap
      shvs = Set.fromList $ map (rn . fst) vbs
  modify (\(cvs, ctx, dcmp) -> (cvs <> shvs, ctx, dcmp))
  fvbs <- traverse (\(v, b) -> (,) (rn v) <$> rec' (ABT.renames shadowMap b)) vbs
  modify (\(vs, ctx, dcmp) -> (vs, ctx ++ fvbs, dcmp))
  pure $ ABT.renames shadowMap e
  where
    rec' b
      | Just (vs0, mty, vs1, bd) <- unLamsAnnot b =
          lam' a vs0 . maybe id (flip $ ann a) mty . lam' a vs1 <$> rec bd
      where
        a = ABT.annotation b
    rec' b = rec b

lamFloater ::
  (Var v, Monoid a) =>
  Bool ->
  Term v a ->
  Maybe v ->
  a ->
  [v] ->
  Term v a ->
  FloatM v a v
lamFloater closed tm mv a vs bd =
  state $ \trip@(cvs, ctx, dcmp) -> case find p ctx of
    Just (v, _) -> (v, trip)
    Nothing ->
      let v = ABT.freshIn cvs $ fromMaybe (typed Var.Float) mv
       in ( v,
            ( Set.insert v cvs,
              ctx <> [(v, lam' a vs bd)],
              floatDecomp closed v tm dcmp
            )
          )
  where
    tgt = unannotate (lam' a vs bd)
    p (_, flam) = unannotate flam == tgt

floatDecomp ::
  Bool -> v -> Term v a -> [(v, Term v a)] -> [(v, Term v a)]
floatDecomp True v b dcmp = (v, b) : dcmp
floatDecomp False _ _ dcmp = dcmp

floater ::
  (Var v, Monoid a) =>
  Bool ->
  (Term v a -> FloatM v a (Term v a)) ->
  Term v a ->
  Maybe (FloatM v a (Term v a))
floater top rec tm0@(Ann' tm ty) =
  (fmap . fmap) (\tm -> ann a tm ty) (floater top rec tm)
  where
    a = ABT.annotation tm0
floater top rec (LetRecNamed' vbs e) =
  Just $
    letFloater rec vbs e >>= \case
      lm@(LamsNamed' vs bd) | top -> lam' a vs <$> rec bd
        where
          a = ABT.annotation lm
      tm -> rec tm
floater _ rec (Let1Named' v b e)
  | Just (vs0, _, vs1, bd) <- unLamsAnnot b =
      Just $
        rec bd
          >>= lamFloater True b (Just v) a (vs0 ++ vs1)
          >>= \lv -> rec $ ABT.renames (Map.singleton v lv) e
  where
    a = ABT.annotation b
floater top rec tm@(LamsNamed' vs bd)
  | top = Just $ lam' a vs <$> rec bd
  | otherwise = Just $ do
      bd <- rec bd
      lv <- lamFloater True tm Nothing a vs bd
      pure $ var a lv
  where
    a = ABT.annotation tm
floater _ _ _ = Nothing

float ::
  Var v =>
  Monoid a =>
  Term v a ->
  (Term v a, [(Reference, Term v a)], [(Reference, Term v a)])
float tm = case runState go0 (Set.empty, [], []) of
  (bd, (_, ctx, dcmp)) ->
    let m = hashTermComponentsWithoutTypes . Map.fromList $ fmap deannotate <$> ctx
        trips = Map.toList m
        f (v, (id, tm)) = ((v, id), (v, idtm), (id, tm))
          where
            idtm = ref (ABT.annotation tm) (DerivedId id)
        (subvs, subs, tops) = unzip3 $ map f trips
        subm = Map.fromList subvs
     in ( letRec' True [] . ABT.substs subs . deannotate $ bd,
          fmap (first DerivedId) tops,
          dcmp <&> \(v, tm) -> (DerivedId $ subm Map.! v, open tm)
        )
  where
    go0 = fromMaybe (go tm) (floater True go tm)
    go = ABT.visit $ floater False go

unAnn :: Term v a -> Term v a
unAnn (Ann' tm _) = tm
unAnn tm = tm

unLamsAnnot :: Term v a -> Maybe ([v], Maybe (Ty.Type v a), [v], Term v a)
unLamsAnnot tm0
  | null vs0, null vs1 = Nothing
  | otherwise = Just (vs0, mty, vs1, bd)
  where
    (vs0, bd0)
      | LamsNamed' vs bd <- tm0 = (vs, bd)
      | otherwise = ([], tm0)
    (mty, bd1)
      | Ann' bd ty <- bd0 = (Just ty, bd)
      | otherwise = (Nothing, bd0)
    (vs1, bd)
      | LamsNamed' vs bd <- bd1 = (vs, bd)
      | otherwise = ([], bd1)

deannotate :: Var v => Term v a -> Term v a
deannotate = ABT.visitPure $ \case
  Ann' c _ -> Just $ deannotate c
  _ -> Nothing

lamLift ::
  Var v =>
  Monoid a =>
  Term v a ->
  (Term v a, [(Reference, Term v a)], [(Reference, Term v a)])
lamLift = float . close Set.empty

saturate ::
  (Var v, Monoid a) =>
  Map ConstructorReference Int ->
  Term v a ->
  Term v a
saturate dat = ABT.visitPure $ \case
  Apps' f@(Constructor' r) args -> sat r f args
  Apps' f@(Request' r) args -> sat r f args
  f@(Constructor' r) -> sat r f []
  f@(Request' r) -> sat r f []
  _ -> Nothing
  where
    frsh avoid _ =
      let v = Var.freshIn avoid $ typed Var.Eta
       in (Set.insert v avoid, v)
    sat r f args = case Map.lookup r dat of
      Just n
        | m < n,
          vs <- snd $ mapAccumL frsh fvs [1 .. n - m],
          nargs <- var mempty <$> vs ->
            Just . lam' mempty vs . apps' f $ args' ++ nargs
        | m > n,
          (sargs, eargs) <- splitAt n args',
          sv <- Var.freshIn fvs $ typed Var.Eta ->
            Just
              . let1' False [(sv, apps' f sargs)]
              $ apps' (var mempty sv) eargs
      _ -> Just (apps' f args')
      where
        m = length args
        fvs = foldMap freeVars args
        args' = saturate dat <$> args

addDefaultCases :: Var v => Monoid a => String -> Term v a -> Term v a
addDefaultCases = ABT.visitPure . defaultCaseVisitor

defaultCaseVisitor ::
  Var v => Monoid a => String -> Term v a -> Maybe (Term v a)
defaultCaseVisitor func m@(Match' scrut cases)
  | scrut <- addDefaultCases func scrut,
    cases <- fmap (addDefaultCases func) <$> cases =
      Just $ match a scrut (cases ++ [dflt])
  where
    a = ABT.annotation m
    v = Var.freshIn mempty $ typed Var.Blank
    txt = "pattern match failure in function `" <> func <> "`"
    msg = text a $ Data.Text.pack txt
    bu = ref a (Builtin "bug")
    dflt =
      MatchCase (P.Var a) Nothing
        . ABT.abs' a v
        $ apps bu [(a, Ty.tupleTerm [msg, var a v])]
defaultCaseVisitor _ _ = Nothing

inlineAlias :: Var v => Monoid a => Term v a -> Term v a
inlineAlias = ABT.visitPure $ \case
  Let1Named' v b@(Var' _) e -> Just . inlineAlias $ ABT.subst v b e
  _ -> Nothing

minimizeCyclesOrCrash :: Var v => Term v a -> Term v a
minimizeCyclesOrCrash t = case minimize' t of
  Right t -> t
  Left e ->
    internalBug $
      "tried to minimize let rec with duplicate definitions: "
        ++ show (fst <$> toList e)

data Mem = UN | BX deriving (Eq, Ord, Show, Enum)

-- Context entries with evaluation strategy
data CTE v s
  = ST (Direction Word16) [v] [Mem] s
  | LZ v (Either Reference v) [v]
  deriving (Show)

pattern ST1 :: Direction Word16 -> v -> Mem -> s -> CTE v s
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
newtype RTag = RTag Word64
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (EC.EnumKey)

newtype CTag = CTag Word16
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (EC.EnumKey)

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

-- Masks a packed tag to extract just the constructor tag portion
maskTags :: Word64 -> Word64
maskTags w = w .&. 0xFFFF

ensureRTag :: (Ord n, Show n, Num n) => String -> n -> r -> r
ensureRTag s n x
  | n > 0xFFFFFFFFFFFF =
      internalBug $ s ++ "@RTag: too large: " ++ show n
  | otherwise = x

ensureCTag :: (Ord n, Show n, Num n) => String -> n -> r -> r
ensureCTag s n x
  | n > 0xFFFF =
      internalBug $ s ++ "@CTag: too large: " ++ show n
  | otherwise = x

instance Enum RTag where
  toEnum i = ensureRTag "toEnum" i . RTag $ toEnum i
  fromEnum (RTag w) = fromEnum w

instance Enum CTag where
  toEnum i = ensureCTag "toEnum" i . CTag $ toEnum i
  fromEnum (CTag w) = fromEnum w

instance Num RTag where
  fromInteger i = ensureRTag "fromInteger" i . RTag $ fromInteger i
  (+) = internalBug "RTag: +"
  (*) = internalBug "RTag: *"
  abs = internalBug "RTag: abs"
  signum = internalBug "RTag: signum"
  negate = internalBug "RTag: negate"

instance Num CTag where
  fromInteger i = ensureCTag "fromInteger" i . CTag $ fromInteger i
  (+) = internalBug "CTag: +"
  (*) = internalBug "CTag: *"
  abs = internalBug "CTag: abs"
  signum = internalBug "CTag: signum"
  negate = internalBug "CTag: negate"

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
matchLit (Text' t) = Just $ T (Util.Text.fromText t)
matchLit (Char' c) = Just $ C c
matchLit _ = Nothing

pattern TLet ::
  ABT.Var v =>
  Direction Word16 ->
  v ->
  Mem ->
  ABTN.Term ANormalF v ->
  ABTN.Term ANormalF v ->
  ABTN.Term ANormalF v
pattern TLet d v m bn bo = ABTN.TTm (ALet d [m] bn (ABTN.TAbs v bo))

pattern TLetD ::
  ABT.Var v =>
  v ->
  Mem ->
  ABTN.Term ANormalF v ->
  ABTN.Term ANormalF v ->
  ABTN.Term ANormalF v
pattern TLetD v m bn bo = ABTN.TTm (ALet Direct [m] bn (ABTN.TAbs v bo))

pattern TLets ::
  ABT.Var v =>
  Direction Word16 ->
  [v] ->
  [Mem] ->
  ABTN.Term ANormalF v ->
  ABTN.Term ANormalF v ->
  ABTN.Term ANormalF v
pattern TLets d vs ms bn bo = ABTN.TTm (ALet d ms bn (ABTN.TAbss vs bo))

pattern TName ::
  ABT.Var v =>
  v ->
  Either Reference v ->
  [v] ->
  ABTN.Term ANormalF v ->
  ABTN.Term ANormalF v
pattern TName v f as bo = ABTN.TTm (AName f as (ABTN.TAbs v bo))

pattern Lit' :: Lit -> Term v a
pattern Lit' l <- (matchLit -> Just l)

pattern TLit ::
  ABT.Var v =>
  Lit ->
  ABTN.Term ANormalF v
pattern TLit l = ABTN.TTm (ALit l)

pattern TApp ::
  ABT.Var v =>
  Func v ->
  [v] ->
  ABTN.Term ANormalF v
pattern TApp f args = ABTN.TTm (AApp f args)

pattern AApv :: v -> [v] -> ANormalF v e
pattern AApv v args = AApp (FVar v) args

pattern TApv ::
  ABT.Var v =>
  v ->
  [v] ->
  ABTN.Term ANormalF v
pattern TApv v args = TApp (FVar v) args

pattern ACom :: Reference -> [v] -> ANormalF v e
pattern ACom r args = AApp (FComb r) args

pattern TCom ::
  ABT.Var v =>
  Reference ->
  [v] ->
  ABTN.Term ANormalF v
pattern TCom r args = TApp (FComb r) args

pattern ACon :: Reference -> CTag -> [v] -> ANormalF v e
pattern ACon r t args = AApp (FCon r t) args

pattern TCon ::
  ABT.Var v =>
  Reference ->
  CTag ->
  [v] ->
  ABTN.Term ANormalF v
pattern TCon r t args = TApp (FCon r t) args

pattern AKon :: v -> [v] -> ANormalF v e
pattern AKon v args = AApp (FCont v) args

pattern TKon ::
  ABT.Var v =>
  v ->
  [v] ->
  ABTN.Term ANormalF v
pattern TKon v args = TApp (FCont v) args

pattern AReq :: Reference -> CTag -> [v] -> ANormalF v e
pattern AReq r t args = AApp (FReq r t) args

pattern TReq ::
  ABT.Var v =>
  Reference ->
  CTag ->
  [v] ->
  ABTN.Term ANormalF v
pattern TReq r t args = TApp (FReq r t) args

pattern APrm :: POp -> [v] -> ANormalF v e
pattern APrm p args = AApp (FPrim (Left p)) args

pattern TPrm ::
  ABT.Var v =>
  POp ->
  [v] ->
  ABTN.Term ANormalF v
pattern TPrm p args = TApp (FPrim (Left p)) args

pattern AFOp :: FOp -> [v] -> ANormalF v e
pattern AFOp p args = AApp (FPrim (Right p)) args

pattern TFOp ::
  ABT.Var v =>
  FOp ->
  [v] ->
  ABTN.Term ANormalF v
pattern TFOp p args = TApp (FPrim (Right p)) args

pattern THnd ::
  ABT.Var v =>
  [Reference] ->
  v ->
  ABTN.Term ANormalF v ->
  ABTN.Term ANormalF v
pattern THnd rs h b = ABTN.TTm (AHnd rs h b)

pattern TShift ::
  ABT.Var v =>
  Reference ->
  v ->
  ABTN.Term ANormalF v ->
  ABTN.Term ANormalF v
pattern TShift i v e = ABTN.TTm (AShift i (ABTN.TAbs v e))

pattern TMatch ::
  ABT.Var v =>
  v ->
  Branched (ABTN.Term ANormalF v) ->
  ABTN.Term ANormalF v
pattern TMatch v cs = ABTN.TTm (AMatch v cs)

pattern TFrc :: ABT.Var v => v -> ABTN.Term ANormalF v
pattern TFrc v = ABTN.TTm (AFrc v)

pattern TVar :: ABT.Var v => v -> ABTN.Term ANormalF v
pattern TVar v = ABTN.TTm (AVar v)

{-# COMPLETE TLet, TName, TVar, TApp, TFrc, TLit, THnd, TShift, TMatch #-}

{-# COMPLETE
  TLet,
  TName,
  TVar,
  TFrc,
  TApv,
  TCom,
  TCon,
  TKon,
  TReq,
  TPrm,
  TFOp,
  TLit,
  THnd,
  TShift,
  TMatch
  #-}

bind :: Var v => Cte v -> ANormal v -> ANormal v
bind (ST d us ms bu) = TLets d us ms bu
bind (LZ u f as) = TName u f as

unbind :: Var v => ANormal v -> Maybe (Cte v, ANormal v)
unbind (TLets d us ms bu bd) = Just (ST d us ms bu, bd)
unbind (TName u f as bd) = Just (LZ u f as, bd)
unbind _ = Nothing

unbinds :: Var v => ANormal v -> ([Cte v], ANormal v)
unbinds (TLets d us ms bu (unbinds -> (ctx, bd))) =
  (ST d us ms bu : ctx, bd)
unbinds (TName u f as (unbinds -> (ctx, bd))) = (LZ u f as : ctx, bd)
unbinds tm = ([], tm)

pattern TBind ::
  Var v =>
  Cte v ->
  ANormal v ->
  ANormal v
pattern TBind bn bd <-
  (unbind -> Just (bn, bd))
  where
    TBind bn bd = bind bn bd

pattern TBinds :: Var v => [Cte v] -> ANormal v -> ANormal v
pattern TBinds ctx bd <-
  (unbinds -> (ctx, bd))
  where
    TBinds ctx bd = foldr bind bd ctx

{-# COMPLETE TBinds #-}

data SeqEnd = SLeft | SRight
  deriving (Eq, Ord, Enum, Show)

data Branched e
  = MatchIntegral (EnumMap Word64 e) (Maybe e)
  | MatchText (Map.Map Util.Text.Text e) (Maybe e)
  | MatchRequest (Map Reference (EnumMap CTag ([Mem], e))) e
  | MatchEmpty
  | MatchData Reference (EnumMap CTag ([Mem], e)) (Maybe e)
  | MatchSum (EnumMap Word64 ([Mem], e))
  deriving (Show, Functor, Foldable, Traversable)

-- Data cases expected to cover all constructors
pattern MatchDataCover :: Reference -> EnumMap CTag ([Mem], e) -> Branched e
pattern MatchDataCover r m = MatchData r m Nothing

data BranchAccum v
  = AccumEmpty
  | AccumIntegral
      Reference
      (Maybe (ANormal v))
      (EnumMap Word64 (ANormal v))
  | AccumText
      (Maybe (ANormal v))
      (Map.Map Util.Text.Text (ANormal v))
  | AccumDefault (ANormal v)
  | AccumPure (ANormal v)
  | AccumRequest
      (Map Reference (EnumMap CTag ([Mem], ANormal v)))
      (Maybe (ANormal v))
  | AccumData
      Reference
      (Maybe (ANormal v))
      (EnumMap CTag ([Mem], ANormal v))
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
  AccumText dl cl <> AccumText dr cr =
    AccumText (dl <|> dr) (cl <> cr)
  AccumData rl dl cl <> AccumData rr dr cr
    | rl == rr = AccumData rl (dl <|> dr) (cl <> cr)
  AccumDefault dl <> AccumIntegral r _ cr =
    AccumIntegral r (Just dl) cr
  AccumDefault dl <> AccumText _ cr =
    AccumText (Just dl) cr
  AccumDefault dl <> AccumData rr _ cr =
    AccumData rr (Just dl) cr
  AccumIntegral r dl cl <> AccumDefault dr =
    AccumIntegral r (dl <|> Just dr) cl
  AccumText dl cl <> AccumDefault dr =
    AccumText (dl <|> Just dr) cl
  AccumData rl dl cl <> AccumDefault dr =
    AccumData rl (dl <|> Just dr) cl
  l@(AccumPure _) <> AccumPure _ = l
  AccumPure dl <> AccumRequest hr _ = AccumRequest hr (Just dl)
  AccumRequest hl dl <> AccumPure dr =
    AccumRequest hl (dl <|> Just dr)
  AccumRequest hl dl <> AccumRequest hr dr =
    AccumRequest hm $ dl <|> dr
    where
      hm = Map.unionWith (<>) hl hr
  l@(AccumSeqEmpty _) <> AccumSeqEmpty _ = l
  AccumSeqEmpty eml <> AccumSeqView er _ cnr =
    AccumSeqView er (Just eml) cnr
  AccumSeqView el eml cnl <> AccumSeqEmpty emr =
    AccumSeqView el (eml <|> Just emr) cnl
  AccumSeqView el eml cnl <> AccumSeqView er emr _
    | el /= er =
        internalBug "AccumSeqView: trying to merge views of opposite ends"
    | otherwise = AccumSeqView el (eml <|> emr) cnl
  AccumSeqView _ _ _ <> AccumDefault _ =
    internalBug "seq views may not have defaults"
  AccumDefault _ <> AccumSeqView _ _ _ =
    internalBug "seq views may not have defaults"
  AccumSeqSplit el nl dl bl <> AccumSeqSplit er nr dr _
    | el /= er =
        internalBug
          "AccumSeqSplit: trying to merge splits at opposite ends"
    | nl /= nr =
        internalBug
          "AccumSeqSplit: trying to merge splits at different positions"
    | otherwise =
        AccumSeqSplit el nl (dl <|> dr) bl
  AccumDefault dl <> AccumSeqSplit er nr _ br =
    AccumSeqSplit er nr (Just dl) br
  AccumSeqSplit el nl dl bl <> AccumDefault dr =
    AccumSeqSplit el nl (dl <|> Just dr) bl
  _ <> _ = internalBug $ "cannot merge data cases for different types"

instance Monoid (BranchAccum e) where
  mempty = AccumEmpty

-- Foreign operation, indexed by words
type FOp = Word64

data Func v
  = -- variable
    FVar v
  | -- top-level combinator
    FComb !Reference
  | -- continuation jump
    FCont v
  | -- data constructor
    FCon !Reference !CTag
  | -- ability request
    FReq !Reference !CTag
  | -- prim op
    FPrim (Either POp FOp)
  deriving (Show, Functor, Foldable, Traversable)

data Lit
  = I Int64
  | N Word64
  | F Double
  | T Util.Text.Text
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

-- Note: Enum/Bounded instances should only be used for things like
-- getting a list of all ops. Using auto-generated numberings for
-- serialization, for instance, could cause observable changes to
-- formats that we want to control and version.
data POp
  = -- Int
    ADDI
  | SUBI
  | MULI
  | DIVI -- +,-,*,/
  | SGNI
  | NEGI
  | MODI -- sgn,neg,mod
  | POWI
  | SHLI
  | SHRI -- pow,shiftl,shiftr
  | INCI
  | DECI
  | LEQI
  | EQLI -- inc,dec,<=,==
  -- Nat
  | ADDN
  | SUBN
  | MULN
  | DIVN -- +,-,*,/
  | MODN
  | TZRO
  | LZRO
  | POPC -- mod,trailing/leadingZeros,popCount
  | POWN
  | SHLN
  | SHRN -- pow,shiftl,shiftr
  | ANDN
  | IORN
  | XORN
  | COMN -- and,or,xor,complement
  | INCN
  | DECN
  | LEQN
  | EQLN -- inc,dec,<=,==
  -- Float
  | ADDF
  | SUBF
  | MULF
  | DIVF -- +,-,*,/
  | MINF
  | MAXF
  | LEQF
  | EQLF -- min,max,<=,==
  | POWF
  | EXPF
  | SQRT
  | LOGF -- pow,exp,sqrt,log
  | LOGB -- logBase
  | ABSF
  | CEIL
  | FLOR
  | TRNF -- abs,ceil,floor,truncate
  | RNDF -- round
  -- Trig
  | COSF
  | ACOS
  | COSH
  | ACSH -- cos,acos,cosh,acosh
  | SINF
  | ASIN
  | SINH
  | ASNH -- sin,asin,sinh,asinh
  | TANF
  | ATAN
  | TANH
  | ATNH -- tan,atan,tanh,atanh
  | ATN2 -- atan2
  -- Text
  | CATT
  | TAKT
  | DRPT
  | SIZT -- ++,take,drop,size
  | UCNS
  | USNC
  | EQLT
  | LEQT -- uncons,unsnoc,==,<=
  | PAKT
  | UPKT -- pack,unpack
  -- Sequence
  | CATS
  | TAKS
  | DRPS
  | SIZS -- ++,take,drop,size
  | CONS
  | SNOC
  | IDXS
  | BLDS -- cons,snoc,at,build
  | VWLS
  | VWRS
  | SPLL
  | SPLR -- viewl,viewr,splitl,splitr
  -- Bytes
  | PAKB
  | UPKB
  | TAKB
  | DRPB -- pack,unpack,take,drop
  | IDXB
  | SIZB
  | FLTB
  | CATB -- index,size,flatten,append
  -- Conversion
  | ITOF
  | NTOF
  | ITOT
  | NTOT
  | TTOI
  | TTON
  | TTOF
  | FTOT
  | -- Concurrency
    FORK
  | -- Universal operations
    EQLU
  | CMPU
  | EROR
  | -- Code
    MISS
  | CACH
  | LKUP
  | LOAD -- isMissing,cache_,lookup,load
  | CVLD
  | SDBX -- validate, sandbox
  | VALU
  | TLTT -- value, Term.Link.toText
  -- Debug
  | PRNT
  | INFO
  | TRCE
  | -- STM
    ATOM
  | TFRC -- try force
  deriving (Show, Eq, Ord, Enum, Bounded)

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
data SuperNormal v = Lambda {conventions :: [Mem], bound :: ANormal v}
  deriving (Show)

data SuperGroup v = Rec
  { group :: [(v, SuperNormal v)],
    entry :: SuperNormal v
  }
  deriving (Show)

type ANFM v =
  ReaderT
    (Set v)
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
  | Mark Word64 Word64 [Reference] (Map Reference Value) Cont
  | Push Word64 Word64 Word64 Word64 GroupRef Cont
  deriving (Show)

data BLit
  = Text Util.Text.Text
  | List (Seq Value)
  | TmLink Referent
  | TyLink Reference
  | Bytes Bytes
  | Quote Value
  | Code (SuperGroup Symbol)
  | BArr PA.ByteArray
  deriving (Show)

groupVars :: ANFM v (Set v)
groupVars = ask

bindLocal :: Ord v => [v] -> ANFM v r -> ANFM v r
bindLocal vs = local (Set.\\ Set.fromList vs)

freshANF :: Var v => Word64 -> v
freshANF fr = Var.freshenId fr $ typed Var.ANFBlank

fresh :: Var v => ANFM v v
fresh = state $ \(fr, bnd, cs) -> (freshANF fr, (fr + 1, bnd, cs))

contextualize :: Var v => DNormal v -> ANFM v (Ctx v, v)
contextualize (_, TVar cv) = do
  gvs <- groupVars
  if cv `Set.notMember` gvs
    then pure (pure [], cv)
    else do
      bv <- fresh
      d <- Indirect <$> binder
      pure (directed [ST1 d bv BX $ TApv cv []], bv)
contextualize (d0, tm) = do
  fv <- fresh
  d <- bindDirection d0
  pure ((d0, [ST1 d fv BX tm]), fv)

binder :: ANFM v Word16
binder = state $ \(fr, bnd, cs) -> (bnd, (fr, bnd + 1, cs))

bindDirection :: Direction a -> ANFM v (Direction Word16)
bindDirection = traverse (const binder)

record :: Var v => (v, SuperNormal v) -> ANFM v ()
record p = modify $ \(fr, bnd, to) -> (fr, bnd, p : to)

superNormalize :: Var v => Term v a -> SuperGroup v
superNormalize tm = Rec l c
  where
    (bs, e)
      | LetRecNamed' bs e <- tm = (bs, e)
      | otherwise = ([], tm)
    grp = Set.fromList $ fst <$> bs
    comp = traverse_ superBinding bs *> toSuperNormal e
    subc = runReaderT comp grp
    (c, (_, _, l)) = runState subc (0, 1, [])

superBinding :: Var v => (v, Term v a) -> ANFM v ()
superBinding (v, tm) = do
  nf <- toSuperNormal tm
  modify $ \(cvs, bnd, ctx) -> (cvs, bnd, (v, nf) : ctx)

toSuperNormal :: Var v => Term v a -> ANFM v (SuperNormal v)
toSuperNormal tm = do
  grp <- groupVars
  if not . Set.null . (Set.\\ grp) $ freeVars tm
    then internalBug $ "free variables in supercombinator: " ++ show tm
    else
      Lambda (BX <$ vs) . ABTN.TAbss vs . snd
        <$> bindLocal vs (anfTerm body)
  where
    (vs, body) = fromMaybe ([], tm) $ unLams' tm

anfTerm :: Var v => Term v a -> ANFM v (DNormal v)
anfTerm tm = f <$> anfBlock tm
  where
    -- f = uncurry (liftA2 TBinds)
    f ((_, []), dtm) = dtm
    f ((_, cx), (_, tm)) = (Indirect (), TBinds cx tm)

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
anfHandled body =
  anfBlock body >>= \case
    (ctx, (_, t@TCon {})) ->
      fresh <&> \v ->
        (ctx <> pure [ST1 Direct v BX t], pure $ TVar v)
    (ctx, (_, t@(TLit l))) ->
      fresh <&> \v ->
        (ctx <> pure [ST1 Direct v cc t], pure $ TVar v)
      where
        cc = case l of T {} -> BX; LM {} -> BX; LY {} -> BX; _ -> UN
    p -> pure p

fls, tru :: Var v => ANormal v
fls = TCon Ty.booleanRef 0 []
tru = TCon Ty.booleanRef 1 []

-- Helper function for renaming a variable arising from a
--   let v = u
-- binding during ANF translation. Renames a variable in a
-- context, and returns an indication of whether the varible
-- was shadowed by one of the context bindings.
renameCtx :: Var v => v -> v -> Ctx v -> (Ctx v, Bool)
renameCtx v u (d, ctx) | (ctx, b) <- rn [] ctx = ((d, ctx), b)
  where
    swap w | w == v = u | otherwise = w

    rn acc [] = (reverse acc, False)
    rn acc (ST d vs ccs b : es)
      | any (== v) vs = (reverse acc ++ e : es, True)
      | otherwise = rn (e : acc) es
      where
        e = ST d vs ccs $ ABTN.rename v u b
    rn acc (LZ w f as : es)
      | w == v = (reverse acc ++ e : es, True)
      | otherwise = rn (e : acc) es
      where
        e = LZ w (swap <$> f) (swap <$> as)

anfBlock :: Var v => Term v a -> ANFM v (Ctx v, DNormal v)
anfBlock (Var' v) = pure (mempty, pure $ TVar v)
anfBlock (If' c t f) = do
  (cctx, cc) <- anfBlock c
  (df, cf) <- anfTerm f
  (dt, ct) <- anfTerm t
  (cx, v) <- contextualize cc
  let cases =
        MatchData
          (Builtin $ Data.Text.pack "Boolean")
          (EC.mapSingleton 0 ([], cf))
          (Just ct)
  pure (cctx <> cx, (Indirect () <> df <> dt, TMatch v cases))
anfBlock (And' l r) = do
  (lctx, vl) <- anfArg l
  (d, tmr) <- anfTerm r
  let tree =
        TMatch vl . MatchDataCover Ty.booleanRef $
          mapFromList
            [ (0, ([], fls)),
              (1, ([], tmr))
            ]
  pure (lctx, (Indirect () <> d, tree))
anfBlock (Or' l r) = do
  (lctx, vl) <- anfArg l
  (d, tmr) <- anfTerm r
  let tree =
        TMatch vl . MatchDataCover Ty.booleanRef $
          mapFromList
            [ (1, ([], tru)),
              (0, ([], tmr))
            ]
  pure (lctx, (Indirect () <> d, tree))
anfBlock (Handle' h body) =
  anfArg h >>= \(hctx, vh) ->
    anfHandled body >>= \case
      (ctx, (_, TCom f as)) | floatableCtx ctx -> do
        v <- fresh
        pure
          ( hctx <> ctx <> pure [LZ v (Left f) as],
            (Indirect (), TApp (FVar vh) [v])
          )
      (ctx, (_, TApv f as)) | floatableCtx ctx -> do
        v <- fresh
        pure
          ( hctx <> ctx <> pure [LZ v (Right f) as],
            (Indirect (), TApp (FVar vh) [v])
          )
      (ctx, (_, TVar v)) | floatableCtx ctx -> do
        pure (hctx <> ctx, (Indirect (), TApp (FVar vh) [v]))
      p@(_, _) ->
        internalBug $ "handle body should be a simple call: " ++ show p
anfBlock (Match' scrut cas) = do
  (sctx, sc) <- anfBlock scrut
  (cx, v) <- contextualize sc
  (d, brn) <- anfCases v cas
  fmap (first ((Indirect () <> d) <>)) <$> case brn of
    AccumDefault (TBinds (directed -> dctx) df) -> do
      pure (sctx <> cx <> dctx, pure df)
    AccumRequest _ Nothing ->
      internalBug "anfBlock: AccumRequest without default"
    AccumPure (ABTN.TAbss us bd)
      | [u] <- us,
        TBinds (directed -> bx) bd <- bd ->
          case cx of
            (_, []) -> do
              d0 <- Indirect <$> binder
              pure (sctx <> pure [ST1 d0 u BX (TFrc v)] <> bx, pure bd)
            (d0, [ST1 d1 _ BX tm]) ->
              pure (sctx <> (d0, [ST1 d1 u BX tm]) <> bx, pure bd)
            _ -> internalBug "anfBlock|AccumPure: impossible"
      | otherwise -> internalBug "pure handler with too many variables"
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
            | (_, [ST _ _ _ _]) <- cx =
                internalBug "anfBlock: impossible"
            | otherwise = (Indirect (), TFrc v)
      pure
        ( sctx <> pure [LZ hv (Right r) vs],
          (d, THnd (Map.keys abr) hv msc)
        )
    AccumText df cs ->
      pure (sctx <> cx, pure . TMatch v $ MatchText cs df)
    AccumIntegral r df cs -> do
      i <- fresh
      let dcs =
            MatchDataCover
              r
              (EC.mapSingleton 0 ([UN], ABTN.TAbss [i] ics))
          ics = TMatch i $ MatchIntegral cs df
      pure (sctx <> cx, pure $ TMatch v dcs)
    AccumData r df cs ->
      pure (sctx <> cx, pure . TMatch v $ MatchData r cs df)
    AccumSeqEmpty _ ->
      internalBug "anfBlock: non-exhaustive AccumSeqEmpty"
    AccumSeqView en (Just em) bd -> do
      r <- fresh
      let op
            | SLeft <- en = Builtin "List.viewl"
            | otherwise = Builtin "List.viewr"
      b <- binder
      pure
        ( sctx <> cx
            <> (Indirect (), [ST1 (Indirect b) r BX (TCom op [v])]),
          pure . TMatch r $
            MatchDataCover
              Ty.seqViewRef
              ( EC.mapFromList
                  [ (fromIntegral Ty.seqViewEmpty, ([], em)),
                    (fromIntegral Ty.seqViewElem, ([BX, BX], bd))
                  ]
              )
        )
    AccumSeqView {} ->
      internalBug "anfBlock: non-exhaustive AccumSeqView"
    AccumSeqSplit en n mdf bd -> do
      i <- fresh
      r <- fresh
      n <- fresh
      pure
        ( sctx <> cx <> directed [lit i, split i r],
          pure . TMatch r . MatchSum $
            mapFromList
              [ (0, ([], df n)),
                (1, ([BX, BX], bd))
              ]
        )
      where
        op
          | SLeft <- en = SPLL
          | otherwise = SPLR
        lit i = ST1 Direct i UN (TLit . N $ fromIntegral n)
        split i r = ST1 Direct r UN (TPrm op [i, v])
        df n =
          fromMaybe
            ( TLet Direct n BX (TLit (T "pattern match failure")) $
                TPrm EROR [n, v]
            )
            mdf
    AccumEmpty -> pure (sctx <> cx, pure $ TMatch v MatchEmpty)
anfBlock (Let1Named' v b e) =
  anfBlock b >>= \case
    (bctx, (Direct, TVar u)) -> do
      (ectx, ce) <- anfBlock e
      (ectx, shaded) <- pure $ renameCtx v u ectx
      ce <- pure $ if shaded then ce else ABTN.rename v u <$> ce
      pure (bctx <> ectx, ce)
    (bctx, (d0, cb)) -> bindLocal [v] $ do
      (ectx, ce) <- anfBlock e
      d <- bindDirection d0
      let octx = bctx <> directed [ST1 d v BX cb] <> ectx
      pure (octx, ce)
anfBlock (Apps' (Blank' b) args) = do
  nm <- fresh
  (actx, cas) <- anfArgs args
  pure
    ( actx <> pure [ST1 Direct nm BX (TLit (T msg))],
      pure $ TPrm EROR (nm : cas)
    )
  where
    msg = Util.Text.pack . fromMaybe "blank expression" $ nameb b
anfBlock (Apps' f args) = do
  (fctx, (d, cf)) <- anfFunc f
  (actx, cas) <- anfArgs args
  pure (fctx <> actx, (d, TApp cf cas))
anfBlock (Constructor' (ConstructorReference r t)) =
  pure (mempty, pure $ TCon r (fromIntegral t) [])
anfBlock (Request' (ConstructorReference r t)) =
  pure (mempty, (Indirect (), TReq r (fromIntegral t) []))
anfBlock (Boolean' b) =
  pure (mempty, pure $ TCon Ty.booleanRef (if b then 1 else 0) [])
anfBlock (Lit' l@(T _)) =
  pure (mempty, pure $ TLit l)
anfBlock (Lit' l) = do
  lv <- fresh
  pure
    ( directed [ST1 Direct lv UN $ TLit l],
      pure $ TCon (litRef l) 0 [lv]
    )
anfBlock (Ref' r) = pure (mempty, (Indirect (), TCom r []))
anfBlock (Blank' b) = do
  nm <- fresh
  ev <- fresh
  pure
    ( pure
        [ ST1 Direct nm BX (TLit (T name)),
          ST1 Direct ev BX (TLit (T $ Util.Text.pack msg))
        ],
      pure $ TPrm EROR [nm, ev]
    )
  where
    name = "blank expression"
    msg = fromMaybe "blank expression" $ nameb b
anfBlock (TermLink' r) = pure (mempty, pure . TLit $ LM r)
anfBlock (TypeLink' r) = pure (mempty, pure . TLit $ LY r)
anfBlock (List' as) = fmap (pure . TPrm BLDS) <$> anfArgs tms
  where
    tms = toList as
anfBlock t = internalBug $ "anf: unhandled term: " ++ show t

-- Note: this assumes that patterns have already been translated
-- to a state in which every case matches a single layer of data,
-- with no guards, and no variables ignored. This is not checked
-- completely.
anfInitCase ::
  Var v =>
  v ->
  MatchCase p (Term v a) ->
  ANFD v (BranchAccum v)
anfInitCase u (MatchCase p guard (ABT.AbsN' vs bd))
  | Just _ <- guard = internalBug "anfInitCase: unexpected guard"
  | P.Unbound _ <- p,
    [] <- vs =
      AccumDefault <$> anfBody bd
  | P.Var _ <- p,
    [v] <- vs =
      AccumDefault . ABTN.rename v u <$> anfBody bd
  | P.Var _ <- p =
      internalBug $ "vars: " ++ show (length vs)
  | P.Int _ (fromIntegral -> i) <- p =
      AccumIntegral Ty.intRef Nothing . EC.mapSingleton i <$> anfBody bd
  | P.Nat _ i <- p =
      AccumIntegral Ty.natRef Nothing . EC.mapSingleton i <$> anfBody bd
  | P.Char _ c <- p,
    w <- fromIntegral $ fromEnum c =
      AccumIntegral Ty.charRef Nothing . EC.mapSingleton w <$> anfBody bd
  | P.Boolean _ b <- p,
    t <- if b then 1 else 0 =
      AccumData Ty.booleanRef Nothing
        . EC.mapSingleton t
        . ([],)
        <$> anfBody bd
  | P.Text _ t <- p,
    [] <- vs =
      AccumText Nothing . Map.singleton (Util.Text.fromText t) <$> anfBody bd
  | P.Constructor _ (ConstructorReference r t) ps <- p = do
      (,) <$> expandBindings ps vs <*> anfBody bd <&> \(us, bd) ->
        AccumData r Nothing
          . EC.mapSingleton (fromIntegral t)
          . (BX <$ us,)
          . ABTN.TAbss us
          $ bd
  | P.EffectPure _ q <- p =
      (,) <$> expandBindings [q] vs <*> anfBody bd <&> \(us, bd) ->
        AccumPure $ ABTN.TAbss us bd
  | P.EffectBind _ (ConstructorReference r t) ps pk <- p = do
      (,,) <$> expandBindings (snoc ps pk) vs
        <*> Compose (pure <$> fresh)
        <*> anfBody bd
        <&> \(exp, kf, bd) ->
          let (us, uk) =
                maybe (internalBug "anfInitCase: unsnoc impossible") id $
                  unsnoc exp
              jn = Builtin "jumpCont"
           in flip AccumRequest Nothing
                . Map.singleton r
                . EC.mapSingleton (fromIntegral t)
                . (BX <$ us,)
                . ABTN.TAbss us
                . TShift r kf
                . TName uk (Left jn) [kf]
                $ bd
  | P.SequenceLiteral _ [] <- p =
      AccumSeqEmpty <$> anfBody bd
  | P.SequenceOp _ l op r <- p,
    Concat <- op,
    P.SequenceLiteral p ll <- l = do
      AccumSeqSplit SLeft (length ll) Nothing
        <$> (ABTN.TAbss <$> expandBindings [P.Var p, r] vs <*> anfBody bd)
  | P.SequenceOp _ l op r <- p,
    Concat <- op,
    P.SequenceLiteral p rl <- r =
      AccumSeqSplit SLeft (length rl) Nothing
        <$> (ABTN.TAbss <$> expandBindings [l, P.Var p] vs <*> anfBody bd)
  | P.SequenceOp _ l op r <- p,
    dir <- case op of Cons -> SLeft; _ -> SRight =
      AccumSeqView dir Nothing
        <$> (ABTN.TAbss <$> expandBindings [l, r] vs <*> anfBody bd)
  where
    anfBody tm = Compose . bindLocal vs $ anfTerm tm
anfInitCase _ (MatchCase p _ _) =
  internalBug $ "anfInitCase: unexpected pattern: " ++ show p

valueTermLinks :: Value -> [Reference]
valueTermLinks = Set.toList . valueLinks f
  where
    f False r = Set.singleton r
    f _ _ = Set.empty

valueLinks :: Monoid a => (Bool -> Reference -> a) -> Value -> a
valueLinks f (Partial (GR cr _) _ bs) =
  f False cr <> foldMap (valueLinks f) bs
valueLinks f (Data dr _ _ bs) =
  f True dr <> foldMap (valueLinks f) bs
valueLinks f (Cont _ bs k) =
  foldMap (valueLinks f) bs <> contLinks f k
valueLinks f (BLit l) = litLinks f l

contLinks :: Monoid a => (Bool -> Reference -> a) -> Cont -> a
contLinks f (Push _ _ _ _ (GR cr _) k) =
  f False cr <> contLinks f k
contLinks f (Mark _ _ ps de k) =
  foldMap (f True) ps
    <> Map.foldMapWithKey (\k c -> f True k <> valueLinks f c) de
    <> contLinks f k
contLinks _ KE = mempty

litLinks :: Monoid a => (Bool -> Reference -> a) -> BLit -> a
litLinks f (List s) = foldMap (valueLinks f) s
litLinks _ _ = mempty

groupTermLinks :: SuperGroup v -> [Reference]
groupTermLinks = Set.toList . groupLinks f
  where
    f False r = Set.singleton r
    f _ _ = Set.empty

groupLinks :: Monoid a => (Bool -> Reference -> a) -> SuperGroup v -> a
groupLinks f (Rec bs e) =
  foldMap (foldMap (normalLinks f)) bs <> normalLinks f e

normalLinks ::
  Monoid a => (Bool -> Reference -> a) -> SuperNormal v -> a
normalLinks f (Lambda _ e) = anfLinks f e

anfLinks :: Monoid a => (Bool -> Reference -> a) -> ANormal v -> a
anfLinks f (ABTN.Term _ (ABTN.Abs _ e)) = anfLinks f e
anfLinks f (ABTN.Term _ (ABTN.Tm e)) = anfFLinks f (anfLinks f) e

anfFLinks ::
  Monoid a =>
  (Bool -> Reference -> a) ->
  (e -> a) ->
  ANormalF v e ->
  a
anfFLinks _ g (ALet _ _ b e) = g b <> g e
anfFLinks f g (AName er _ e) =
  bifoldMap (f False) (const mempty) er <> g e
anfFLinks f g (AMatch _ bs) = branchLinks (f True) g bs
anfFLinks f g (AShift r e) = f True r <> g e
anfFLinks f g (AHnd rs _ e) = foldMap (f True) rs <> g e
anfFLinks f _ (AApp fu _) = funcLinks f fu
anfFLinks _ _ _ = mempty

branchLinks ::
  Monoid a =>
  (Reference -> a) ->
  (e -> a) ->
  Branched e ->
  a
branchLinks f g bs = tyRefs f bs <> foldMap g bs

tyRefs :: Monoid a => (Reference -> a) -> Branched e -> a
tyRefs f (MatchRequest m _) = foldMap f (Map.keys m)
tyRefs f (MatchData r _ _) = f r
tyRefs _ _ = mempty

funcLinks ::
  Monoid a =>
  (Bool -> Reference -> a) ->
  Func v ->
  a
funcLinks f (FComb r) = f False r
funcLinks f (FCon r _) = f True r
funcLinks f (FReq r _) = f True r
funcLinks _ _ = mempty

expandBindings' ::
  Var v =>
  Word64 ->
  [P.Pattern p] ->
  [v] ->
  Either String (Word64, [v])
expandBindings' fr [] [] = Right (fr, [])
expandBindings' fr (P.Unbound _ : ps) vs =
  fmap (u :) <$> expandBindings' (fr + 1) ps vs
  where
    u = freshANF fr
expandBindings' fr (P.Var _ : ps) (v : vs) =
  fmap (v :) <$> expandBindings' fr ps vs
expandBindings' _ [] (_ : _) =
  Left "expandBindings': more bindings than expected"
expandBindings' _ (_ : _) [] =
  Left "expandBindings': more patterns than expected"
expandBindings' _ _ _ =
  Left $ "expandBindings': unexpected pattern"

expandBindings :: Var v => [P.Pattern p] -> [v] -> ANFD v [v]
expandBindings ps vs =
  Compose . state $ \(fr, bnd, co) -> case expandBindings' fr ps vs of
    Left err -> internalBug $ err ++ " " ++ show (ps, vs)
    Right (fr, l) -> (pure l, (fr, bnd, co))

anfCases ::
  Var v =>
  v ->
  [MatchCase p (Term v a)] ->
  ANFM v (Directed () (BranchAccum v))
anfCases u = getCompose . fmap fold . traverse (anfInitCase u)

anfFunc :: Var v => Term v a -> ANFM v (Ctx v, Directed () (Func v))
anfFunc (Var' v) = pure (mempty, (Indirect (), FVar v))
anfFunc (Ref' r) = pure (mempty, (Indirect (), FComb r))
anfFunc (Constructor' (ConstructorReference r t)) = pure (mempty, (Direct, FCon r $ fromIntegral t))
anfFunc (Request' (ConstructorReference r t)) = pure (mempty, (Indirect (), FReq r $ fromIntegral t))
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
indent ind = showString (replicate (ind * 2) ' ')

prettyGroup :: Var v => String -> SuperGroup v -> ShowS
prettyGroup s (Rec grp ent) =
  showString ("let rec[" ++ s ++ "]\n")
    . foldr f id grp
    . showString "entry"
    . prettySuperNormal 1 ent
  where
    f (v, sn) r =
      indent 1 . pvar v
        . prettySuperNormal 2 sn
        . showString "\n"
        . r

pvar :: Var v => v -> ShowS
pvar v = showString . Data.Text.unpack $ Var.name v

prettyVars :: Var v => [v] -> ShowS
prettyVars =
  foldr (\v r -> showString " " . pvar v . r) id

prettyLVars :: Var v => [Mem] -> [v] -> ShowS
prettyLVars [] [] = showString " "
prettyLVars (c : cs) (v : vs) =
  showString " "
    . showParen True (pvar v . showString ":" . shows c)
    . prettyLVars cs vs
prettyLVars [] (_ : _) = internalBug "more variables than conventions"
prettyLVars (_ : _) [] = internalBug "more conventions than variables"

prettyRBind :: Var v => [v] -> ShowS
prettyRBind [] = showString "()"
prettyRBind [v] = pvar v
prettyRBind (v : vs) =
  showParen True $
    pvar v . foldr (\v r -> shows v . showString "," . r) id vs

prettySuperNormal :: Var v => Int -> SuperNormal v -> ShowS
prettySuperNormal ind (Lambda ccs (ABTN.TAbss vs tm)) =
  prettyLVars ccs vs
    . showString "="
    . prettyANF False (ind + 1) tm

reqSpace :: Var v => Bool -> ANormal v -> Bool
reqSpace _ TLets {} = True
reqSpace _ TName {} = True
reqSpace b _ = b

prettyANF :: Var v => Bool -> Int -> ANormal v -> ShowS
prettyANF m ind tm =
  prettySpace (reqSpace m tm) ind . case tm of
    TLets _ vs _ bn bo ->
      prettyRBind vs
        . showString " ="
        . prettyANF False (ind + 1) bn
        . prettyANF True ind bo
    TName v f vs bo ->
      prettyRBind [v]
        . showString " := "
        . prettyLZF f
        . prettyVars vs
        . prettyANF True ind bo
    TLit l -> shows l
    TFrc v -> showString "!" . pvar v
    TVar v -> pvar v
    TApp f vs -> prettyFunc f . prettyVars vs
    TMatch v bs ->
      showString "match "
        . pvar v
        . showString " with"
        . prettyBranches (ind + 1) bs
    TShift r v bo ->
      showString "shift[" . shows r . showString "]"
        . prettyVars [v]
        . showString "."
        . prettyANF False (ind + 1) bo
    THnd rs v bo ->
      showString "handle" . prettyRefs rs
        . prettyANF False (ind + 1) bo
        . showString " with "
        . pvar v
    _ -> shows tm

prettySpace :: Bool -> Int -> ShowS
prettySpace False _ = showString " "
prettySpace True ind = showString "\n" . indent ind

prettyLZF :: Var v => Either Reference v -> ShowS
prettyLZF (Left w) = showString "ENV(" . shows w . showString ") "
prettyLZF (Right v) = pvar v . showString " "

prettyRefs :: [Reference] -> ShowS
prettyRefs [] = showString "{}"
prettyRefs (r : rs) =
  showString "{" . shows r
    . foldr (\t r -> shows t . showString "," . r) id rs
    . showString "}"

prettyFunc :: Var v => Func v -> ShowS
prettyFunc (FVar v) = pvar v . showString " "
prettyFunc (FCont v) = pvar v . showString " "
prettyFunc (FComb w) = showString "ENV(" . shows w . showString ")"
prettyFunc (FCon r t) =
  showString "CON("
    . shows r
    . showString ","
    . shows t
    . showString ")"
prettyFunc (FReq r t) =
  showString "REQ("
    . shows r
    . showString ","
    . shows t
    . showString ")"
prettyFunc (FPrim op) = either shows shows op . showString " "

prettyBranches :: Var v => Int -> Branched (ANormal v) -> ShowS
prettyBranches ind bs = case bs of
  MatchEmpty -> showString "{}"
  MatchIntegral bs df ->
    maybe id (\e -> prettyCase ind (showString "_") e id) df
      . foldr (uncurry $ prettyCase ind . shows) id (mapToList bs)
  MatchText bs df ->
    maybe id (\e -> prettyCase ind (showString "_") e id) df
      . foldr (uncurry $ prettyCase ind . shows) id (Map.toList bs)
  MatchData _ bs df ->
    maybe id (\e -> prettyCase ind (showString "_") e id) df
      . foldr
        (uncurry $ prettyCase ind . shows)
        id
        (mapToList $ snd <$> bs)
  MatchRequest bs df ->
    foldr
      ( \(r, m) s ->
          foldr
            (\(c, e) -> prettyCase ind (prettyReq r c) e)
            s
            (mapToList $ snd <$> m)
      )
      (prettyCase ind (prettyReq (0 :: Int) (0 :: Int)) df id)
      (Map.toList bs)
  MatchSum bs ->
    foldr
      (uncurry $ prettyCase ind . shows)
      id
      (mapToList $ snd <$> bs)
      -- _ -> error "prettyBranches: todo"
  where
    -- prettyReq :: Reference -> CTag -> ShowS
    prettyReq r c =
      showString "REQ("
        . shows r
        . showString ","
        . shows c
        . showString ")"

prettyCase :: Var v => Int -> ShowS -> ANormal v -> ShowS -> ShowS
prettyCase ind sc (ABTN.TAbss vs e) r =
  showString "\n" . indent ind . sc . prettyVars vs
    . showString " ->"
    . prettyANF False (ind + 1) e
    . r
