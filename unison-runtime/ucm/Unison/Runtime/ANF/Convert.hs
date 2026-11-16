{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- | Compiling Unison terms to ANF: closure conversion, lambda lifting
-- and the translation itself. The ANF it produces, and everything the
-- runtime does with it, is in "Unison.Runtime.ANF".
module Unison.Runtime.ANF.Convert
  ( minimizeCyclesOrCrash,
    FloatName (..),
    prettyFloatName,
    close,
    saturate,
    float,
    floatGroup,
    lamLift,
    lamLiftGroup,
    inlineAlias,
    addDefaultCases,
    superNormalize,
    anfTerm,
  )
where

import Control.Lens (snoc, unsnoc)
import Control.Monad.Reader (ReaderT (..), ask, local)
import Control.Monad.State (MonadState (..), State, gets, modify, runState)
import Data.Functor.Compose (Compose (..))
import Data.List hiding (and, or, unsnoc, unzip)
import Data.Map qualified as Map
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as Data.Text
import Unison.ABT qualified as ABT
import Unison.ABT.Normalized qualified as ABTN
import Unison.Blank (nameb)
import Unison.Builtin.Decls qualified as Ty
import Unison.ConstructorReference (ConstructorReference, GConstructorReference (..))
import Unison.Hashing.V2.Convert (hashTermComponentsWithoutTypes)
import Unison.Pattern (SeqOp (..))
import Unison.Pattern qualified as P
import Unison.Prelude
import Unison.PrettyPrintEnv (PrettyPrintEnv, termName)
import Unison.Reference (Id, Reference, Reference' (Builtin, DerivedId))
import Unison.ReferentPrime qualified as Rfn
import Unison.Runtime.ANF
import Unison.Runtime.InternalError (internalBug)
import Unison.Syntax.NamePrinter (prettyHashQualified)
import Unison.Term hiding (Char, Float, List, Record, Ref, Text, arity, float, fresh, record, resolve)
import Unison.Type qualified as Ty
import Unison.Typechecker.Components (minimize')
import Unison.Util.EnumContainers as EC
import Unison.Util.Pretty qualified as Pretty
import Unison.Util.Text qualified as Util.Text
import Unison.Var (Var, typed)
import Unison.Var qualified as Var
import Prelude hiding (abs, and, or, seq, unzip)

closure :: (Var v) => Map v (Set v, Set v) -> Map v (Set v)
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
abstract keep bnd = lamWithoutBindingAnns a evs bnd
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
    lvbs =
      vbs
        <&> \(v, trm) ->
          (v, ABT.annotation trm, (rec keep' . abstract keep' . ABT.substs xpnd) trm)
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
    lamb = lamWithoutBindingAnns a evs (annotate $ lamWithoutBindingAnns a vs lbody)
-- No lambda. Won't be floated, might shadow variable in `keep`.
enclose keep rec (Let1NamedTop' top v bn bd) =
  Just $
    let1'
      top
      [(v, rec keep bn)]
      (rec (Set.delete v keep) bd)
enclose keep rec t@(LamsAnnot vs0 mty vs1 body) =
  Just $ if null evs then lamb else apps' lamb $ map (var a) evs
  where
    -- remove shadowed variables
    keep' = Set.difference keep $ Set.fromList (vs0 ++ vs1)
    fvs = ABT.freeVars t
    evs = Set.toList $ Set.difference fvs keep
    a = ABT.annotation t
    lbody = rec keep' body
    lamb = lamsAnnot a (evs ++ vs0) mty vs1 lbody
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
      | null evs = lamWithoutBindingAnns a [fv] lbody
      | otherwise = lamWithoutBindingAnns a evs lbody
enclose keep rec t@(Match' s0 cs0) = Just $ match a s cs
  where
    a = ABT.annotation t
    s = rec keep s0
    cs = encloseCase a keep rec <$> cs0
enclose _ _ _ = Nothing

encloseCase ::
  (Var v, Monoid a) =>
  a ->
  Set v ->
  (Set v -> Term v a -> Term v a) ->
  MatchCase a (Term v a) ->
  MatchCase a (Term v a)
encloseCase a keep rec0 (MatchCase pats guard body) =
  MatchCase pats (rec <$> guard) (rec body)
  where
    rec (ABT.AbsN' vs bd) =
      ABT.absChain' ((,) a <$> vs) $
        rec0 (keep `Set.difference` Set.fromList vs) bd

newtype Prefix v x = Pfx (Map v [v]) deriving (Show)

instance Functor (Prefix v) where
  fmap _ (Pfx m) = Pfx m

instance (Ord v) => Applicative (Prefix v) where
  pure _ = Pfx Map.empty
  Pfx ml <*> Pfx mr = Pfx $ Map.unionWith common ml mr

common :: (Eq v) => [v] -> [v] -> [v]
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
prefix :: (Ord v) => Term v a -> Prefix v (Term v a)
prefix = ABT.visit \case
  Apps' (Var' u) as -> case splitPfx u as of
    (pf, rest) -> Just $ traverse prefix rest *> pf
  Var' u -> Just . Pfx $ Map.singleton u []
  _ -> Nothing

appPfx :: (Ord v) => Prefix v a -> v -> [v] -> [v]
appPfx (Pfx m) v = maybe (const []) common $ Map.lookup v m

-- Rewrites a term by dropping the first n arguments to every
-- application of `v`. This just assumes such a thing makes sense, as
-- in `beta`, where we've calculated how many arguments to drop by
-- looking at every occurrence of `v`.
dropPrefix :: (Ord v) => (Semigroup a) => v -> Int -> Term v a -> Term v a
dropPrefix _ 0 = id
dropPrefix v n = ABT.visitPure rw
  where
    rw (Apps' f@(Var' u) as)
      | v == u = Just (apps' (var (ABT.annotation f) u) (drop n as))
    rw _ = Nothing

dropPrefixes ::
  (Ord v) => (Semigroup a) => Map v Int -> Term v a -> Term v a
dropPrefixes m = ABT.visitPure rw
  where
    rw (Apps' f@(Var' u) as)
      | Just n <- Map.lookup u m =
          Just (apps' (var (ABT.annotation f) u) (drop n as))
    rw _ = Nothing

-- Performs opposite transformations to those in enclose. Named after
-- the lambda case, which is beta reduction.
beta :: (Var v) => (Monoid a) => (Term v a -> Term v a) -> Term v a -> Maybe (Term v a)
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
      vbs <&> \(v, b0) -> (v,ABT.annotation b0,) $ case b0 of
        LamsNamed' vs b
          | Just n <- Map.lookup v m ->
              lamWithoutBindingAnns (ABT.annotation b0) (drop n vs) (dropPrefixes m b)
        -- shouldn't happen
        b -> dropPrefixes m b

    lbd = dropPrefixes m bd
beta rec (Let1NamedTop' top v l@(LamsNamed' vs bd) (rec -> e))
  | n > 0 = Just $ let1' top [(v, lamb)] (dropPrefix v n e)
  | otherwise = Nothing
  where
    lamb = lamWithoutBindingAnns al (drop n vs) (bd)
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
      Just $ apps' (lamWithoutBindingAnns al (drop n vs) (rec body)) (drop n as)
  | otherwise = Nothing
  where
    al = ABT.annotation l
    matchVars !n (u : us) (Var' v : as) | u == v = matchVars (1 + n) us as
    matchVars n _ _ = n
beta _ _ = Nothing

isStructured :: (Var v) => Term v a -> Bool
isStructured (Var' {}) = False
isStructured (Lam' {}) = False
isStructured (Nat' {}) = False
isStructured (Int' {}) = False
isStructured (Float' {}) = False
isStructured (Text' {}) = False
isStructured (Char' {}) = False
isStructured (Constructor' {}) = False
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

data FloatSeg v = FSRef Reference | FSText Text | FSVar v

data FloatName v = FloatName [FloatSeg v]

extendName :: FloatSeg v -> FloatName v -> FloatName v
extendName s (FloatName ss) = FloatName $ s : ss

prettyFloatName ::
  (Var v) => PrettyPrintEnv -> FloatName v -> Pretty.Pretty Pretty.ColorText
prettyFloatName ppe (FloatName ts) =
  Pretty.sep "$" . fmap prettySeg $ reverse ts
  where
    prettySeg (FSText tx) = Pretty.text tx
    prettySeg (FSVar v) = Pretty.text $ Var.name v
    prettySeg (FSRef r) =
      Pretty.syntaxToColor
        . prettyHashQualified
        . termName ppe
        $ Rfn.Ref' r

data FloatState v a = FS
  { lambdas :: Int,
    path :: FloatName v,
    ctxVars :: Set v,
    floated :: [(v, Term v a)],
    floatNames :: [(v, FloatName v)],
    decomp :: [(v, Term v a)]
  }

emptyState :: FloatState v a
emptyState = FS 0 (FloatName []) Set.empty [] [] []

type FloatM v a r = State (FloatState v a) r

addVars :: (Ord v) => Set v -> FloatM v a ()
addVars new = modify \st -> st {ctxVars = new <> ctxVars st}

inLocal :: FloatSeg v -> FloatM v a r -> FloatM v a r
inLocal nm act = do
  st <- get
  put $
    st
      { path = extendName nm $ path st,
        lambdas = 0
      }
  r <- act
  modify \st' -> st' {path = path st, lambdas = lambdas st}
  pure r

inLocalLam :: FloatM v a r -> FloatM v a r
inLocalLam act = do
  n <- gets lambdas
  inLocal (FSText $ "Lambda" <> Data.Text.pack (show n)) act

addFloated ::
  [(v, FloatSeg v, Term v a)] -> [(v, Term v a)] -> FloatM v a ()
addFloated fln dc = modify \st ->
  let fl = fln <&> \(v, _, tm) -> (v, tm)
      fn = fln <&> \(v, n, _) -> (v, extendName n $ path st)
   in st
        { floated = fl <> floated st,
          floatNames = fn <> floatNames st,
          decomp = dc <> decomp st
        }

nameLambda :: (Var v) => Maybe v -> FloatM v a Text
nameLambda (Just v) = pure $ Var.name v
nameLambda Nothing = state \st ->
  let n = lambdas st
   in ("Lambda" <> Data.Text.pack (show n), st {lambdas = n + 1})

freshFloat :: (Var v) => Set v -> v -> v
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

groupFloater ::
  (Var v, Monoid a) =>
  (Term v a -> FloatM v a (Term v a)) ->
  [(v, Term v a)] ->
  FloatM v a (Map v v)
groupFloater rec vbs = do
  cvs <- gets ctxVars
  let shadows =
        [ (v, freshFloat cvs v)
        | (v, _) <- vbs,
          Set.member v cvs
        ]
      shadowMap = Map.fromList shadows
      rn v = Map.findWithDefault v v shadowMap
      shvs = Set.fromList $ map (rn . fst) vbs
      h (v, b) =
        (rn v,nm,) <$> inLocal nm (rec' (ABT.renames shadowMap b))
        where
          nm = FSVar v
  addVars shvs
  fvnbs <- traverse h vbs
  let dvbs = fmap (\(v, b) -> (rn v, deannotate b)) vbs
  addFloated fvnbs dvbs
  pure shadowMap
  where
    rec' b
      | LamsAnnot vs0 mty vs1 bd <- b =
          lamsAnnot a vs0 mty vs1 <$> rec bd
      where
        a = ABT.annotation b
    rec' b = rec b

letFloater ::
  (Var v, Monoid a) =>
  (Term v a -> FloatM v a (Term v a)) ->
  [(v, Term v a)] ->
  Term v a ->
  FloatM v a (Term v a)
letFloater rec vbs e = do
  shadowMap <- groupFloater rec vbs
  pure $ ABT.renames shadowMap e

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
  get >>= \FS {ctxVars, floated} ->
    case find p floated of
      Just (v, _) -> pure v
      Nothing -> do
        let v = ABT.freshIn ctxVars $ fromMaybe (typed Var.Float) mv
        nm <- nameLambda mv
        addVars $ Set.singleton v
        addFloated
          [(v, FSText nm, lamWithoutBindingAnns a vs bd)]
          (floatDecomp closed v tm)
        pure v
  where
    tgt = unannotate (lamWithoutBindingAnns a vs bd)
    p (_, flam) = unannotate flam == tgt

floatDecomp ::
  Bool -> v -> Term v a -> [(v, Term v a)]
floatDecomp True v b = [(v, b)]
floatDecomp False _ _ = []

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
      lm@(LamsNamed' vs bd) | top -> lamWithoutBindingAnns a vs <$> rec bd
        where
          a = ABT.annotation lm
      tm -> rec tm
floater _ rec (Let1Named' v b e)
  | LamsAnnot vs0 _ vs1 bd <- b =
      Just $
        rec bd
          >>= lamFloater True b (Just v) a (vs0 ++ vs1)
          >>= \lv -> rec $ ABT.renames (Map.singleton v lv) e
  where
    a = ABT.annotation b
floater top rec tm@(LamsAnnot vs0 mty vs1 bd)
  | top = Just $ lamsAnnot a vs0 mty vs1 <$> inLocalLam (rec bd)
  | otherwise = Just $ do
      bd <- inLocalLam $ rec bd
      lv <- lamFloater True tm Nothing a (vs0 ++ vs1) bd
      pure $ var a lv
  where
    a = ABT.annotation tm
floater _ _ _ = Nothing

postFloat ::
  (Var v) =>
  (Monoid a) =>
  Map v Reference ->
  FloatState v a ->
  ( [(v, Term v a)],
    [(v, Id)],
    [(Reference, FloatName v)],
    [(Reference, Term v a)],
    [(Reference, Term v a)]
  )
postFloat orig (FS {floatNames, floated, decomp}) =
  ( subs,
    subvs,
    mapMaybe (fmap $ fmap originals) nms,
    tops,
    decomp >>= \(v, tm) ->
      let stm = open $ ABT.substs dsubs tm
       in (subm Map.! v, stm) : [(r, stm) | Just r <- [Map.lookup v orig]]
  )
  where
    m =
      fmap (fmap deannotate)
        . snd {- we ignore the hashing warnings in the runtime; they're not relevant -}
        . hashTermComponentsWithoutTypes
        . Map.fromList
        $ floated
    vname = Map.fromList floatNames
    trips = Map.toList m
    f (v, (id, tm)) =
      ((v, id), (rf,) <$> Map.lookup v vname, (v, idtm), (rf, tm))
      where
        rf = DerivedId id
        idtm = ref (ABT.annotation tm) rf
    unzip4 [] = ([], [], [], [])
    unzip4 ((a, b, c, d) : (unzip4 -> ~(as, bs, cs, ds))) =
      (a : as, b : bs, c : cs, d : ds)
    (subvs, nms, subs, tops) = unzip4 $ map f trips
    subm = fmap DerivedId (Map.fromList subvs)
    dsubs = Map.toList $ Map.map (ref mempty) orig <> Map.fromList subs

    originals (FloatName ss) =
      FloatName $
        ss <&> \case
          FSVar v
            | Just r <- Map.lookup v orig -> FSRef r
          seg -> seg

float ::
  (Var v) =>
  (Monoid a) =>
  Map v Reference ->
  Term v a ->
  ( Term v a,
    Map Reference Reference,
    Map Reference (FloatName v),
    [(Reference, Term v a)],
    [(Reference, Term v a)]
  )
float orig tm = case runState go0 emptyState of
  (bd, st) -> case postFloat orig st of
    (subs, subvs, fnames, tops, dcmp) ->
      ( letRec' True [] . ABT.substs subs . deannotate $ bd,
        Map.fromList . mapMaybe f $ subvs,
        Map.fromList fnames,
        tops,
        dcmp
      )
  where
    f (v, i) = (,DerivedId i) <$> Map.lookup v orig
    go0 = fromMaybe (go tm) (floater True go tm)
    go = ABT.visit $ floater False go

floatGroup ::
  (Var v) =>
  (Monoid a) =>
  Map v Reference ->
  [(v, Term v a)] ->
  ( [(v, Id)],
    [(Reference, FloatName v)],
    [(Reference, Term v a)],
    [(Reference, Term v a)]
  )
floatGroup orig grp = case runState go0 emptyState of
  (_, st) -> case postFloat orig st of
    (_, subvs, fnames, tops, dcmp) -> (subvs, fnames, tops, dcmp)
  where
    go = ABT.visit $ floater False go
    go0 = groupFloater go grp

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

-- Matches a lambda term with an annotation in the middle, like:
--
--   w x -> (y z -> ...) : ...
--
-- This can occur due to enclosing an annotated lambda term with free
-- variables.
pattern LamsAnnot ::
  [v] -> Maybe (Ty.Type v a) -> [v] -> Term v a -> Term v a
pattern LamsAnnot us mty vs bd <-
  (unLamsAnnot -> Just (us, mty, vs, bd))

-- Builds a lambda term with arguments separated by an annotation, as
-- above. Just a convenience function that reverses the above pattern.
lamsAnnot ::
  (Var v) => a -> [v] -> Maybe (Ty.Type v a) -> [v] -> Term v a -> Term v a
lamsAnnot a us mty vs bd =
  lamWithoutBindingAnns a us
    . maybe id (flip $ ann a) mty
    . lamWithoutBindingAnns a vs
    $ bd

deannotate :: (Var v) => Term v a -> Term v a
deannotate = ABT.visitPure $ \case
  Ann' c _ -> Just $ deannotate c
  _ -> Nothing

lamLift ::
  (Var v) =>
  (Monoid a) =>
  Map v Reference ->
  Term v a ->
  ( Term v a,
    Map Reference Reference,
    Map Reference (FloatName v),
    [(Reference, Term v a)],
    [(Reference, Term v a)]
  )
lamLift orig = float orig . close Set.empty

lamLiftGroup ::
  (Var v) =>
  (Monoid a) =>
  Map v Reference ->
  [(v, Term v a)] ->
  ( [(v, Id)],
    [(Reference, FloatName v)],
    [(Reference, Term v a)],
    [(Reference, Term v a)]
  )
lamLiftGroup orig gr = floatGroup orig . (fmap . fmap) (close keep) $ gr
  where
    keep = Set.fromList $ map fst gr

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
            Just . lamWithoutBindingAnns mempty vs . apps' f $ args' ++ nargs
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

addDefaultCases :: (Var v) => (Monoid a) => Text -> Term v a -> Term v a
addDefaultCases = ABT.visitPure . defaultCaseVisitor

defaultCaseVisitor ::
  (Var v) => (Monoid a) => Text -> Term v a -> Maybe (Term v a)
defaultCaseVisitor func m@(Match' scrut cases)
  | scrut <- addDefaultCases func scrut,
    cases <- fmap (addDefaultCases func) <$> cases =
      Just $ match a scrut (cases ++ [dflt])
  where
    a = ABT.annotation m
    v = Var.freshIn mempty $ typed Var.Blank
    txt = "pattern match failure in function `" <> func <> "`"
    msg = text a txt
    bu = ref a (Builtin "bug")
    dflt =
      MatchCase (P.Var a) Nothing
        . ABT.abs' a v
        $ apps bu [(a, Ty.tupleTerm [msg, var a v])]
defaultCaseVisitor _ _ = Nothing

inlineAlias :: (Var v) => (Monoid a) => Term v a -> Term v a
inlineAlias = ABT.visitPure $ \case
  Let1Named' v b@(Var' _) e -> Just . inlineAlias $ ABT.subst v b e
  _ -> Nothing

minimizeCyclesOrCrash :: (Var v, Ord a) => Term v a -> Term v a
minimizeCyclesOrCrash t = case minimize' t of
  Right t -> t
  Left e ->
    internalBug [] $
      "tried to minimize let rec with duplicate definitions: "
        ++ show (fst <$> toList e)

groupVars :: ANFM v (Set v)
groupVars = ask

bindLocal :: (Ord v) => [v] -> ANFM v r -> ANFM v r
bindLocal vs = local (Set.\\ Set.fromList vs)

freshANF :: (Var v) => Word64 -> v
freshANF fr = Var.freshenId fr $ typed Var.ANFBlank

fresh :: (Var v) => ANFM v v
fresh = state $ \(fr, bnd, cs) -> (freshANF fr, (fr + 1, bnd, cs))

contextualize :: (Var v) => DNormal v -> ANFM v (Ctx v, v)
contextualize (_, TVar cv) = do
  gvs <- groupVars
  if cv `Set.notMember` gvs
    then pure (mempty, cv)
    else do
      bv <- fresh
      d <- Indirect <$> binder
      pure (directed [ST1 d bv BX $ TApv cv []], bv)
contextualize (d0, tm) = do
  fv <- fresh
  d <- bindDirection d0
  pure ((d0, Seq.singleton (ST1 d fv BX tm)), fv)

binder :: ANFM v Word16
binder = state $ \(fr, bnd, cs) -> (bnd, (fr, bnd + 1, cs))

bindDirection :: Direction a -> ANFM v (Direction Word16)
bindDirection = traverse (const binder)

record :: (Var v) => (v, SuperNormal Reference v) -> ANFM v ()
record p = modify $ \(fr, bnd, to) -> (fr, bnd, p : to)

superNormalize :: (Var v) => Term v a -> SuperGroup Reference v
superNormalize tm = Rec l c
  where
    (bs, e)
      | LetRecNamed' bs e <- tm = (bs, e)
      | otherwise = ([], tm)
    grp = Set.fromList $ fst <$> bs
    comp = traverse_ superBinding bs *> toSuperNormal e
    subc = runReaderT comp grp
    (c, (_, _, l)) = runState subc (0, 1, [])

superBinding :: (Var v) => (v, Term v a) -> ANFM v ()
superBinding (v, tm) = do
  nf <- toSuperNormal tm
  modify $ \(cvs, bnd, ctx) -> (cvs, bnd, (v, nf) : ctx)

toSuperNormal :: (Var v) => Term v a -> ANFM v (SuperNormal Reference v)
toSuperNormal tm = do
  grp <- groupVars
  if not . Set.null . (Set.\\ grp) $ freeVars tm
    then internalBug [] $ "free variables in supercombinator: " ++ show tm
    else
      Lambda (BX <$ vs) . ABTN.TAbss vs . snd
        <$> bindLocal vs (anfTerm body)
  where
    (vs, body) = fromMaybe ([], tm) $ unLams' tm

anfTerm :: (Var v) => Term v a -> ANFM v (DNormal v)
anfTerm tm = f <$> anfBlock tm
  where
    -- f = uncurry (liftA2 TBinds)
    f ((_, Seq.Empty), dtm) = dtm
    f ((_, cx), (_, tm)) = (Indirect (), TBinds (toList cx) tm)

floatableCtx :: (Var v) => Ctx v -> Bool
floatableCtx = all p . snd
  where
    p (LZ _ _ _) = True
    p (ST _ _ _ tm) = q tm
    q (TLit _) = True
    q (TVar _) = True
    q (TCon _ _ _) = True
    q _ = False

anfHandled :: (Var v) => Term v a -> ANFM v (Ctx v, DNormal v)
anfHandled body =
  anfBlock body >>= \case
    (ctx, (_, t@TCon {})) ->
      fresh <&> \v ->
        (ctx <> pure (Seq.singleton (ST1 Direct v BX t)), pure $ TVar v)
    (ctx, (_, t@(TLit l))) ->
      fresh <&> \v ->
        (ctx <> pure (Seq.singleton (ST1 Direct v cc t)), pure $ TVar v)
      where
        cc = case l of T {} -> BX; LM {} -> BX; LY {} -> BX; _ -> UN
    p -> pure p

-- Helper function for renaming a variable arising from a
--   let v = u
-- binding during ANF translation. Renames a variable in a
-- context, and returns an indication of whether the varible
-- was shadowed by one of the context bindings.
--
-- Note: this assumes that `u` is not bound by any of the context
-- entries, as no effort is made to rename them to avoid capturing
-- `u`.
renameCtx :: (Var v) => v -> v -> Ctx v -> (Ctx v, Bool)
renameCtx v u (d, ctx) | (ctx, b) <- renameCtes v u (toList ctx) = ((d, Seq.fromList ctx), b)

-- As above, but without the Direction.
renameCtes :: (Var v) => v -> v -> [Cte v] -> ([Cte v], Bool)
renameCtes v u = rn []
  where
    swap w
      | w == v = u
      | otherwise = w

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

-- Simultaneously renames variables in a list of context entries.
--
-- Assumes that the variables being renamed to are not bound by the
-- context entries, so that it is unnecessary to rename them.
renamesCtes :: (Var v) => Map v v -> [Cte v] -> [Cte v]
renamesCtes rn = map f
  where
    swap w
      | Just u <- Map.lookup w rn = u
      | otherwise = w

    f (ST d vs ccs b) = ST d vs ccs (ABTN.renames rn b)
    f (LZ v r as) = LZ v (second swap r) (map swap as)

-- Calculates the free variables occurring in a context. This
-- consists of the free variables in the expressions being bound,
-- but with previously bound variables subtracted.
freeVarsCtx :: (Ord v) => Ctx v -> Set v
freeVarsCtx = freeVarsCte . toList . snd

freeVarsCte :: (Ord v) => [Cte v] -> Set v
freeVarsCte = foldr m Set.empty
  where
    m (ST _ vs _ bn) rest =
      ABTN.freeVars bn `Set.union` (rest Set.\\ Set.fromList vs)
    m (LZ v r as) rest =
      Set.fromList (either (const id) (:) r as)
        `Set.union` Set.delete v rest

-- Conditionally freshens a list of variables. The predicate
-- argument selects which variables to freshen, and the set is a set
-- of variables to avoid for freshness. The process ensures that the
-- result is mutually fresh, and returns a new set of variables to
-- avoid, which includes the freshened variables.
--
-- Presumably any variables selected by the predicate should be
-- included in the set, but the set may contain additional variables
-- to avoid, when freshening.
freshens :: (Var v) => (v -> Bool) -> Set v -> [v] -> (Set v, [v])
freshens p avoid0 vs =
  mapAccumL f (Set.union avoid0 (Set.fromList vs)) vs
  where
    f avoid v
      | p v, u <- Var.freshIn avoid v = (Set.insert u avoid, u)
      | otherwise = (avoid, v)

-- Freshens the variable bindings in a context to avoid a set of
-- variables. Returns the renaming necessary for anything that was
-- bound in the freshened context.
--
-- Note: this only freshens if it's necessary to avoid variables in
-- the _original_ set. We need to keep track of other variables to
-- avoid when making up new names for those, but it it isn't
-- necessary to freshen variables to remove shadowing _within_ the
-- context, since it is presumably already correctly formed.
freshenCtx :: (Var v) => Set v -> Ctx v -> (Map v v, Ctx v)
freshenCtx avoid0 (d, ctx) =
  case go lavoid Map.empty [] $ reverse (toList ctx) of
    (rn, ctx) -> (rn, (d, Seq.fromList ctx))
  where
    -- precalculate all variable occurrences in the context to just
    -- completely avoid those as well.
    lavoid =
      foldl (flip $ Set.union . cteVars) avoid0 ctx

    go _ rns fresh [] = (rns, fresh)
    go avoid rns fresh (bn : bns) = case bn of
      LZ v r as
        | v `Set.member` avoid0,
          u <- Var.freshIn avoid v,
          (fresh, _) <- renameCtes v u fresh,
          avoid <- Set.insert u avoid,
          rns <- Map.alter (Just . fromMaybe u) v rns ->
            go avoid rns (LZ u r as : fresh) bns
      ST d vs ccs expr
        | (avoid, us) <- freshens (`Set.member` avoid0) avoid vs,
          rn <- Map.fromList (filter (uncurry (/=)) $ zip vs us),
          not (Map.null rn),
          fresh <- renamesCtes rn fresh,
          -- Note: rns union left-biased, so inner contexts take
          -- priority.
          rns <- Map.union rns rn ->
            go avoid rns (ST d us ccs expr : fresh) bns
      _ -> go avoid rns (bn : fresh) bns

anfBlock :: (Ord v, Var v) => Term v a -> ANFM v (Ctx v, DNormal v)
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
            [ (0, ([], UFalse)),
              (1, ([], tmr))
            ]
  pure (lctx, (Indirect () <> d, tree))
anfBlock (Or' l r) = do
  (lctx, vl) <- anfArg l
  (d, tmr) <- anfTerm r
  let tree =
        TMatch vl . MatchDataCover Ty.booleanRef $
          mapFromList
            [ (1, ([], UTrue)),
              (0, ([], tmr))
            ]
  pure (lctx, (Indirect () <> d, tree))
anfBlock (Handle' h body) =
  anfArg h >>= \(hctx, vh) ->
    anfHandled body >>= \case
      (ctx, (_, TCom f as)) | floatableCtx ctx -> do
        v <- fresh
        pure
          ( hctx <> ctx <> pure (Seq.singleton (LZ v (Left f) as)),
            (Indirect (), TApp (FVar vh) [v])
          )
      (ctx, (_, TApv f as)) | floatableCtx ctx -> do
        v <- fresh
        pure
          ( hctx <> ctx <> pure (Seq.singleton (LZ v (Right f) as)),
            (Indirect (), TApp (FVar vh) [v])
          )
      (ctx, (_, TVar v)) | floatableCtx ctx -> do
        pure (hctx <> ctx, (Indirect (), TApp (FVar vh) [v]))
      p@(_, _) ->
        internalBug [] $ "handle body should be a simple call: " ++ show p
anfBlock (Match' scrut cas) = do
  (sctx, sc) <- anfBlock scrut
  (cx, v) <- contextualize sc
  (d, brn) <- anfCases v cas
  fmap (first ((Indirect () <> d) <>)) <$> case brn of
    AccumDefault (TBinds (directed -> dctx) df) -> do
      pure (sctx <> cx <> dctx, pure df)
    AccumRequest _ Nothing ->
      internalBug [] "anfBlock: AccumRequest without default"
    AccumPure (ABTN.TAbss us bd)
      | [u] <- us,
        TBinds (directed -> bx) bd <- bd ->
          case cx of
            (_, Seq.Empty) -> do
              d0 <- Indirect <$> binder
              pure (sctx <> pure (Seq.singleton (ST1 d0 u BX (TFrc v))) <> bx, pure bd)
            (d0, ST1 d1 _ BX tm Seq.:<| Seq.Empty) ->
              pure (sctx <> (d0, Seq.singleton (ST1 d1 u BX tm)) <> bx, pure bd)
            _ -> internalBug [] "anfBlock|AccumPure: impossible"
      | otherwise -> internalBug [] "pure handler with too many variables"
    AccumRequest abr (Just df) -> do
      (r, vs) <- do
        r <- fresh
        v <- fresh
        (hfvs, hcomb) <- makeHandler v abr df
        record (r, hcomb)
        pure (r, hfvs)
      hv <- fresh
      let (d, msc)
            | (d, ST1 _ _ BX tm Seq.:<| Seq.Empty) <- cx = (d, tm)
            | (_, ST _ _ _ _ Seq.:<| Seq.Empty) <- cx =
                internalBug [] "anfBlock: impossible"
            | otherwise = (Indirect (), TFrc v)
      pure
        ( sctx <> pure (Seq.singleton (LZ hv (Right r) vs)),
          (d, THnd (Map.keys abr) hv Nothing msc)
        )
    AccumText df cs ->
      pure (sctx <> cx, pure . TMatch v $ MatchText cs df)
    AccumBytes df cs ->
      pure (sctx <> cx, pure . TMatch v $ MatchBytes cs df)
    AccumIntegral r df cs ->
      pure (sctx <> cx, pure $ TMatch v $ MatchNumeric r cs df)
    AccumData r df cs ->
      pure (sctx <> cx, pure . TMatch v $ MatchData r cs df)
    AccumRec rs bd -> do
      pure (sctx <> cx, pure $ TMatch v $ MatchRec rs bd)
    AccumSeqEmpty _ ->
      internalBug [] "anfBlock: non-exhaustive AccumSeqEmpty"
    AccumSeqView en (Just em) bd -> do
      r <- fresh
      let op
            | SLeft <- en = Builtin "List.viewl"
            | otherwise = Builtin "List.viewr"
      b <- binder
      pure
        ( sctx
            <> cx
            <> (Indirect (), Seq.singleton (ST1 (Indirect b) r BX (TCom op [v]))),
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
      internalBug [] "anfBlock: non-exhaustive AccumSeqView"
    AccumSeqSplit en n mdf bd -> do
      i <- fresh
      r <- fresh
      s <- fresh
      b <- binder
      let split = ST1 (Indirect b) r BX (TCom op [i, v])
      pure
        ( sctx <> cx <> directed [lit i, split],
          pure . TMatch r . MatchDataCover Ty.seqViewRef $
            mapFromList
              [ (fromIntegral Ty.seqViewEmpty, ([], df s)),
                (fromIntegral Ty.seqViewElem, ([BX, BX], bd))
              ]
        )
      where
        op
          | SLeft <- en = Builtin "List.splitLeft"
          | otherwise = Builtin "List.splitRight"
        lit i = ST1 Direct i BX (TBLit . N $ fromIntegral n)
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
      (brn, bctx) <- fixupBctx bctx ectx ce
      u <- pure $ Map.findWithDefault u u brn
      (ectx, shaded) <- pure $ renameCtx v u ectx
      ce <- pure $ if shaded then ce else ABTN.rename v u <$> ce
      pure (bctx <> ectx, ce)
    (bctx, (d0, cb)) -> bindLocal [v] $ do
      (ectx, ce) <- anfBlock e
      d <- bindDirection d0
      (brn, bctx) <- fixupBctx bctx ectx ce
      cb <- pure $ ABTN.renames brn cb
      let octx = bctx <> directed [ST1 d v BX cb] <> ectx
      pure (octx, ce)
  where
    fixupBctx bctx ectx (_, ce) =
      pure $ freshenCtx (Set.union ecfvs efvs) bctx
      where
        ecfvs = freeVarsCtx ectx
        efvs = ABTN.freeVars ce
anfBlock (Apps' (Blank' b) args) = do
  nm <- fresh
  (actx, cas) <- anfArgs args
  pure
    ( actx <> pure (Seq.singleton (ST1 Direct nm BX (TLit (T msg)))),
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
anfBlock (Lit' l) =
  pure (mempty, pure $ TBLit l)
anfBlock (Ref' r) = pure (mempty, (Indirect (), TCom r []))
anfBlock (Blank' b) = do
  nm <- fresh
  ev <- fresh
  pure
    ( pure $
        Seq.fromList
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
anfBlock (Record' fields) = fmap (pure . TRec recSchema) <$> anfArgs tms
  where
    recSchema = RecordSchema (Map.keysSet fields)
    tms = toList fields
anfBlock (RecordSet' field r v) = fmap (pure . TRecSet field) <$> anfArgs [r, v]
-- A record-set block is sugar: expand it to the match and `RecordSet` chain it
-- means, then compile that.
anfBlock t@(RecordSetBlock' _ _ _) = anfBlock (expandRecordSetBlock t)
anfBlock t = internalBug [] $ "anf: unhandled term: " ++ show t

type ReqBranches ref v =
  Map Reference (EnumMap CTag ([Mem], ANormal ref v))

makeHandler ::
  (Var v) => v -> ReqBranches Reference v -> ANormal Reference v -> ANFM v ([v], SuperNormal Reference v)
makeHandler v abr df = do
  hfvs <-
    groupVars <&> \gvs ->
      Set.toList $ ABTN.freeVars hfb `Set.difference` gvs
  pure (hfvs, Lambda (BX <$ hfvs ++ [v]) $ ABTN.TAbss hfvs hfb)
  where
    hfb = ABTN.TAbs v . TMatch v $ MatchRequest (Map.toList abr) df

-- Note: this assumes that patterns have already been translated
-- to a state in which every case matches a single layer of data,
-- with no guards, and no variables ignored. This is not checked
-- completely.
anfInitCase ::
  (Var v) =>
  v ->
  MatchCase p (Term v a) ->
  ANFD v (BranchAccum v)
anfInitCase u (MatchCase p guard (ABT.AbsN' vs bd))
  | Just _ <- guard = internalBug [] "anfInitCase: unexpected guard"
  | P.Unbound _ <- p,
    [] <- vs =
      AccumDefault <$> anfBody bd
  | P.Var _ <- p,
    [v] <- vs =
      AccumDefault . ABTN.rename v u <$> anfBody bd
  | P.Var _ <- p =
      internalBug [] $ "vars: " ++ show (length vs)
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
  | P.Bytes _ b <- p,
    [] <- vs =
      AccumBytes Nothing . Map.singleton b <$> anfBody bd
  | P.Constructor _ (ConstructorReference r t) ps <- p = do
      (,)
        <$> expandBindings ps vs
        <*> anfBody bd
        <&> \(us, bd) ->
          AccumData r Nothing . EC.mapSingleton (fromIntegral t) . (BX <$ us,) $ ABTN.TAbss us bd
  | P.RecordLiteral _ fields <- p = do
      (,)
        <$> expandBindings (Map.elems fields) vs
        <*> anfBody bd
        <&> \(us, bd) -> AccumRec (RecordSchema $ Map.keysSet fields) $ ABTN.TAbss us bd
  | P.EffectPure _ q <- p =
      (,)
        <$> expandBindings [q] vs
        <*> anfBody bd
        <&> \(us, bd) -> AccumPure $ ABTN.TAbss us bd
  | P.EffectBind _ (ConstructorReference r t) ps pk <- p = do
      (,,)
        <$> expandBindings (snoc ps pk) vs
        <*> Compose (pure <$> fresh)
        <*> anfBody bd
        <&> \(exp, kf, bd) ->
          let (us, uk) =
                maybe (internalBug [] "anfInitCase: unsnoc impossible") id $
                  unsnoc exp
              jn = Builtin "jumpCont"
           in flip AccumRequest Nothing
                . Map.singleton r
                . EC.mapSingleton (fromIntegral t)
                . (BX <$ us,)
                . ABTN.TAbss us
                . TShift r kf
                $ TName uk (Left jn) [kf] bd
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
  internalBug [] $ "anfInitCase: unexpected pattern: " ++ show p

expandBindings' ::
  (Var v) =>
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

expandBindings :: (Var v) => [P.Pattern p] -> [v] -> ANFD v [v]
expandBindings ps vs =
  Compose . state $ \(fr, bnd, co) -> case expandBindings' fr ps vs of
    Left err -> internalBug [] $ err ++ " " ++ show (ps, vs)
    Right (fr, l) -> (pure l, (fr, bnd, co))

anfCases ::
  (Var v) =>
  v ->
  [MatchCase p (Term v a)] ->
  ANFM v (Directed () (BranchAccum v))
anfCases u = getCompose . fmap fold . traverse (anfInitCase u)

anfFunc ::
  (Var v) => Term v a -> ANFM v (Ctx v, Directed () (Func Reference v))
anfFunc (Var' v) = pure (mempty, (Indirect (), FVar v))
anfFunc (Ref' r) = pure (mempty, (Indirect (), FComb r))
anfFunc (Constructor' (ConstructorReference r t)) = pure (mempty, (Direct, FCon r $ fromIntegral t))
anfFunc (Request' (ConstructorReference r t)) = pure (mempty, (Indirect (), FReq r $ fromIntegral t))
anfFunc tm = do
  (fctx, ctm) <- anfBlock tm
  (cx, v) <- contextualize ctm
  pure (fctx <> cx, (Indirect (), FVar v))

anfArg :: (Var v) => Term v a -> ANFM v (Ctx v, v)
anfArg tm = do
  (ctx, ctm) <- anfBlock tm
  (cx, v) <- contextualize ctm
  pure (ctx <> cx, v)

anfArgs :: (Var v) => [Term v a] -> ANFM v (Ctx v, [v])
anfArgs tms = first fold . unzip <$> traverse anfArg tms
