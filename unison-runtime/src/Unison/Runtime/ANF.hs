{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Runtime.ANF
  ( minimizeCyclesOrCrash,
    pattern TVar,
    pattern TLit,
    pattern TBLit,
    pattern TApp,
    pattern TApv,
    pattern TCom,
    pattern TCon,
    pattern UFalse,
    pattern UTrue,
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
    pattern TDiscard,
    pattern TLocal,
    pattern TUpdate,
    FloatName (..),
    prettyFloatName,
    Mem (..),
    Lit (..),
    Cacheability (..),
    Direction (..),
    SuperNormal (..),
    arity,
    SuperGroup (..),
    arities,
    POp (..),
    close,
    saturate,
    float,
    floatGroup,
    lamLift,
    lamLiftGroup,
    litRef,
    inlineAlias,
    addDefaultCases,
    ANormalF (.., AApv, ACom, ACon, AKon, AReq, APrm, AFOp),
    ANormal,
    RTag,
    CTag,
    PackedTag (..),
    Tag (..),
    GroupRef (..),
    Code (..),
    ValList,
    Value (..),
    Cont (..),
    BLit (..),
    packTags,
    unpackTags,
    maskTags,
    ANFM,
    Branched (.., MatchDataCover),
    Func (..),
    SGEqv (..),
    equivocate,
    superNormalize,
    anfTerm,
    codeGroup,
    valueTermLinks,
    collectValueLinks,
    valueLinks,
    groupTermLinks,
    replaceConstructors,
    replaceFunctions,
    foldGroup,
    foldGroupLinks,
    overGroup,
    overGroupLinks,
    traverseGroup,
    traverseGroupLinks,
    normalLinks,
    prettyGroup,
    prettySuperNormal,
    prettyANF,
  )
where

import Control.Lens (snoc, unsnoc)
import Control.Monad.Reader (ReaderT (..), ask, local)
import Control.Monad.State (MonadState (..), State, gets, modify, runState)
import Data.Bifoldable (Bifoldable (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Functor.Compose (Compose (..))
import Data.List hiding (and, or, unsnoc, unzip)
import Data.Map qualified as Map
import Data.Ord (comparing)
import Data.Set qualified as Set
import Data.Text qualified as Data.Text
import Data.Text qualified as Text
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
import Unison.Reference (Id, Reference, Reference' (Builtin, DerivedId), toShortHash)
import Unison.ReferentPrime qualified as Rfn
import Unison.Runtime.ANF.POp (POp (..))
import Unison.Runtime.Array qualified as PA
import Unison.Runtime.Foreign.Function.Type (ForeignFunc (..))
import Unison.Runtime.InternalError (internalBug)
import Unison.Runtime.Referenced (Referential (..))
import Unison.Runtime.TypeTags (CTag (..), PackedTag (..), RTag (..), Tag (..), maskTags, packTags, unpackTags)
import Unison.ShortHash (shortenTo)
import Unison.Symbol (Symbol)
import Unison.Syntax.NamePrinter (prettyHashQualified, prettyShortHash)
import Unison.Term hiding (Char, Float, List, Ref, Text, arity, float, fresh, resolve)
import Unison.Type qualified as Ty
import Unison.Typechecker.Components (minimize')
import Unison.Util.Bytes (Bytes)
import Unison.Util.EnumContainers as EC
import Unison.Util.Pretty qualified as Pretty
import Unison.Util.Text qualified as Util.Text
import Unison.Var (Var, typed)
import Unison.Var qualified as Var
import Prelude hiding (abs, and, or, seq, unsnoc, unzip)

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

replaceConstructors ::
  (Ord ref, Var v) =>
  Map ref (Map CTag ForeignFunc) ->
  SuperGroup ref v ->
  SuperGroup ref v
replaceConstructors reps (Rec bs entry) =
  Rec (fmap go0 <$> bs) (go0 entry)
  where
    go0 (Lambda ccs body) = Lambda ccs $ ABTN.visitPure f body

    f (TApp (FCon r c) as) = do
      cs <- Map.lookup r reps
      ff <- Map.lookup c cs
      pure $ TApp (FPrim (Right ff)) as
    f _ = Nothing

replaceFunctions ::
  (Ord ref, Var v) =>
  Map ref ref ->
  SuperGroup ref v ->
  SuperGroup ref v
replaceFunctions reps (Rec bs entry) =
  Rec (fmap go0 <$> bs) (go0 entry)
  where
    go0 (Lambda ccs body) = Lambda ccs $ ABTN.visitPure f body

    f (TApp (FComb r) as) =
      Map.lookup r reps <&> \r -> TApp (FComb r) as
    f _ = Nothing

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

data Mem = UN | BX deriving (Eq, Ord, Show, Enum)

-- Context entries with evaluation strategy
data CTE v s
  = ST (Direction Word16) [v] [Mem] s
  | LZ v (Either Reference v) [v]
  deriving (Show)

pattern ST1 :: Direction Word16 -> v -> Mem -> s -> CTE v s
pattern ST1 d v m s = ST d [v] [m] s

-- All variables, both bound and free occurring in a CTE. This is
-- useful for avoiding both free and bound variables when
-- freshening.
cteVars :: (Ord v) => Cte v -> Set v
cteVars (ST _ vs _ e) = Set.fromList vs `Set.union` ABTN.freeVars e
cteVars (LZ v r as) = Set.fromList (either (const id) (:) r $ v : as)

data ANormalF ref v e
  = ALet (Direction Word16) [Mem] e e
  | AName (Either ref v) [v] e
  | ALit (Lit ref)
  | ABLit (Lit ref) -- direct boxed literal
  | AMatch v (Branched ref e)
  | AShift ref e
  | AHnd [ref] v (Maybe v) e
  | AApp (Func ref v) [v]
  | AFrc v
  | AVar v
  | -- Affine handler support
    ADiscard v
  | ALocal v e
  | -- Boolean indicates whether there are indirect calls afterward
    AUpdate Bool v v
  deriving (Show, Eq, Functor, Foldable, Traversable)

instance Bifunctor (ANormalF ref) where
  bimap f _ (AVar v) = AVar (f v)
  bimap _ _ (ALit l) = ALit l
  bimap _ _ (ABLit l) = ABLit l
  bimap _ g (ALet d m bn bo) = ALet d m (g bn) (g bo)
  bimap f g (AName n as bo) = AName (f <$> n) (f <$> as) $ g bo
  bimap f g (AMatch v br) = AMatch (f v) $ fmap g br
  bimap f g (AHnd rs nh ah e) = AHnd rs (f nh) (fmap f ah) $ g e
  bimap _ g (AShift i e) = AShift i $ g e
  bimap f _ (AFrc v) = AFrc (f v)
  bimap f _ (AApp fu args) = AApp (fmap f fu) $ fmap f args
  bimap f _ (ADiscard v) = ADiscard (f v)
  bimap f g (ALocal v bo) = ALocal (f v) (g bo)
  bimap f _ (AUpdate b r v) = AUpdate b (f r) (f v)

instance Bifoldable (ANormalF ref) where
  bifoldMap f _ (AVar v) = f v
  bifoldMap _ _ (ALit _) = mempty
  bifoldMap _ _ (ABLit _) = mempty
  bifoldMap _ g (ALet _ _ b e) = g b <> g e
  bifoldMap f g (AName n as e) = foldMap f n <> foldMap f as <> g e
  bifoldMap f g (AMatch v br) = f v <> foldMap g br
  bifoldMap f g (AHnd _ nh ah e) = f nh <> foldMap f ah <> g e
  bifoldMap _ g (AShift _ e) = g e
  bifoldMap f _ (AFrc v) = f v
  bifoldMap f _ (AApp func args) = foldMap f func <> foldMap f args
  bifoldMap f _ (ADiscard v) = f v
  bifoldMap f g (ALocal v bo) = f v <> g bo
  bifoldMap f _ (AUpdate _ r v) = f r <> f v

instance (Ord ref) => ABTN.Align (ANormalF ref) where
  align f _ (AVar u) (AVar v) = Just $ AVar <$> f u v
  align _ _ (ALit l) (ALit r)
    | l == r = Just $ pure (ALit l)
  align _ _ (ABLit l) (ABLit r)
    | l == r = Just $ pure (ABLit l)
  align _ g (ALet dl ccl bl el) (ALet dr ccr br er)
    | dl == dr,
      ccl == ccr =
        Just $ ALet dl ccl <$> g bl br <*> g el er
  align f g (AName hl asl el) (AName hr asr er)
    | length asl == length asr,
      Just hs <- alignEither f hl hr =
        Just $
          AName
            <$> hs
            <*> traverse (uncurry f) (zip asl asr)
            <*> g el er
  align f g (AMatch vl bsl) (AMatch vr bsr)
    | Just bss <- alignBranch g bsl bsr =
        Just $ AMatch <$> f vl vr <*> bss
  align f g (AHnd rl nhl ahl bl) (AHnd rr nhr ahr br)
    | rl == rr,
      Just ah <- alignMaybe f ahl ahr =
        Just $ AHnd rl <$> f nhl nhr <*> ah <*> g bl br
  align _ g (AShift rl bl) (AShift rr br)
    | rl == rr = Just $ AShift rl <$> g bl br
  align f _ (AFrc u) (AFrc v) = Just $ AFrc <$> f u v
  align f _ (AApp hl asl) (AApp hr asr)
    | Just hs <- alignFunc f hl hr,
      length asl == length asr =
        Just $ AApp <$> hs <*> traverse (uncurry f) (zip asl asr)
  align f _ (ADiscard u) (ADiscard v) = Just $ ADiscard <$> f u v
  align f g (ALocal u bl) (ALocal v br) =
    Just $ ALocal <$> f u v <*> g bl br
  align f _ (AUpdate b r u) (AUpdate c s v)
    | b == c = Just $ AUpdate b <$> f r s <*> f u v
  align _ _ _ _ = Nothing

alignEither ::
  (Eq ref, Applicative f) =>
  (l -> r -> f s) ->
  Either ref l ->
  Either ref r ->
  Maybe (f (Either ref s))
alignEither _ (Left rl) (Left rr) | rl == rr = Just . pure $ Left rl
alignEither f (Right u) (Right v) = Just $ Right <$> f u v
alignEither _ _ _ = Nothing

alignMaybe ::
  (Applicative f) =>
  (l -> r -> f s) ->
  Maybe l ->
  Maybe r ->
  Maybe (f (Maybe s))
alignMaybe f (Just l) (Just r) = Just $ Just <$> f l r
alignMaybe _ Nothing Nothing = Just (pure Nothing)
alignMaybe _ _ _ = Nothing

alignFunc ::
  (Eq ref, Applicative f) =>
  (vl -> vr -> f vs) ->
  Func ref vl ->
  Func ref vr ->
  Maybe (f (Func ref vs))
alignFunc f (FVar u) (FVar v) = Just $ FVar <$> f u v
alignFunc _ (FComb rl) (FComb rr) | rl == rr = Just . pure $ FComb rl
alignFunc f (FCont u) (FCont v) = Just $ FCont <$> f u v
alignFunc _ (FCon rl tl) (FCon rr tr)
  | rl == rr, tl == tr = Just . pure $ FCon rl tl
alignFunc _ (FReq rl tl) (FReq rr tr)
  | rl == rr, tl == tr = Just . pure $ FReq rl tl
alignFunc _ (FPrim ol) (FPrim or)
  | ol == or = Just . pure $ FPrim ol
alignFunc _ _ _ = Nothing

alignBranch ::
  (Ord ref, Applicative f) =>
  (el -> er -> f es) ->
  Branched ref el ->
  Branched ref er ->
  Maybe (f (Branched ref es))
alignBranch _ MatchEmpty MatchEmpty = Just $ pure MatchEmpty
alignBranch f (MatchIntegral bl dl) (MatchIntegral br dr)
  | keysSet bl == keysSet br,
    Just ds <- alignMaybe f dl dr =
      Just $
        MatchIntegral
          <$> interverse f bl br
          <*> ds
alignBranch f (MatchText bl dl) (MatchText br dr)
  | Map.keysSet bl == Map.keysSet br,
    Just ds <- alignMaybe f dl dr =
      Just $
        MatchText
          <$> traverse id (Map.intersectionWith f bl br)
          <*> ds
alignBranch f (MatchRequest bl pl) (MatchRequest br pr)
  | Just bs <- alignAscList h bl br =
      Just $ MatchRequest <$> bs <*> f pl pr
  where
    h csl csr
      | keysSet csl == keysSet csr,
        all q (keys csl) =
          Just $ interverse (alignCCs f) csl csr
      | otherwise = Nothing
      where
        q t = fst (csl ! t) == fst (csr ! t)
alignBranch f (MatchData rfl bl dl) (MatchData rfr br dr)
  | rfl == rfr,
    keysSet bl == keysSet br,
    all (\t -> fst (bl ! t) == fst (br ! t)) (keys bl),
    Just ds <- alignMaybe f dl dr =
      Just $ MatchData rfl <$> interverse (alignCCs f) bl br <*> ds
alignBranch f (MatchSum bl) (MatchSum br)
  | keysSet bl == keysSet br,
    all (\w -> fst (bl ! w) == fst (br ! w)) (keys bl) =
      Just $ MatchSum <$> interverse (alignCCs f) bl br
alignBranch f (MatchNumeric rl bl dl) (MatchNumeric rr br dr)
  | rl == rr,
    keysSet bl == keysSet br,
    Just ds <- alignMaybe f dl dr =
      Just $
        MatchNumeric rl
          <$> interverse f bl br
          <*> ds
alignBranch _ _ _ = Nothing

alignAscList ::
  (Applicative f, Ord k) =>
  (a -> b -> Maybe (f c)) ->
  [(k, a)] ->
  [(k, b)] ->
  Maybe (f [(k, c)])
alignAscList f ls0 rs0
  | ll /= lr = Nothing
  | otherwise = getCompose $ zipped ls rs
  where
    (ll, ls) = case prep 0 ls0 of
      Left n -> (n, sortBy (comparing fst) ls0)
      Right n -> (n, ls0)

    (lr, rs) = case prep 0 rs0 of
      Left n -> (n, sortBy (comparing fst) rs0)
      Right n -> (n, rs0)

    prep !n ((k0, _) : xs@((k1, _) : _))
      | k0 <= k1 = prep (n + 1) xs
    prep n [_] = Right (n + 1)
    prep n [] = Right n
    prep n xs = Left (n + length xs)

    zipped [] [] = Compose . Just $ pure []
    zipped ((lk, lv) : lkvs) ((rk, rv) : rkvs)
      | lk == rk = (:) . (lk,) <$> Compose (f lv rv) <*> zipped lkvs rkvs
    zipped _ _ = Compose Nothing

alignCCs :: (Functor f) => (l -> r -> f s) -> (a, l) -> (a, r) -> f (a, s)
alignCCs f (ccs, l) (_, r) = (,) ccs <$> f l r

matchLit :: Term v a -> Maybe (Lit Reference)
matchLit (Int' i) = Just $ I i
matchLit (Nat' n) = Just $ N n
matchLit (Float' f) = Just $ F f
matchLit (Text' t) = Just $ T (Util.Text.fromText t)
matchLit (Char' c) = Just $ C c
matchLit _ = Nothing

pattern TLet ::
  (ABT.Var v) =>
  Direction Word16 ->
  v ->
  Mem ->
  ANormal ref v ->
  ANormal ref v ->
  ANormal ref v
pattern TLet d v m bn bo = ABTN.TTm (ALet d [m] bn (ABTN.TAbs v bo))

pattern TLetD ::
  (ABT.Var v) =>
  v ->
  Mem ->
  ANormal ref v ->
  ANormal ref v ->
  ANormal ref v
pattern TLetD v m bn bo = ABTN.TTm (ALet Direct [m] bn (ABTN.TAbs v bo))

pattern TLets ::
  (ABT.Var v) =>
  Direction Word16 ->
  [v] ->
  [Mem] ->
  ANormal ref v ->
  ANormal ref v ->
  ANormal ref v
pattern TLets d vs ms bn bo = ABTN.TTm (ALet d ms bn (ABTN.TAbss vs bo))

pattern TName ::
  (ABT.Var v) =>
  v ->
  Either ref v ->
  [v] ->
  ANormal ref v ->
  ANormal ref v
pattern TName v f as bo = ABTN.TTm (AName f as (ABTN.TAbs v bo))

pattern Lit' :: Lit Reference -> Term v a
pattern Lit' l <- (matchLit -> Just l)

pattern TLit ::
  (ABT.Var v) =>
  Lit ref ->
  ANormal ref v
pattern TLit l = ABTN.TTm (ALit l)

pattern TBLit ::
  (ABT.Var v) =>
  Lit ref ->
  ANormal ref v
pattern TBLit l = ABTN.TTm (ABLit l)

pattern TApp ::
  (ABT.Var v) =>
  Func ref v ->
  [v] ->
  ANormal ref v
pattern TApp f args = ABTN.TTm (AApp f args)

pattern AApv :: v -> [v] -> ANormalF ref v e
pattern AApv v args = AApp (FVar v) args

pattern TApv ::
  (ABT.Var v) =>
  v ->
  [v] ->
  ANormal ref v
pattern TApv v args = TApp (FVar v) args

pattern ACom :: ref -> [v] -> ANormalF ref v e
pattern ACom r args = AApp (FComb r) args

pattern TCom ::
  (ABT.Var v) =>
  ref ->
  [v] ->
  ANormal ref v
pattern TCom r args = TApp (FComb r) args

pattern ACon :: ref -> CTag -> [v] -> ANormalF ref v e
pattern ACon r t args = AApp (FCon r t) args

pattern TCon ::
  (ABT.Var v) =>
  ref ->
  CTag ->
  [v] ->
  ANormal ref v
pattern TCon r t args = TApp (FCon r t) args

pattern AKon :: v -> [v] -> ANormalF ref v e
pattern AKon v args = AApp (FCont v) args

pattern TKon ::
  (ABT.Var v) =>
  v ->
  [v] ->
  ANormal ref v
pattern TKon v args = TApp (FCont v) args

pattern AReq :: ref -> CTag -> [v] -> ANormalF ref v e
pattern AReq r t args = AApp (FReq r t) args

pattern TReq ::
  (ABT.Var v) =>
  ref ->
  CTag ->
  [v] ->
  ANormal ref v
pattern TReq r t args = TApp (FReq r t) args

pattern APrm :: POp -> [v] -> ANormalF ref v e
pattern APrm p args = AApp (FPrim (Left p)) args

pattern TPrm ::
  (ABT.Var v) =>
  POp ->
  [v] ->
  ANormal ref v
pattern TPrm p args = TApp (FPrim (Left p)) args

pattern AFOp :: ForeignFunc -> [v] -> ANormalF ref v e
pattern AFOp p args = AApp (FPrim (Right p)) args

pattern TFOp ::
  (ABT.Var v) =>
  ForeignFunc ->
  [v] ->
  ANormal ref v
pattern TFOp p args = TApp (FPrim (Right p)) args

pattern THnd ::
  (ABT.Var v) =>
  [ref] ->
  v ->
  Maybe v ->
  ANormal ref v ->
  ANormal ref v
pattern THnd rs nh ah b = ABTN.TTm (AHnd rs nh ah b)

pattern TShift ::
  (ABT.Var v) =>
  ref ->
  v ->
  ANormal ref v ->
  ANormal ref v
pattern TShift i v e = ABTN.TTm (AShift i (ABTN.TAbs v e))

pattern TMatch ::
  (ABT.Var v) =>
  v ->
  Branched ref (ANormal ref v) ->
  ANormal ref v
pattern TMatch v cs = ABTN.TTm (AMatch v cs)

pattern TFrc :: (ABT.Var v) => v -> ANormal ref v
pattern TFrc v = ABTN.TTm (AFrc v)

pattern TVar :: (ABT.Var v) => v -> ANormal ref v
pattern TVar v = ABTN.TTm (AVar v)

pattern TDiscard :: (ABT.Var v) => v -> ANormal ref v
pattern TDiscard v = ABTN.TTm (ADiscard v)

pattern TLocal ::
  (ABT.Var v) => v -> ANormal ref v -> ANormal ref v
pattern TLocal v e = ABTN.TTm (ALocal v e)

pattern TUpdate ::
  (ABT.Var v) => Bool -> v -> v -> ABTN.Term (ANormalF ref) v
pattern TUpdate ind u v = ABTN.TTm (AUpdate ind u v)

{-# COMPLETE
  TLets,
  TName,
  TVar,
  TApp,
  TFrc,
  TLit,
  TBLit,
  THnd,
  TShift,
  TMatch,
  TDiscard,
  TLocal,
  TUpdate,
  ABTN.TAbs
  #-}

{-# COMPLETE
  TLets,
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
  TBLit,
  THnd,
  TShift,
  TMatch,
  TDiscard,
  TLocal,
  TUpdate,
  ABTN.TAbs
  #-}

bind :: (Var v) => Cte v -> ANormal Reference v -> ANormal Reference v
bind (ST d us ms bu) = TLets d us ms bu
bind (LZ u f as) = TName u f as

unbind ::
  (Var v) => ANormal Reference v -> Maybe (Cte v, ANormal Reference v)
unbind (TLets d us ms bu bd) = Just (ST d us ms bu, bd)
unbind (TName u f as bd) = Just (LZ u f as, bd)
unbind _ = Nothing

unbinds ::
  (Var v) => ANormal Reference v -> ([Cte v], ANormal Reference v)
unbinds (TLets d us ms bu (unbinds -> (ctx, bd))) =
  (ST d us ms bu : ctx, bd)
unbinds (TName u f as (unbinds -> (ctx, bd))) = (LZ u f as : ctx, bd)
unbinds tm = ([], tm)

pattern TBind ::
  (Var v) =>
  Cte v ->
  ANormal Reference v ->
  ANormal Reference v
pattern TBind bn bd <-
  (unbind -> Just (bn, bd))
  where
    TBind bn bd = bind bn bd

pattern TBinds ::
  (Var v) => [Cte v] -> ANormal Reference v -> ANormal Reference v
pattern TBinds ctx bd <-
  (unbinds -> (ctx, bd))
  where
    TBinds ctx bd = foldr bind bd ctx

{-# COMPLETE TBinds #-}

data SeqEnd = SLeft | SRight
  deriving (Eq, Ord, Enum, Show)

-- Note: MatchNumeric is a new form for matching directly on boxed
-- numeric data. This leaves MatchIntegral around so that builtins can
-- continue to use it. But interchanged code can be free of unboxed
-- details.
data Branched ref e
  = MatchIntegral (EnumMap Word64 e) (Maybe e)
  | MatchText (Map.Map Util.Text.Text e) (Maybe e)
  | MatchRequest [(ref, (EnumMap CTag ([Mem], e)))] e
  | MatchEmpty
  | MatchData ref (EnumMap CTag ([Mem], e)) (Maybe e)
  | MatchSum (EnumMap Word64 ([Mem], e))
  | MatchNumeric ref (EnumMap Word64 e) (Maybe e)
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- Data cases expected to cover all constructors
pattern MatchDataCover ::
  ref -> EnumMap CTag ([Mem], e) -> Branched ref e
pattern MatchDataCover r m = MatchData r m Nothing

data BranchAccum v
  = AccumEmpty
  | AccumIntegral
      Reference
      (Maybe (ANormal Reference v))
      (EnumMap Word64 (ANormal Reference v))
  | AccumText
      (Maybe (ANormal Reference v))
      (Map.Map Util.Text.Text (ANormal Reference v))
  | AccumDefault (ANormal Reference v)
  | AccumPure (ANormal Reference v)
  | AccumRequest
      (Map Reference (EnumMap CTag ([Mem], ANormal Reference v)))
      (Maybe (ANormal Reference v))
  | AccumData
      Reference
      (Maybe (ANormal Reference v))
      (EnumMap CTag ([Mem], ANormal Reference v))
  | AccumSeqEmpty (ANormal Reference v)
  | AccumSeqView
      SeqEnd
      (Maybe (ANormal Reference v)) -- empty
      (ANormal Reference v) -- cons/snoc
  | AccumSeqSplit
      SeqEnd
      Int -- split at
      (Maybe (ANormal Reference v)) -- default
      (ANormal Reference v) -- split

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
        internalBug [] "AccumSeqView: trying to merge views of opposite ends"
    | otherwise = AccumSeqView el (eml <|> emr) cnl
  AccumSeqView _ _ _ <> AccumDefault _ =
    internalBug [] "seq views may not have defaults"
  AccumDefault _ <> AccumSeqView _ _ _ =
    internalBug [] "seq views may not have defaults"
  AccumSeqSplit el nl dl bl <> AccumSeqSplit er nr dr _
    | el /= er =
        internalBug [] "AccumSeqSplit: trying to merge splits at opposite ends"
    | nl /= nr =
        internalBug [] "AccumSeqSplit: trying to merge splits at different positions"
    | otherwise =
        AccumSeqSplit el nl (dl <|> dr) bl
  AccumDefault dl <> AccumSeqSplit er nr _ br =
    AccumSeqSplit er nr (Just dl) br
  AccumSeqSplit el nl dl bl <> AccumDefault dr =
    AccumSeqSplit el nl (dl <|> Just dr) bl
  _ <> _ = internalBug [] "cannot merge data cases for different types"

instance Monoid (BranchAccum e) where
  mempty = AccumEmpty

data Func ref v
  = -- variable
    FVar v
  | -- top-level combinator
    FComb !ref
  | -- continuation jump
    FCont v
  | -- data constructor
    FCon !ref !CTag
  | -- ability request
    FReq !ref !CTag
  | -- prim op
    FPrim (Either POp ForeignFunc)
  deriving (Show, Eq, Functor, Foldable, Traversable)

data Lit ref
  = I Int64
  | N Word64
  | F Double
  | T Util.Text.Text
  | C Char
  | LM (Rfn.Referent' ref) -- Term Link
  | LY ref -- Type Link
  deriving (Show, Eq)

litRef :: Lit ref -> Reference
litRef (I _) = Ty.intRef
litRef (N _) = Ty.natRef
litRef (F _) = Ty.floatRef
litRef (T _) = Ty.textRef
litRef (C _) = Ty.charRef
litRef (LM _) = Ty.termLinkRef
litRef (LY _) = Ty.typeLinkRef

type ANormal ref = ABTN.Term (ANormalF ref)

type Cte v = CTE v (ANormal Reference v)

type Ctx v = Directed () [Cte v]

data Direction a = Indirect a | Direct
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

directed :: (Foldable f) => f (Cte v) -> Directed () (f (Cte v))
directed x = (foldMap f x, x)
  where
    f (ST d _ _ _) = () <$ d
    f _ = Direct

instance (Semigroup a) => Semigroup (Direction a) where
  Indirect l <> Indirect r = Indirect $ l <> r
  Direct <> r = r
  l <> Direct = l

instance (Semigroup a) => Monoid (Direction a) where
  mempty = Direct

type Directed a = (,) (Direction a)

type DNormal v = Directed () (ANormal Reference v)

-- Should be a completely closed term
data SuperNormal ref v = Lambda {conventions :: [Mem], bound :: ANormal ref v}
  deriving (Show, Eq)

data SuperGroup ref v = Rec
  { group :: [(v, SuperNormal ref v)],
    entry :: SuperNormal ref v
  }
  deriving (Show)

-- | Whether the evaluation of a given definition is cacheable or not.
-- i.e. it's a top-level pure value.
data Cacheability = Cacheable | Uncacheable
  deriving stock (Eq, Show)

instance (Ord ref, Var v) => Eq (SuperGroup ref v) where
  g0 == g1 | Left _ <- equivocate g0 g1 = False | otherwise = True

-- Failure modes for SuperGroup alpha equivalence test
data SGEqv ref v
  = -- mismatch number of definitions in group
    NumDefns (SuperGroup ref v) (SuperGroup ref v)
  | -- mismatched SuperNormal calling conventions
    DefnConventions (SuperNormal ref v) (SuperNormal ref v)
  | -- mismatched subterms in corresponding definition
    Subterms (ANormal ref v) (ANormal ref v)

-- Yields the number of arguments directly accepted by a combinator.
arity :: SuperNormal ref v -> Int
arity (Lambda ccs _) = length ccs

-- Yields the numbers of arguments directly accepted by the
-- combinators in a group. The main entry is the first element, and
-- local bindings follow in their original order.
arities :: SuperGroup ref v -> [Int]
arities (Rec bs e) = arity e : fmap (arity . snd) bs

-- Checks if two SuperGroups are equivalent up to renaming. The rest
-- of the structure must match on the nose. If the two groups are not
-- equivalent, an example of conflicting structure is returned.
equivocate ::
  (Ord ref, Var v) =>
  SuperGroup ref v ->
  SuperGroup ref v ->
  Either (SGEqv ref v) ()
equivocate g0@(Rec bs0 e0) g1@(Rec bs1 e1)
  | length bs0 == length bs1 =
      traverse_ eqvSN (zip ns0 ns1) *> eqvSN (e0, e1)
  | otherwise = Left $ NumDefns g0 g1
  where
    (vs0, ns0) = unzip bs0
    (vs1, ns1) = unzip bs1
    vm = Map.fromList (zip vs1 vs0)

    promote (Left (l, r)) = Left $ Subterms l r
    promote (Right v) = Right v

    eqvSN (Lambda ccs0 e0, Lambda ccs1 e1)
      | ccs0 == ccs1 = promote $ ABTN.alpha vm e0 e1
    eqvSN (n0, n1) = Left $ DefnConventions n0 n1

type ANFM v =
  ReaderT
    (Set v)
    (State (Word64, Word16, [(v, SuperNormal Reference v)]))

type ANFD v = Compose (ANFM v) (Directed ())

data GroupRef ref = GR ref Word64
  deriving (Functor, Foldable, Traversable, Show, Eq)

-- | A list of either unboxed or boxed values.
-- Each slot is one of unboxed or boxed but not both.
type ValList ref = [Value ref]

data Value ref
  = Partial (GroupRef ref) (ValList ref)
  | Data ref Word64 (ValList ref)
  | Cont (ValList ref) (Cont ref)
  | BLit (BLit ref)
  deriving (Show, Eq)

-- Since we can now track cacheability of supergroups, this type
-- pairs the two together. This is the type that should be used
-- as the representation of unison Code values rather than the
-- previous `SuperGroup Symbol`.
data Code ref = CodeRep (SuperGroup ref Symbol) Cacheability
  deriving (Show)

codeGroup :: Code ref -> SuperGroup ref Symbol
codeGroup (CodeRep sg _) = sg

instance (Ord ref) => Eq (Code ref) where
  CodeRep sg1 _ == CodeRep sg2 _ = sg1 == sg2

overGroup ::
  (SuperGroup ref0 Symbol -> SuperGroup ref1 Symbol) ->
  Code ref0 ->
  Code ref1
overGroup f (CodeRep sg ch) = CodeRep (f sg) ch

foldGroup ::
  (Monoid m) => (SuperGroup ref Symbol -> m) -> Code ref -> m
foldGroup f (CodeRep sg _) = f sg

traverseGroup ::
  (Applicative f) =>
  (SuperGroup ref0 Symbol -> f (SuperGroup ref1 Symbol)) ->
  Code ref0 ->
  f (Code ref1)
traverseGroup f (CodeRep sg ch) = flip CodeRep ch <$> f sg

instance Referential Code where
  overRefs f (CodeRep sg ch) = CodeRep (overGroupLinks f sg) ch
  foldMapRefs f (CodeRep sg _) = foldGroupLinks f sg
  traverseRefs f (CodeRep sg ch) =
    flip CodeRep ch <$> traverseGroupLinks f sg

data Cont ref
  = KE
  | Mark
      Word64 -- pending args
      [ref]
      [(ref, Value ref)]
      (Cont ref)
  | Push
      Word64 -- Frame size
      Word64 -- Pending args
      (GroupRef ref)
      (Cont ref)
  deriving (Show, Eq)

data BLit ref
  = Text Util.Text.Text
  | List (Seq (Value ref))
  | TmLink (Rfn.Referent' ref)
  | TyLink ref
  | Bytes Bytes
  | Quote (Value ref)
  | Code (Code ref)
  | BArr PA.ByteArray
  | Arr (PA.Array (Value ref))
  | -- Despite the following being in the Boxed Literal type, they all represent unboxed values
    Pos Word64
  | Neg Word64
  | Char Char
  | Float Double
  | -- special cases for newer formats
    Map [(Value ref, Value ref)]
  deriving (Show, Eq)

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
    f ((_, []), dtm) = dtm
    f ((_, cx), (_, tm)) = (Indirect (), TBinds cx tm)

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
        (ctx <> pure [ST1 Direct v BX t], pure $ TVar v)
    (ctx, (_, t@(TLit l))) ->
      fresh <&> \v ->
        (ctx <> pure [ST1 Direct v cc t], pure $ TVar v)
      where
        cc = case l of T {} -> BX; LM {} -> BX; LY {} -> BX; _ -> UN
    p -> pure p

pattern UFalse <- TCon ((== Ty.booleanRef) -> True) 0 []
  where
    UFalse = TCon Ty.booleanRef 0 []

pattern UTrue <- TCon ((== Ty.booleanRef) -> True) 1 []
  where
    UTrue = TCon Ty.booleanRef 1 []

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
renameCtx v u (d, ctx) | (ctx, b) <- renameCtes v u ctx = ((d, ctx), b)

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
freeVarsCtx = freeVarsCte . snd

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
  case go lavoid Map.empty [] $ reverse ctx of
    (rn, ctx) -> (rn, (d, ctx))
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
            (_, []) -> do
              d0 <- Indirect <$> binder
              pure (sctx <> pure [ST1 d0 u BX (TFrc v)] <> bx, pure bd)
            (d0, [ST1 d1 _ BX tm]) ->
              pure (sctx <> (d0, [ST1 d1 u BX tm]) <> bx, pure bd)
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
            | (d, [ST1 _ _ BX tm]) <- cx = (d, tm)
            | (_, [ST _ _ _ _]) <- cx =
                internalBug [] "anfBlock: impossible"
            | otherwise = (Indirect (), TFrc v)
      pure
        ( sctx <> pure [LZ hv (Right r) vs],
          (d, THnd (Map.keys abr) hv Nothing msc)
        )
    AccumText df cs ->
      pure (sctx <> cx, pure . TMatch v $ MatchText cs df)
    AccumIntegral r df cs ->
      pure (sctx <> cx, pure $ TMatch v $ MatchNumeric r cs df)
    AccumData r df cs ->
      pure (sctx <> cx, pure . TMatch v $ MatchData r cs df)
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
anfBlock (Lit' l) =
  pure (mempty, pure $ TBLit l)
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
  | P.Constructor _ (ConstructorReference r t) ps <- p = do
      (,)
        <$> expandBindings ps vs
        <*> anfBody bd
        <&> \(us, bd) ->
          AccumData r Nothing . EC.mapSingleton (fromIntegral t) . (BX <$ us,) $ ABTN.TAbss us bd
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

valueTermLinks :: (Ord ref) => Value ref -> [ref]
valueTermLinks = Set.toList . valueLinks f
  where
    f False r = Set.singleton r
    f _ _ = Set.empty

collectValueLinks :: (Ord ref) => Value ref -> (Set ref, Set ref)
collectValueLinks = valueLinks f
  where
    f False r = (mempty, Set.singleton r)
    f True r = (Set.singleton r, mempty)

-- Folds over the references necessary to _load_ a `Value`. This does
-- not include references in quoted code or values, or literal
-- term/type links.
valueLinks :: (Monoid a) => (Bool -> ref -> a) -> Value ref -> a
valueLinks f = go
  where
    go (Partial (GR cr _) vs) = f False cr <> foldMap go vs
    go (Data dr _ vs) = f True dr <> foldMap go vs
    go (Cont vs k) = foldMap go vs <> contLinks f k
    go (BLit l) = blitLinks f l
{-# INLINE valueLinks #-}

-- Traversals of _all_ references in a `Value`, for e.g.
-- canonicalization.
instance Referential Value where
  overRefs h = \case
    Partial gr vs ->
      Partial (h False <$> gr) (fmap (overRefs h) vs)
    Data r t vs ->
      Data (h True r) t (fmap (overRefs h) vs)
    Cont vs k ->
      Cont (fmap (overRefs h) vs) (overRefs h k)
    BLit l -> BLit (overRefs h l)

  foldMapRefs h = \case
    Partial (GR r _) vs -> h False r <> foldMap (foldMapRefs h) vs
    Data r _ vs -> h True r <> foldMap (foldMapRefs h) vs
    Cont vs k -> foldMap (foldMapRefs h) vs <> foldMapRefs h k
    BLit l -> foldMapRefs h l

  traverseRefs h = \case
    Partial gr vs ->
      Partial
        <$> traverse (h False) gr
        <*> traverse (traverseRefs h) vs
    Data r t vs ->
      flip Data t
        <$> h True r
        <*> traverse (traverseRefs h) vs
    Cont vs k ->
      Cont
        <$> traverse (traverseRefs h) vs
        <*> traverseRefs h k
    BLit l -> BLit <$> traverseRefs h l

contLinks :: (Monoid a) => (Bool -> ref -> a) -> Cont ref -> a
contLinks f = go
  where
    go (Push _ _ (GR cr _) k) =
      f False cr <> go k
    go (Mark _ ps de k) =
      foldMap (f True) ps
        <> foldMap (\(k, c) -> f True k <> valueLinks f c) de
        <> go k
    go KE = mempty
{-# INLINE contLinks #-}

-- Traversals over references in a cont.
--
-- This traverses _all_ references in the continuation, not just the
-- ones necessary to load it.
instance Referential Cont where
  overRefs h = \case
    KE -> KE
    Mark asz rs env k ->
      Mark
        asz
        (fmap (h True) rs)
        (fmap (bimap (h True) (overRefs h)) env)
        (overRefs h k)
    Push fsz asz gr k ->
      Push fsz asz (h False <$> gr) (overRefs h k)

  foldMapRefs h = \case
    KE -> mempty
    Push _ _ (GR r _) k -> h False r <> foldMapRefs h k
    Mark _ rs env k ->
      foldMap (h True) rs
        <> foldMap (bifoldMap (h True) (foldMapRefs h)) env
        <> foldMapRefs h k

  traverseRefs h = \case
    KE -> pure KE
    Mark asz rs env k ->
      Mark asz
        <$> traverse (h True) rs
        <*> traverse (bitraverse (h True) (traverseRefs h)) env
        <*> traverseRefs h k
    Push fsz asz gr k ->
      Push fsz asz
        <$> traverse (h False) gr
        <*> traverseRefs h k

blitLinks :: (Monoid a) => (Bool -> ref -> a) -> BLit ref -> a
blitLinks f = go
  where
    go (List s) = foldMap (valueLinks f) s
    go (Arr a) = foldMap (valueLinks f) a
    go (Map m) =
      foldMap (\(k, v) -> valueLinks f k <> valueLinks f v) m
    go _ = mempty
{-# INLINE blitLinks #-}

instance Referential BLit where
  overRefs h = \case
    List vs -> List (fmap (overRefs h) vs)
    TmLink rn -> TmLink (overRefs h rn)
    TyLink r -> TyLink $ h True r
    Quote v -> Quote $ overRefs h v
    Code co -> Code $ overRefs h co
    Arr a -> Arr $ fmap (overRefs h) a
    Map kvs -> Map $ fmap (bimap (overRefs h) (overRefs h)) kvs
    Text t -> Text t
    Bytes b -> Bytes b
    BArr ba -> BArr ba
    Pos n -> Pos n
    Neg n -> Neg n
    Char c -> Char c
    Float f -> Float f

  foldMapRefs h = \case
    List vs -> foldMap (foldMapRefs h) vs
    TmLink rn -> foldMapRefs h rn
    TyLink r -> h True r
    Quote v -> foldMapRefs h v
    Code co -> foldMapRefs h co
    Arr a -> foldMap (foldMapRefs h) a
    Map kvs -> foldMap (bifoldMap (foldMapRefs h) (foldMapRefs h)) kvs
    _ -> mempty

  traverseRefs h = \case
    List vs -> List <$> traverse (traverseRefs h) vs
    TmLink rn -> TmLink <$> traverseRefs h rn
    TyLink r -> TyLink <$> h True r
    Quote v -> Quote <$> traverseRefs h v
    Code co -> Code <$> traverseRefs h co
    Arr a -> Arr <$> traverse (traverseRefs h) a
    Map kvs ->
      Map
        <$> traverse (bitraverse (traverseRefs h) (traverseRefs h)) kvs
    Text t -> pure $ Text t
    Bytes b -> pure $ Bytes b
    BArr ba -> pure $ BArr ba
    Pos n -> pure $ Pos n
    Neg n -> pure $ Neg n
    Char c -> pure $ Char c
    Float f -> pure $ Float f

groupTermLinks :: (Ord ref, Var v) => SuperGroup ref v -> [ref]
groupTermLinks = Set.toList . foldGroupLinks f
  where
    f False r = Set.singleton r
    f _ _ = Set.empty

overGroupLinks ::
  (Var v) =>
  (Bool -> ref0 -> ref1) ->
  SuperGroup ref0 v ->
  SuperGroup ref1 v
overGroupLinks f =
  runIdentity . traverseGroupLinks (\b -> Identity . f b)

traverseGroupLinks ::
  (Applicative f, Var v) =>
  (Bool -> ref0 -> f ref1) ->
  SuperGroup ref0 v ->
  f (SuperGroup ref1 v)
traverseGroupLinks f (Rec bs e) =
  Rec <$> (traverse . traverse) (normalLinks f) bs <*> normalLinks f e

foldGroupLinks ::
  (Monoid r, Var v) =>
  (Bool -> ref -> r) ->
  SuperGroup ref v ->
  r
foldGroupLinks f = getConst . traverseGroupLinks (\b -> Const . f b)

normalLinks ::
  (Applicative f, Var v) =>
  (Bool -> ref0 -> f ref1) ->
  SuperNormal ref0 v ->
  f (SuperNormal ref1 v)
normalLinks f (Lambda ccs e) = Lambda ccs <$> anfLinks f e

anfLinks ::
  (Applicative f, Var v) =>
  (Bool -> ref0 -> f ref1) ->
  ANormal ref0 v ->
  f (ANormal ref1 v)
anfLinks f (ABTN.Term _ (ABTN.Abs v e)) =
  ABTN.TAbs v <$> anfLinks f e
anfLinks f (ABTN.Term _ (ABTN.Tm e)) =
  ABTN.TTm <$> anfFLinks f (anfLinks f) e

anfFLinks ::
  (Applicative f) =>
  (Bool -> ref0 -> f ref1) ->
  (e0 -> f e1) ->
  ANormalF ref0 v e0 ->
  f (ANormalF ref1 v e1)
anfFLinks _ g (ALet d ccs b e) = ALet d ccs <$> g b <*> g e
anfFLinks f g (AName er vs e) =
  flip AName vs <$> bitraverse (f False) pure er <*> g e
anfFLinks f g (AMatch v bs) =
  AMatch v <$> branchLinks (f True) g bs
anfFLinks f g (AShift r e) =
  AShift <$> f True r <*> g e
anfFLinks f g (AHnd rs nh ah e) =
  (\rs -> AHnd rs nh ah) <$> traverse (f True) rs <*> g e
anfFLinks f _ (AApp fu vs) = flip AApp vs <$> funcLinks f fu
anfFLinks f _ (ALit l) = ALit <$> litLinks f l
anfFLinks f _ (ABLit l) = ABLit <$> litLinks f l
anfFLinks _ _ (AFrc v) = pure $ AFrc v
anfFLinks _ _ (AVar v) = pure $ AVar v
anfFLinks _ _ (ADiscard v) = pure $ ADiscard v
anfFLinks _ g (ALocal v e) = ALocal v <$> g e
anfFLinks _ _ (AUpdate b u v) = pure $ AUpdate b u v

litLinks ::
  (Applicative f) =>
  (Bool -> ref0 -> f ref1) ->
  Lit ref0 ->
  f (Lit ref1)
litLinks f (LY r) = LY <$> f True r
litLinks f (LM (Rfn.Con' (ConstructorReference r i) t)) =
  LM . flip Rfn.Con' t . flip ConstructorReference i <$> f True r
litLinks f (LM (Rfn.Ref' r)) = LM . Rfn.Ref' <$> f False r
litLinks _ (I i) = pure $ I i
litLinks _ (N n) = pure $ N n
litLinks _ (F d) = pure $ F d
litLinks _ (T t) = pure $ T t
litLinks _ (C c) = pure $ C c

branchLinks ::
  (Applicative f) =>
  (ref0 -> f ref1) ->
  (e0 -> f e1) ->
  Branched ref0 e0 ->
  f (Branched ref1 e1)
branchLinks f g (MatchRequest m e) =
  MatchRequest <$> traverse h m <*> g e
  where
    h (r, cs) = (,) <$> f r <*> (traverse . traverse) g cs
branchLinks f g (MatchData r m e) =
  MatchData <$> f r <*> (traverse . traverse) g m <*> traverse g e
branchLinks _ g (MatchText m e) =
  MatchText <$> traverse g m <*> traverse g e
branchLinks _ g (MatchIntegral m e) =
  MatchIntegral <$> traverse g m <*> traverse g e
branchLinks f g (MatchNumeric r m e) =
  MatchNumeric <$> f r <*> traverse g m <*> traverse g e
branchLinks _ g (MatchSum m) =
  MatchSum <$> (traverse . traverse) g m
branchLinks _ _ MatchEmpty = pure MatchEmpty

funcLinks ::
  (Applicative f) =>
  (Bool -> ref0 -> f ref1) ->
  Func ref0 v ->
  f (Func ref1 v)
funcLinks f (FComb r) = FComb <$> f False r
funcLinks f (FCon r t) = flip FCon t <$> f True r
funcLinks f (FReq r t) = flip FReq t <$> f True r
funcLinks _ (FVar v) = pure $ FVar v
funcLinks _ (FCont v) = pure $ FCont v
funcLinks _ (FPrim e) = pure $ FPrim e

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

indent :: Int -> ShowS
indent ind = showString (replicate (ind * 2) ' ')

prettyGroup :: (Var v) => String -> SuperGroup Reference v -> ShowS
prettyGroup s (Rec grp ent) =
  showString ("let rec[" ++ s ++ "]\n")
    . foldr f id grp
    . showString "entry"
    . prettySuperNormal 1 ent
  where
    f (v, sn) r =
      indent 1
        . pvar v
        . prettySuperNormal 2 sn
        . showString "\n"
        . r

pvar :: (Var v) => v -> ShowS
pvar v = showString . Data.Text.unpack $ Var.name v

prettyVars :: (Var v) => [v] -> ShowS
prettyVars =
  foldr (\v r -> showString " " . pvar v . r) id

prettyLVars :: (Var v) => [Mem] -> [v] -> ShowS
prettyLVars [] [] = showString " "
prettyLVars (c : cs) (v : vs) =
  showString " "
    . showParen True (pvar v . showString ":" . shows c)
    . prettyLVars cs vs
prettyLVars [] (_ : _) = internalBug [] "more variables than conventions"
prettyLVars (_ : _) [] = internalBug [] "more conventions than variables"

prettyRBind :: (Var v) => [v] -> ShowS
prettyRBind [] = showString "()"
prettyRBind [v] = pvar v
prettyRBind (v : vs) =
  showParen True $
    pvar v . foldr (\v r -> shows v . showString "," . r) id vs

prettySuperNormal ::
  (Var v) => Int -> SuperNormal Reference v -> ShowS
prettySuperNormal ind (Lambda ccs (ABTN.TAbss vs tm)) =
  prettyLVars ccs vs
    . showString "="
    . prettyANF False (ind + 1) tm

reqSpace :: (Var v) => Bool -> ANormal ref v -> Bool
reqSpace _ TLets {} = True
reqSpace _ TName {} = True
reqSpace _ TLocal {} = True
reqSpace b _ = b

prettyANF :: (Var v) => Bool -> Int -> ANormal Reference v -> ShowS
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
    TBLit l -> shows l
    TFrc v -> showString "!" . pvar v
    TVar v -> pvar v
    TApp f vs -> prettyFunc f . prettyVars vs
    TMatch v bs ->
      showString "match "
        . pvar v
        . showString " with"
        . prettyBranches (ind + 1) bs
    TShift r v bo ->
      showString "shift["
        . showsShort r
        . showString "]"
        . prettyVars [v]
        . showString "."
        . prettyANF False (ind + 1) bo
    THnd rs nh ah bo ->
      showString "handle"
        . prettyRefs rs
        . prettyANF False (ind + 1) bo
        . prettySpace True ind
        . showString "with "
        . pvar nh
        . maybe id (\v -> showString " with affine " . pvar v) ah
    TLocal hr bo ->
      showString "in-local "
        . pvar hr
        . prettyANF True (ind + 1) bo
    TDiscard hr ->
      showString "discard[" . pvar hr . showString "]"
    TUpdate _ hr v ->
      showString "update["
        . pvar hr
        . showString ", "
        . pvar v
        . showString "]"
    ABTN.TAbs v (ABTN.TAbss vs bo) ->
      prettyVars (v : vs)
        . showString " ->"
        . prettyANF True (ind + 1) bo

prettySpace :: Bool -> Int -> ShowS
prettySpace False _ = showString " "
prettySpace True ind = showString "\n" . indent ind

prettyLZF :: (Var v) => Either Reference v -> ShowS
prettyLZF (Left w) = showString "ENV(" . showsShort w . showString ") "
prettyLZF (Right v) = pvar v . showString " "

prettyRefs :: [Reference] -> ShowS
prettyRefs [] = showString "{}"
prettyRefs (r : rs) =
  showString "{"
    . showsShort r
    . foldr (\t r -> showString "," . showsShort t . r) id rs
    . showString "}"

prettyFunc :: (Var v) => Func Reference v -> ShowS
prettyFunc (FVar v) = pvar v . showString " "
prettyFunc (FCont v) = pvar v . showString " "
prettyFunc (FComb w) = showString "ENV(" . showsShort w . showString ")"
prettyFunc (FCon r t) =
  showString "CON("
    . showsShort r
    . showString ","
    . shows t
    . showString ")"
prettyFunc (FReq r t) =
  showString "REQ("
    . showsShort r
    . showString ","
    . shows t
    . showString ")"
prettyFunc (FPrim op) = either shows shows op . showString " "

showsShort :: Reference -> ShowS
showsShort =
  showString . Text.unpack . Pretty.toPlain 0 . prettyShortHash . shortenTo 10 . toShortHash

prettyBranches ::
  (Var v) => Int -> Branched Reference (ANormal Reference v) -> ShowS
prettyBranches ind bs = case bs of
  MatchEmpty -> showString "{}"
  MatchIntegral bs df ->
    maybe id (\e -> prettyCase ind (showString "_") e id) df
      . foldr (uncurry $ prettyCase ind . shows) id (mapToList bs)
  MatchText bs df ->
    maybe id (\e -> prettyCase ind (showString "_") e id) df
      . foldr (uncurry $ prettyCase ind . shows) id (Map.toList bs)
  MatchData r bs df ->
    maybe id (\e -> prettyCase ind (showString "_") e id) df
      . foldr
        (uncurry $ prettyCase ind . prettyTag r)
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
      (prettyCase ind (showString "REQ(0,0)") df id)
      bs
  MatchSum bs ->
    foldr
      (uncurry $ prettyCase ind . shows)
      id
      (mapToList $ snd <$> bs)
  MatchNumeric _ bs df ->
    maybe id (\e -> prettyCase ind (showString "_") e id) df
      . foldr (uncurry $ prettyCase ind . shows) id (mapToList bs)
      -- _ -> error "prettyBranches: todo"
  where
    -- prettyReq :: Reference -> CTag -> ShowS
    prettyReq r c =
      showString "REQ("
        . showsShort r
        . showString ","
        . shows c
        . showString ")"

    prettyTag r c =
      showString "CON("
        . showsShort r
        . showString ","
        . shows c
        . showString ")"

prettyCase ::
  (Var v) => Int -> ShowS -> ANormal Reference v -> ShowS -> ShowS
prettyCase ind sc (ABTN.TAbss vs e) r =
  showString "\n"
    . indent ind
    . sc
    . prettyVars vs
    . showString " ->"
    . prettyANF True (ind + 1) e
    . r
