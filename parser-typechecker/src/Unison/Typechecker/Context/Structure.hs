{-# LANGUAGE RecordWildCards #-}

module Unison.Typechecker.Context.Structure
  ( Element (..)
  , variable
  , Info (..)
  , unsolvedExistentials
  , Context (..)
  , context0
  , CtxSegment
  , (|>)
  , (<|)
  , fmap'
  , BadContext (..)
  , validateSegment
  , SearchResult (..)
  , focusProblem
  , culprit
  , info
  , filter
  , partition
  , apply
  , FT.fromList
  , mapMaybe
  , single
  , split
  , splitSeg
  , surround
  ) where

import Data.FingerTree as FT hiding (split)
import Data.Foldable qualified as FO
import Data.Function (on)
import Data.List qualified as List
import Data.Set as Set hiding (split, filter, partition)
import Data.Text qualified as Text
import Data.Map.Strict as Map hiding (split, filter, mapMaybe, partition)
import Prelude hiding (filter)

import Unison.ABT qualified as ABT
import Unison.Blank qualified as B
import Unison.Type qualified as Type
import Unison.Typechecker.TypeVar qualified as TypeVar
import Unison.Var (Var)
import Unison.Var qualified as Var

import Unison.PrettyPrintEnv qualified as PPE
import Unison.Syntax.TypePrinter qualified as TP

type Monotype v loc = Type.Monotype (TypeVar v loc) loc
type TypeVar v loc = TypeVar.TypeVar (B.Blank loc) v
type Type v loc = Type.Type (TypeVar v loc) loc

-- | Elements of an ordered algorithmic context. A context is a sequence
-- of `Element`s (here represented as a finger tree), but not every
-- sequence represents a valid context. A context is only valid if every
-- type that occurs in the context is well scoped with regard to the
-- previous variable bindings in the context.
--
-- We will refer to invalid trees as 'context segments' since they can be
-- prefixed to become part of a valid context. Keeping track of the same
-- context information for _every_ segment is important to avoiding some
-- performance problems with a more naive implementation.
data Element v loc
  = -- | A variable declaration
    Var (TypeVar v loc)
  | -- | `v` is solved to some monotype
    Solved (B.Blank loc) v (Monotype v loc)
  | -- | `v` has type `a`, maybe quantified
    -- loc contains the span of the name of the bound 'v'
    Ann v loc (Type v loc)
  | -- | used for scoping
    Marker v

variable :: Element v loc -> v
variable (Var v) = TypeVar.underlying v
variable (Solved _ v _) = v
variable (Ann v _ _) = v
variable (Marker v) = v

instance (Var v) => Eq (Element v loc) where
  Var v == Var v2 = v == v2
  Solved _ v t == Solved _ v2 t2 = v == v2 && t == t2
  Ann v _loc t == Ann v2 _loc2 t2 = v == v2 && t == t2
  Marker v == Marker v2 = v == v2
  _ == _ = False

-- | Aggregate context (segment) information, built up from `Element`s.
--
-- `boundExistentialVars` is the set of existential variables that are
-- known to be 'in scope' due to the context segment.
--
-- `solvedExistentials` is a mapping of existential variables to their
-- solutions in the segment.
--
-- `boundUniversalVars` is the set of universal variables that are known
-- to be 'in scope' due to the context segment.
--
-- `termVarAnnotations` is a mapping of term variables to their type.
--
-- `allBoundVars` contains all the variables 'in scope' due to the
-- segment. Primarily used for freshening.
--
-- `freeUniversal/ExistentialVars` contains all the variables that occur
-- _free_ in types in the segment. This means that they are _not_ bound
-- in an `Element` previous to the one that the type occurs in.
--
-- `shadowedVars` contains variables that occur _multiple_ times in the
-- segment, and thus the latter shadows the former.
--
-- `recorded` tracks certain annotated elements.
--
-- A context segment is a well formed context if `free*Vars` and
-- `shadowedVars` are both empty.
data Info v loc = Info
  { boundExistentialVars :: !(Set v),
    solvedExistentials :: !(Map v (Monotype v loc)),
    boundUniversalVars :: !(Set v),
    termVarAnnotations :: !(Map v (loc, Type v loc)),
    allBoundVars :: !(Set v),
    freeUniversalVars :: !(Set v),
    freeExistentialVars :: !(Set v),
    shadowedVars :: !(Set v),
    recorded :: Map v (B.Recorded loc, Type v loc)
  }

type CtxSegment v loc = FingerTree (Info v loc) (Element v loc)

newtype Context v loc = Context (CtxSegment v loc)

-- | The empty context
context0 :: Ord v => Context v loc
context0 = Context FT.empty

instance Ord v => Semigroup (Info v loc) where
  segl <> segr =
    Info
      { boundExistentialVars =
          (Set.union `on` boundExistentialVars) segl segr,
        boundUniversalVars =
          (Set.union `on` boundUniversalVars) segl segr,
        allBoundVars =
          (Set.union `on` allBoundVars) segl segr,
        -- Note: prefer solutions/annotations from the _right_
        solvedExistentials =
          (Map.union `on` solvedExistentials) segr segl,
        termVarAnnotations =
          (Map.union `on` termVarAnnotations) segr segl,
        freeExistentialVars =
          Set.union
            (freeExistentialVars segl)
            (Set.difference
              (freeExistentialVars segr)
              (boundExistentialVars segl)),
        freeUniversalVars =
          Set.union
            (freeUniversalVars segl)
            (Set.difference
              (freeUniversalVars segr)
              (boundUniversalVars segl)),
        shadowedVars =
          Set.unions
            [ shadowedVars segl
            , shadowedVars segr
            , Set.intersection
                (allBoundVars segl)
                (allBoundVars segr)
            ],
        recorded = (Map.union `on` recorded) segr segl
      }

emptyInfo :: Info v loc
emptyInfo =
  Info {
    boundExistentialVars = Set.empty,
    solvedExistentials = Map.empty,
    boundUniversalVars = Set.empty,
    termVarAnnotations = Map.empty,
    allBoundVars = Set.empty,
    freeExistentialVars = Set.empty,
    freeUniversalVars = Set.empty,
    shadowedVars = Set.empty,
    recorded = Map.empty
  }

-- | Gets the _unsolved_ existential variables of a context info.
unsolvedExistentials :: (Ord v) => Info v loc -> Set v
unsolvedExistentials (Info {..}) =
  boundExistentialVars `Set.difference` Map.keysSet solvedExistentials

instance Ord v => Monoid (Info v loc) where
  mempty = emptyInfo

part :: Set (TypeVar v loc) -> ([v], [v])
part = List.foldl' classify ([], []) . Set.toDescList
  where
    classify (es, us) (TypeVar.Existential _ e) = (e:es, us)
    classify (es, us) (TypeVar.Universal u) = (es, u:us)

instance Ord v => Measured (Info v loc) (Element v loc) where
  measure (Var tv)
    | TypeVar.Universal v <- tv =
        emptyInfo
          { boundUniversalVars = Set.singleton v
          , allBoundVars = Set.singleton v
          }
    | TypeVar.Existential b v <- tv =
        emptyInfo
          { boundExistentialVars = Set.singleton v
          , allBoundVars = Set.singleton v
          , recorded = case b of
              B.Recorded b' ->
                Map.singleton v (b', ty)
                where
                  ty = ABT.annotatedVar (B.loc b') tv
              _ -> Map.empty
          }

  measure (Solved b v ty)
    | pty <- Type.getPolytype ty,
      (evs, uvs) <- part $ Type.freeVars pty =
        emptyInfo
          { boundExistentialVars = Set.singleton v
          , solvedExistentials = Map.singleton v ty
          , allBoundVars = Set.singleton v
          , freeExistentialVars = Set.fromList evs
          , freeUniversalVars = Set.fromList uvs
          , recorded = case b of
              B.Recorded b' ->
                Map.singleton v (b', pty)
              _ -> Map.empty
          }

  measure (Ann v loc ty)
    | (evs, uvs) <- part $ Type.freeVars ty =
        emptyInfo
          { allBoundVars = Set.singleton v
          , termVarAnnotations = Map.singleton v (loc, ty)
          , freeExistentialVars = Set.fromList evs
          , freeUniversalVars = Set.fromList uvs
          }

  measure (Marker v) = emptyInfo { allBoundVars = Set.singleton v }

instance (Ord v) => Measured (Info v loc) (Context v loc) where
  measure (Context c) = measure c

data BadContext
  = Shadowing
  | UnboundEx
  | UnboundUn

-- | Checks that a context segment forms a valid context. If so, then the
-- proper context is yielded. Otherwise an indication of what is wrong
-- with the segment is returned.
validateSegment ::
  Ord v => CtxSegment v loc -> Either BadContext (Context v loc)
validateSegment seg
  | not $ Set.null shadowedVars = Left Shadowing
  | not $ Set.null freeExistentialVars = Left UnboundEx
  | not $ Set.null freeUniversalVars = Left UnboundUn
  | otherwise = Right $ Context seg
  where
    Info {..} = measure seg

-- Given a predicate on measures that is monotone, searches for a point
-- in the sequence at which the predicate first becomes true.
focusProblem ::
  Measured v e =>
  (v -> Bool) ->
  FingerTree v e -> SearchResult v e
focusProblem p = search q
  where
    q l _ = p l

-- Searches for the first point at which an extracted set becomes
-- non-null.
findFirst ::
  Measured v e =>
  (v -> Set a) ->
  FingerTree v e -> SearchResult v e
findFirst ex = focusProblem (not . Set.null . ex)

-- Given a malformed context and a validation error, finds an element
-- that induces the error, and the preceding context.
culprit ::
  (Ord v) =>
  CtxSegment v loc ->
  BadContext ->
  (CtxSegment v loc, Element v loc)
culprit whole err
  | Position pre e _ <- srch whole = (pre, e)
  | otherwise = error "Context.Structure.culprit: impossible"
  where
    srch = case err of
      Shadowing -> findFirst shadowedVars
      UnboundEx -> findFirst freeExistentialVars
      UnboundUn -> findFirst freeUniversalVars


-- | Return the aggregate `Info` associated to the context.
info :: (Ord v, Measured (Info v loc) c) => c -> Info v loc
info = measure

-- | Replace any existentials with their solution in the context
apply ::
  (Var v, Measured (Info v loc) c) =>
  c ->
  Type v loc ->
  Type v loc
apply ctx = apply' (solvedExistentials . measure $ ctx)

apply' ::
  (Var v) => Map v (Monotype v loc) -> Type v loc -> Type v loc
apply' _ t | Set.null (Type.freeVars t) = t
apply' solved t = go t
  where
    go t = case t of
      Type.Var' (TypeVar.Universal _) -> t
      Type.Ref' _ -> t
      Type.Var' (TypeVar.Existential _ v) ->
        maybe t (\(Type.Monotype t') -> go t') (Map.lookup v solved)
      Type.Arrow' i o -> Type.arrow a (go i) (go o)
      Type.App' x y -> Type.app a (go x) (go y)
      Type.Ann' v k -> Type.ann a (go v) k
      Type.Effect1' e t -> Type.effect1 a (go e) (go t)
      Type.Effects' es -> Type.effects a (fmap go es)
      Type.ForallNamed' v t' -> Type.forAll a v (go t')
      Type.IntroOuterNamed' v t' -> Type.introOuter a v (go t')
      _ -> error $ "Match error in Context.apply': " ++ show t
      where
        a = ABT.annotation t

-- Splits a context segment at an `Element` binding the given variable.
-- In a general context segment, variable bindings needn't be unique, so
-- this finds the right-most such occurrence, which would be the most
-- locally relevant occurrence.
--
-- Technically the context can contain both type and term variable
-- bindings, and these are not distinguished, so take care.
splitSeg ::
  Ord v =>
  v ->
  CtxSegment v loc ->
  Maybe (CtxSegment v loc, Element v loc, CtxSegment v loc)
splitSeg tgt seg = case search p seg of
  Position l e r -> Just (l, e, r)
  _ -> Nothing
  where
    -- Given info for a partition | i | j | of the context, tests whether
    -- `tgt` is bound in `i` and not in `j`. This means | i | ends with
    -- the last binding of `tgt`.
    p i j =
      (tgt `Set.notMember` allBoundVars j) &&
      (tgt `Set.member` allBoundVars i)

-- Splits a context at an `Element` binding the given variable. Since in
-- a valid context, a variable only occurs once, this is completely
-- unambiguous.
split ::
  Ord v =>
  v ->
  Context v loc ->
  Maybe (Context v loc, Element v loc, CtxSegment v loc)
split tgt (Context seg) = g <$> splitSeg tgt seg
  where
    -- Note: every prefix of a valid context is valid.
    g (l, e, r) = (Context l, e, r)

surround ::
  Ord v =>
  CtxSegment v loc ->
  Element v loc ->
  CtxSegment v loc ->
  CtxSegment v loc
surround l e r = l >< e <| r

filter ::
  (Var v) =>
  (Element v loc -> Bool) ->
  CtxSegment v loc -> CtxSegment v loc
filter p = FO.foldl' c mempty
  where
    c seg e
      | p e = seg |> e
      | otherwise = seg

partition ::
  (Var v) =>
  (Element v loc -> Either (Element v loc) (Element v loc)) ->
  CtxSegment v loc ->
  (CtxSegment v loc, CtxSegment v loc)
partition f = FO.foldl' g (mempty, mempty)
  where
    g (ls, rs) e = case f e of
      Left l -> (ls |> l, rs)
      Right r -> (ls, rs |> r)

-- | Applies a partial mapping to a context segment, resulting in a list.
mapMaybe :: (Element v loc -> Maybe b) -> CtxSegment v loc -> [b]
mapMaybe f = Prelude.foldr c []
  where
    c e = maybe id (:) $ f e

single :: (Var v) => Element v loc -> CtxSegment v loc
single e = FT.singleton e

instance (Var v) => Show (Element v loc) where
  show (Var v) = case v of
    TypeVar.Universal x -> "@" <> show x
    e -> show e
  show (Solved _ v t) = "'" ++ Text.unpack (Var.name v) ++ " = " ++ Text.unpack (TP.prettyStr 0 PPE.empty (Type.getPolytype t))
  show (Ann v _loc t) =
    Text.unpack (Var.name v)
      ++ " : "
      ++ Text.unpack (TP.prettyStr 0 PPE.empty t)
  show (Marker v) = "|" ++ Text.unpack (Var.name v) ++ "|"

toReverseList :: CtxSegment v loc -> [Element v loc]
toReverseList = Prelude.foldl (flip (:)) []

renderType :: (Ord loc, Var v) => Context v loc -> Type v loc -> String
renderType ctx ty = Text.unpack . TP.prettyStr 0 PPE.empty $ apply ctx ty

renderElement ::
  (Ord loc, Var v) => Context v loc -> Element v loc -> String
renderElement ctx = \case
  Var v
    | TypeVar.Universal x <- v -> "@" <> show x
    | otherwise -> show v
  Solved _ v (Type.Monotype t) ->
    mconcat
      [ "'"
      , Text.unpack $ Var.name v
      , " = "
      , renderType ctx t
      ]
  Ann v _loc t ->
    mconcat
      [ Text.unpack $ Var.name v
      , " : "
      , renderType ctx t
      ]
  Marker v -> "|" <> Text.unpack (Var.name v) <> "|"

renderElements ::
  (Ord loc, Var v) => Context v loc -> [Element v loc] -> String
renderElements ctx = List.intercalate "\n  " . fmap (renderElement ctx)

instance (Ord loc, Var v) => Show (Context v loc) where
  show ctx@(Context es) =
    "Γ\n  " <> renderElements ctx (toReverseList es)
