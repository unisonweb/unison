module Unison.PatternMatchCoverage.NormalizedConstraints
  ( NormalizedConstraints (..),
    VarInfo (..),
    VarConstraints (..),
    EffectInfo (..),
    markDirty,
    emptyNormalizedConstraints,
    updateF,
    ConstraintUpdate (..),
    expectCanon,
    declVar,
    prettyNormalizedConstraints,
    prettyDnf,
  )
where

import Data.Functor.Compose
import Data.Sequence (pattern Empty)
import qualified Data.Set as Set
import Unison.ConstructorReference (ConstructorReference)
import Unison.PatternMatchCoverage.Constraint
import Unison.PatternMatchCoverage.IntervalSet (IntervalSet)
import qualified Unison.PatternMatchCoverage.IntervalSet as IntervalSet
import qualified Unison.PatternMatchCoverage.PmLit as PmLit
import Unison.PatternMatchCoverage.Pretty
import Unison.PatternMatchCoverage.UFMap (UFMap)
import qualified Unison.PatternMatchCoverage.UFMap as UFMap
import Unison.Prelude
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.Syntax.TypePrinter as TypePrinter
import Unison.Type (Type, booleanRef, charRef, floatRef, intRef, listRef, natRef, textRef, pattern App', pattern Ref')
import Unison.Util.Pretty
import Unison.Var (Var)

-- | Normalized refinement types (fig 6)
--
-- Each variable may be associated with a number of constraints
-- represented by 'VarInfo'. A 'NormalizedConstraints' is conceptually
-- the conjunction of all constraints in the map. Disjunction is
-- represented by passing a Set of NormalizedConstraints. So, the
-- constraints are normalized into disjunctive normal form and each
-- @NormalizedConstraints@ is a DNF term.
--
-- Additionally, the following invariants are enforced (Section 3.4)
--
-- * Mutual compatibility: No two constraints should conflict with
-- each other.
--
-- * Inhabitation: There must be at least one value that inhabits each
-- refinement type. (N.B. We don't truly know if a type is inhabited,
-- see 'inhabited' in "Unison.PatternMatchCoverage.Solve" for
-- details. However, the refinement type is inhabited as far as our
-- inhabitation checker is concerned.)
--
-- These invariants ensure that each term in our DNF has at least one
-- solution, and it is easy to expand and print these solutions.
data NormalizedConstraints vt v loc = NormalizedConstraints
  { -- | Constraints keyed by the variable they constrain. Equality
    -- constraints are handled by 'UFMap'.
    constraintMap :: UFMap v (VarInfo vt v loc),
    -- | dirty variables are ones that must be checked for inhabitance
    dirtySet :: Set v
  }
  deriving stock (Eq, Ord, Show)

-- | Mark a variable as requiring a new test for inhabitation.
markDirty ::
  (Ord v) =>
  v ->
  NormalizedConstraints vt v loc ->
  NormalizedConstraints vt v loc
markDirty k nc@NormalizedConstraints {dirtySet} =
  nc {dirtySet = Set.insert k dirtySet}

emptyNormalizedConstraints :: (Ord v) => NormalizedConstraints vt v loc
emptyNormalizedConstraints =
  NormalizedConstraints
    { constraintMap = UFMap.empty,
      dirtySet = mempty
    }

-- | Lookup the canonical value of @v@ from the constraint map. Throws
-- an error if the variable is not in the map.
expectCanon ::
  forall vt v loc.
  (Var v) =>
  v ->
  NormalizedConstraints vt v loc ->
  (v, VarInfo vt v loc, NormalizedConstraints vt v loc)
expectCanon k nc =
  let ((v, vi), nc') = updateF k (\v vi -> ((v, vi), Ignore)) nc
   in (v, vi, nc')

-- | Alter a constraint, marks var as dirty if updated
alterF ::
  forall vt v loc f.
  (Var v, Functor f) =>
  v ->
  f (ConstraintUpdate (VarInfo vt v loc)) ->
  (v -> VarInfo vt v loc -> f (ConstraintUpdate (VarInfo vt v loc))) ->
  NormalizedConstraints vt v loc ->
  f (NormalizedConstraints vt v loc)
alterF v nothing just nc =
  (\(f, x) -> f nc {constraintMap = x})
    <$> getCompose
      ( UFMap.alterF
          v
          nothing'
          just'
          (constraintMap nc)
      )
  where
    just' canonK eqClassSize vi =
      fmap (UFMap.Canonical eqClassSize) $
        Compose $
          just canonK vi <&> \case
            Ignore -> (id, vi)
            Update vi -> (markDirty canonK, vi)
    nothing' =
      Compose $
        nothing <&> \case
          Ignore -> (id, Nothing)
          Update x -> (markDirty v, Just x)
{-# INLINE alterF #-}

-- | Generic function to lookup or alter constraints in the constraint
-- map. Throws an error if the variable is not in the map.
updateF ::
  forall vt v loc f.
  (Var v, Functor f) =>
  -- | variable to lookup
  v ->
  -- | update function
  (v -> VarInfo vt v loc -> f (ConstraintUpdate (VarInfo vt v loc))) ->
  -- | constraint map
  NormalizedConstraints vt v loc ->
  f (NormalizedConstraints vt v loc)
updateF v just nc =
  alterF v nothing just nc
  where
    nothing = error ("expected " <> show v <> " to be in UFMap")

data ConstraintUpdate a
  = Update a
  | Ignore
  deriving stock (Functor)

-- | Install a new variable into the constraint map. Throws an error
-- if the variable already exists in the map.
declVar ::
  forall vt v loc.
  (Var v) =>
  -- | new variable to install
  v ->
  -- | type of variable
  Type vt loc ->
  -- | modifier for the default var info of the given type
  (VarInfo vt v loc -> VarInfo vt v loc) ->
  -- | Normalized constraints to install the variable into
  NormalizedConstraints vt v loc ->
  NormalizedConstraints vt v loc
declVar v t f nc@NormalizedConstraints {constraintMap} =
  nc {constraintMap = UFMap.alter v nothing just constraintMap}
  where
    nothing =
      let !vi = f (mkVarInfo v t)
       in Just vi
    just _ _ _ = error ("attempted to declare: " <> show v <> " but it already exists")

mkVarInfo :: forall vt v loc. v -> Type vt loc -> VarInfo vt v loc
mkVarInfo v t =
  VarInfo
    { vi_id = v,
      vi_typ = t,
      vi_con = case t of
        App' (Ref' r) t
          | r == listRef -> Vc'ListRoot t Empty Empty (IntervalSet.singleton (0, maxBound))
        Ref' r
          | r == booleanRef -> Vc'Boolean Nothing mempty
          | r == intRef -> Vc'Int Nothing mempty
          | r == natRef -> Vc'Nat Nothing mempty
          | r == floatRef -> Vc'Float Nothing mempty
          | r == textRef -> Vc'Text Nothing mempty
          | r == charRef -> Vc'Char Nothing mempty
        -- this may not be a constructor, but we won't be producing
        -- any constraints for it in that case anyway
        _ -> Vc'Constructor Nothing mempty,
      vi_eff = IsNotEffectful
    }

-- | Normalized constraints on a specific variable
data VarInfo vt v loc = VarInfo
  { vi_id :: v,
    vi_typ :: Type vt loc,
    vi_con :: VarConstraints vt v loc,
    vi_eff :: EffectInfo
  }
  deriving stock (Show, Eq, Ord, Generic)

-- | The constraints are different for different types, although most
-- of them take the form of an optional positive constraint and a set
-- of negative constraints.
data VarConstraints vt v loc
  = Vc'Constructor
      (Maybe (ConstructorReference, [(v, Type vt loc)]))
      (Set ConstructorReference)
  | Vc'Boolean (Maybe Bool) (Set Bool)
  | Vc'Int (Maybe Int64) (Set Int64)
  | Vc'Nat (Maybe Word64) (Set Word64)
  | Vc'Float (Maybe Double) (Set Double)
  | Vc'Text (Maybe Text) (Set Text)
  | Vc'Char (Maybe Char) (Set Char)
  | Vc'ListRoot
      (Type vt loc)
      -- ^ type of list elems
      (Seq v)
      -- ^ Positive constraint on cons elements
      (Seq v)
      -- ^ Positive constraint on snoc elements
      IntervalSet
      -- ^ positive constraint on input list size
  deriving stock (Show, Eq, Ord, Generic)

data EffectInfo
  = IsEffectful
  | IsNotEffectful
  deriving stock (Show, Eq, Ord)

prettyNormalizedConstraints :: forall vt v loc. (Var v, Var vt) => PPE.PrettyPrintEnv -> NormalizedConstraints vt v loc -> Pretty ColorText
prettyNormalizedConstraints ppe (NormalizedConstraints {constraintMap}) = sep " " ["⟨", pconstraints, "⟩"]
  where
    cls = UFMap.toClasses constraintMap

    pconstraints = sep ", " (prettyCon <$> cls)
    prettyCon (kcanon, ks, vi) =
      let posCon = fromMaybe [] $ case vi_con vi of
            Vc'Constructor pos _neg ->
              (\(datacon, convars) -> [PosCon kcanon datacon convars]) <$> pos
            Vc'Boolean pos _neg ->
              (\x -> [PosLit kcanon (PmLit.Boolean x)]) <$> pos
            Vc'Int pos _neg ->
              (\x -> [PosLit kcanon (PmLit.Int x)]) <$> pos
            Vc'Nat pos _neg ->
              (\x -> [PosLit kcanon (PmLit.Nat x)]) <$> pos
            Vc'Float pos _neg ->
              (\x -> [PosLit kcanon (PmLit.Float x)]) <$> pos
            Vc'Text pos _neg ->
              (\x -> [PosLit kcanon (PmLit.Text x)]) <$> pos
            Vc'Char pos _neg ->
              (\x -> [PosLit kcanon (PmLit.Char x)]) <$> pos
            Vc'ListRoot _typ posCons posSnoc _iset ->
              let consConstraints = fmap (\(i, x) -> PosListHead kcanon i x) (zip [0 ..] (toList posCons))
                  snocConstraints = fmap (\(i, x) -> PosListTail kcanon i x) (zip [0 ..] (toList posSnoc))
               in Just (consConstraints ++ snocConstraints)
          negConK :: forall x. Set x -> (v -> x -> Constraint vt v loc) -> [Constraint vt v loc]
          negConK s f = foldr (\a b -> f kcanon a : b) [] s
          negCon = case vi_con vi of
            Vc'Constructor _pos neg -> negConK neg NegCon
            Vc'Boolean _pos neg -> negConK neg (\v a -> NegLit v (PmLit.Boolean a))
            Vc'Int _pos neg -> negConK neg (\v a -> NegLit v (PmLit.Int a))
            Vc'Nat _pos neg -> negConK neg (\v a -> NegLit v (PmLit.Nat a))
            Vc'Float _pos neg -> negConK neg (\v a -> NegLit v (PmLit.Float a))
            Vc'Text _pos neg -> negConK neg (\v a -> NegLit v (PmLit.Text a))
            Vc'Char _pos neg -> negConK neg (\v a -> NegLit v (PmLit.Char a))
            Vc'ListRoot _typ _posCons _posSnoc iset -> [NegListInterval kcanon (IntervalSet.complement iset)]
          botCon = case vi_eff vi of
            IsNotEffectful -> []
            IsEffectful -> [Effectful kcanon]
       in sep " " $
            prettyVar kcanon
              : fmap prettyVar (Set.toList $ Set.delete kcanon ks)
              ++ [":", TypePrinter.pretty ppe (vi_typ vi)]
              ++ ["|"]
              ++ case posCon ++ negCon ++ botCon of
                [] -> ["∅"]
                _ -> [sep ", " $ fmap (prettyConstraint ppe) (posCon ++ negCon ++ botCon)]

prettyDnf :: (Var v, Var vt) => PPE.PrettyPrintEnv -> Set (NormalizedConstraints vt v loc) -> Pretty ColorText
prettyDnf ppe xs = sep " " ("{" : sep ", " (prettyNormalizedConstraints ppe <$> Set.toList xs) : ["}"])
