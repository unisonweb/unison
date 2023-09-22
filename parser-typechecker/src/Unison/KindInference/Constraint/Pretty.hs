{-# LANGUAGE RecursiveDo #-}

module Unison.KindInference.Constraint.Pretty
  ( prettyCyclicUVarKind
  , prettyUVarKind
  , prettySolvedConstraint
  ) where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Unison.KindInference.Solve.Monad
  ( ConstraintMap,
    Env (..),
    Solve (..),
    SolveState (..),
    find,
    run,
  )
import Unison.KindInference.Constraint.Solved qualified as Solved
import Unison.KindInference.UVar (UVar (..))
import Unison.Prelude
import Unison.PrettyPrintEnv (PrettyPrintEnv)
import Unison.Util.Pretty qualified as P
import Unison.Var (Var)

prettyEffect :: Int -> P.Pretty P.ColorText
prettyEffect _prec = "Effect"

prettyStar :: Int -> P.Pretty P.ColorText
prettyStar _prec = "*"

prettyUnknown :: Int -> P.Pretty P.ColorText
prettyUnknown _prec = "_"

prettyArrow :: Int -> P.Pretty P.ColorText -> P.Pretty P.ColorText -> P.Pretty P.ColorText
prettyArrow prec lhs rhs =
  let wrap = if prec > arrPrec then P.parenthesize else id
  in wrap (lhs <> " -> " <> rhs)

prettyCyclicSolvedConstraint ::
  Var v =>
  Solved.Constraint (UVar v loc) v loc ->
  Int ->
  Map (UVar v loc) (P.Pretty P.ColorText) ->
  Set (UVar v loc) ->
  Solve v loc (P.Pretty P.ColorText, Set (UVar v loc))
prettyCyclicSolvedConstraint constraint prec nameMap visitingSet = case constraint of
  Solved.IsEffect _ -> pure (prettyEffect prec, Set.empty)
  Solved.IsStar _ -> pure (prettyStar prec, Set.empty)
  Solved.IsArr _ a b -> do
    (pa, cyclicLhs) <- case Set.member a visitingSet of
      True -> pure (nameMap Map.! a, Set.singleton a)
      False -> prettyCyclicUVarKindWorker (arrPrec + 1) a nameMap visitingSet
    (pb, cyclicRhs) <- case Set.member b visitingSet of
      True -> pure (nameMap Map.! b, Set.singleton b)
      False -> prettyCyclicUVarKindWorker arrPrec b nameMap visitingSet
    pure (prettyArrow prec pa pb, cyclicLhs <> cyclicRhs)

prettyCyclicUVarKindWorker ::
  Var v =>
  Int ->
  UVar v loc ->
  Map (UVar v loc) (P.Pretty P.ColorText) ->
  Set (UVar v loc) ->
  Solve v loc (P.Pretty P.ColorText, Set (UVar v loc))
prettyCyclicUVarKindWorker prec u nameMap visitingSet =
  find u >>= \case
    Nothing -> pure (prettyUnknown prec, Set.empty)
    Just c -> do
      let visitingSet1 = Set.insert u visitingSet
      prettyCyclicSolvedConstraint c prec nameMap visitingSet1

prettyUVarKind :: Var v => PrettyPrintEnv -> ConstraintMap v loc -> UVar v loc -> P.Pretty P.ColorText
prettyUVarKind ppe constraints uvar = ppRunner ppe constraints do
  prettyUVarKind' arrPrec uvar

prettyUVarKind' :: Var v => Int -> UVar v loc -> Solve v loc (P.Pretty P.ColorText)
prettyUVarKind' prec u =
  find u >>= \case
    Nothing -> pure (prettyUnknown prec)
    Just c -> prettySolvedConstraint' prec c

prettySolvedConstraint :: Var v => PrettyPrintEnv -> ConstraintMap v loc -> Solved.Constraint (UVar v loc) v loc -> P.Pretty P.ColorText
prettySolvedConstraint ppe constraints c =
  ppRunner ppe constraints (prettySolvedConstraint' arrPrec c)

prettySolvedConstraint' :: Var v => Int -> Solved.Constraint (UVar v loc) v loc -> Solve v loc (P.Pretty P.ColorText)
prettySolvedConstraint' prec = \case
  Solved.IsEffect _ -> pure (prettyEffect prec)
  Solved.IsStar _ -> pure (prettyStar prec)
  Solved.IsArr _ a b -> do
    a <- prettyUVarKind' (arrPrec + 1) a
    b <- prettyUVarKind' arrPrec b
    pure (prettyArrow prec a b)

ppRunner :: Var v => PrettyPrintEnv -> ConstraintMap v loc -> (forall r. Solve v loc r -> r)
ppRunner ppe constraints =
  let st =
        SolveState
          { unifVars = Set.empty,
            newUnifVars = [],
            constraints = constraints,
            typeMap = mempty
          }
      env = Env ppe
   in \solve -> fst (run env st solve)

arrPrec :: Int
arrPrec = 1

prettyCyclicUVarKind ::
  Var v =>
  PrettyPrintEnv ->
  ConstraintMap v loc ->
  UVar v loc ->
  (P.Pretty P.ColorText -> P.Pretty P.ColorText) ->
  (P.Pretty P.ColorText, P.Pretty P.ColorText)
prettyCyclicUVarKind ppe constraints uvar theUVarStyle = ppRunner ppe constraints do
  find uvar >>= \case
    Nothing -> explode
    Just c -> do
      rec (pp, cyclicUVars) <- prettyCyclicSolvedConstraint c arrPrec nameMap (Set.singleton uvar)
          let nameMap = snd $ foldl' phi (0 :: Int, Map.empty) cyclicUVars
              phi (n, m) a =
                let name = P.string (if n == 0 then "k" else "k" <> show n)
                    !newN = n + 1
                    prettyVar = case a == uvar of
                      True -> theUVarStyle name
                      False -> name
                    !newMap = Map.insert a prettyVar m
                 in (newN, newMap)
      case Map.lookup uvar nameMap of
        Nothing -> explode
        Just n -> pure (n, P.wrap (n <> "=" <> pp))
  where
    explode = error ("[prettyCyclicUVarKind] called with non-cyclic uvar: " <> show uvar)
