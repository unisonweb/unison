module Unison.KindInference.Error
  ( KindError (..),
    lspLoc,
    ConstraintConflict (..),
    improveError,
  )
where

import Unison.ABT qualified as ABT
import Unison.KindInference.Constraint.Context (ConstraintContext (..))
import Unison.KindInference.Constraint.Provenance (Provenance (..))
import Unison.KindInference.Constraint.Solved qualified as Solved
import Unison.KindInference.Constraint.Unsolved qualified as Unsolved
import Unison.KindInference.Generate.Monad (GeneratedConstraint)
import Unison.KindInference.Solve.Monad
  ( ConstraintMap,
    Solve (..),
  )
import Unison.KindInference.UVar (UVar (..))
import Unison.Prelude
import Unison.Type (Type)
import Unison.Var (Var)

-- | Two incompatible constraints on a @UVar@.
data ConstraintConflict v loc = ConstraintConflict'
  { conflictedVar :: UVar v loc,
    impliedConstraint :: Solved.Constraint (UVar v loc) v loc,
    conflictedConstraint :: Solved.Constraint (UVar v loc) v loc
  }

lspLoc :: (Semigroup loc) => KindError v loc -> loc
lspLoc = \case
  CycleDetected loc _ _ -> loc
  UnexpectedArgument _ abs arg _ -> varLoc abs <> varLoc arg
  ArgumentMismatch abs _ actual _ -> varLoc abs <> varLoc actual
  ArgumentMismatchArrow _ ConstraintConflict' {conflictedVar} _ -> varLoc conflictedVar
  EffectListMismatch ConstraintConflict' {conflictedVar} _ -> varLoc conflictedVar
  ConstraintConflict gen _ _ -> gen ^. Unsolved.loc
  where
    varLoc var = ABT.annotation $ uvarType var

-- | Errors that may arise during kind inference
data KindError v loc
  = -- | A variable is constrained to have an infinite kind
    CycleDetected loc (UVar v loc) (ConstraintMap v loc)
  | -- | Something of kind * or Effect is applied to an argument
    UnexpectedArgument
      -- | src span of abs
      loc
      -- | abs var
      (UVar v loc)
      -- | arg var
      (UVar v loc)
      -- | context
      -- | An arrow kind is applied to a type, but its kind doesn't match
      -- the expected argument kind
      (ConstraintMap v loc)
  | ArgumentMismatch
      -- | abs var
      (UVar v loc)
      -- | expected var
      (UVar v loc)
      -- | given var
      (UVar v loc)
      -- | context
      -- | Same as @ArgumentMismatch@, but for applications to the builtin
      -- @Arrow@ type.
      (ConstraintMap v loc)
  | ArgumentMismatchArrow
      -- | (The applied arrow range, lhs, rhs)
      (loc, Type v loc, Type v loc)
      (ConstraintConflict v loc)
      (ConstraintMap v loc)
  | -- | Something appeared in an effect list that isn't of kind Effect
    EffectListMismatch
      (ConstraintConflict v loc)
      (ConstraintMap v loc)
  | -- | Generic constraint conflict
    ConstraintConflict
      -- | Failed to add this constraint
      (GeneratedConstraint v loc)
      -- | Due to this conflict
      (ConstraintConflict v loc)
      -- | in this context
      (ConstraintMap v loc)

-- | Transform generic constraint conflicts into more specific error
-- by examining its @ConstraintContext@.
improveError :: (Var v) => KindError v loc -> Solve v loc (KindError v loc)
improveError = \case
  ConstraintConflict a b c -> improveError' a b c
  e -> pure e

improveError' ::
  (Var v) =>
  GeneratedConstraint v loc ->
  ConstraintConflict v loc ->
  ConstraintMap v loc ->
  Solve v loc (KindError v loc)
improveError' generatedConstraint constraintConflict constraintMap =
  let Provenance ctx loc = generatedConstraint ^. Unsolved.prov
   in case ctx of
        AppAbs abs arg -> pure (UnexpectedArgument loc abs arg constraintMap)
        AppArg abs expected actual -> pure (ArgumentMismatch abs expected actual constraintMap)
        AppArrow loc dom cod -> pure (ArgumentMismatchArrow (loc, dom, cod) constraintConflict constraintMap)
        EffectsList -> pure (EffectListMismatch constraintConflict constraintMap)
        _ -> pure (ConstraintConflict generatedConstraint constraintConflict constraintMap)

instance Show (KindError v loc) where
  show _ = "kind error"
