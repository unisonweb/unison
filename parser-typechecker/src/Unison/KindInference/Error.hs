module Unison.KindInference.Error
  ( KindError (..),
    ConstraintConflict (..),
    improveError,
  )
where

import Control.Lens ((^.))
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
import Unison.Type (Type)
import Unison.Var (Var)

data ConstraintConflict v loc = ConstraintConflict'
  { conflictedVar :: UVar v loc,
    impliedConstraint :: Solved.Constraint (UVar v loc) v loc,
    conflictedConstraint :: Solved.Constraint (UVar v loc) v loc
  }

data KindError v loc
  = CycleDetected loc (UVar v loc) (ConstraintMap v loc)
  | UnexpectedArgument
      loc
      -- ^ src span of abs
      (UVar v loc)
      -- ^ abs var
      (UVar v loc)
      -- ^ arg var
      (ConstraintMap v loc)
      -- ^ context
  | ArgumentMismatch
      (UVar v loc)
      -- ^ abs var
      (UVar v loc)
      -- ^ expected var
      (UVar v loc)
      -- ^ given var
      (ConstraintMap v loc)
      -- ^ context
  | ArgumentMismatchArrow
      (loc, Type v loc, Type v loc)
      -- ^ (The applied arrow range, lhs, rhs)
      (ConstraintConflict v loc)
      (ConstraintMap v loc)
  | EffectListMismatch
      (ConstraintConflict v loc)
      (ConstraintMap v loc)
  | ConstraintConflict
      (GeneratedConstraint v loc)
      -- ^ Failed to add this constraint
      (ConstraintConflict v loc)
      -- ^ Due to this conflict
      (ConstraintMap v loc)
      -- ^ in this context

improveError :: Var v => KindError v loc -> Solve v loc (KindError v loc)
improveError = \case
  ConstraintConflict a b c -> improveError' a b c
  e -> pure e

improveError' ::
  Var v =>
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
