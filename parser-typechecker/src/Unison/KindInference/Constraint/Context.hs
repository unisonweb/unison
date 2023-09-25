module Unison.KindInference.Constraint.Context
  ( ConstraintContext(..)
  ) where

import Unison.KindInference.UVar (UVar)
import Unison.Type (Type)

-- | The context in which the constraint was generated. This is useful
-- when generating user-facing error messages.
data ConstraintContext v loc
  = AppAbs !(UVar v loc) !(UVar v loc)
  | AppArg !(UVar v loc) !(UVar v loc) !(UVar v loc)
  | AppArrow loc !(Type v loc) !(Type v loc)
  | EffectsList
  | ScopeReference
  | TypeAnnotation
  | DeclDefinition
  | Builtin
  | ContextLookup
  deriving stock (Show, Eq, Ord)
