module Unison.PatternMatchCoverage.Literal
  ( Literal (..),
  )
where

import Unison.ConstructorReference (ConstructorReference)
import Unison.PatternMatchCoverage.EffectHandler
import Unison.PatternMatchCoverage.IntervalSet (IntervalSet)
import Unison.PatternMatchCoverage.PmLit (PmLit)
import Unison.Term (Term')
import Unison.Type (Type)

-- | Refinement type literals (fig 3)
data Literal vt v loc
  = -- | True
    T
  | -- | False
    F
  | -- | Positive constraint regarding data type. States that the
    -- given variable must be the given constructor, and it also binds
    -- variables corresponding to constructor arguments.
    PosCon v ConstructorReference [(v, Type vt loc)]
  | -- | Negative constraint concerning data type. States that the
    -- given variable must not be the given constructor.
    NegCon v ConstructorReference
  | -- | Positive constraint regarding data type. States that the
    -- given variable must be the given constructor, and it also binds
    -- variables corresponding to constructor arguments.
    PosEffect v EffectHandler [(v, Type vt loc)]
  | -- | Negative constraint concerning data type. States that the
    -- given variable must not be the given constructor.
    NegEffect v EffectHandler
  | -- | Positive constraint regarding literal
    PosLit v PmLit
  | -- | Negative constraint regarding literal
    NegLit v PmLit
  | -- | Positive constraint on list element with position relative to head of list
    PosListHead
      -- | list root
      v
      -- | cons position (0 is head)
      Int
      -- | element variable
      v
      (Type vt loc)
  | -- | Positive constraint on list element with position relative to end of list
    PosListTail
      -- | list root
      v
      -- | snoc position (0 is last)
      Int
      -- | element variable
      v
      (Type vt loc)
  | -- | Negative constraint on length of the list (/i.e./ the list
    -- may not be an element of the interval set)
    NegListInterval v IntervalSet
  | -- | An effect is performed
    Effectful v
  | -- | Introduce a binding for a term
    Let v (Term' vt v loc) (Type vt loc)
  deriving stock (Show)
