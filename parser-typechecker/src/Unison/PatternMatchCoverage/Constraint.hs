module Unison.PatternMatchCoverage.Constraint
  ( Constraint (..),
    prettyConstraint,
  )
where

import Unison.ConstructorReference (ConstructorReference)
import Unison.PatternMatchCoverage.IntervalSet (IntervalSet)
import Unison.PatternMatchCoverage.PmLit
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.Syntax.TypePrinter as TypePrinter
import Unison.Type (Type)
import Unison.Util.Pretty
import Unison.Var (Var)

-- | A constraint to add to a [normalized constraint
-- set]("Unison.PatternMatchCoverage.NormalizedConstraints") (fig 6)
-- See 'Unison.PatternMatchCoverage.Solve.addConstraint'
data Constraint vt v loc
  = -- | Positive constraint regarding data type. States that the
    -- given variable must be the given constructor, and it also binds
    -- variables corresponding to constructor arguments.
    PosCon v ConstructorReference [(v, Type vt loc)]
  | -- | Negative constraint concerning data type. States that the
    -- given variable must not be the given constructor.
    NegCon v ConstructorReference
  | -- | Positive constraint regarding literal
    PosLit v PmLit
  | -- | Negative constraint regarding literal
    NegLit v PmLit
  | -- | Positive constraint on list element with position relative to head of list
    PosListHead
      v
      -- ^ list root
      Int
      -- ^ cons position (0 is head)
      v
      -- ^ element variable
  | -- | Positive constraint on list element with position relative to end of list
    PosListTail
      v
      -- ^ list root
      Int
      -- ^ snoc position (0 is last)
      v
      -- ^ element variable
  | -- | Negative constraint on length of the list (/i.e./ the list
    -- may not be an element of the interval set)
    NegListInterval v IntervalSet
  | -- | An effect is performed
    Effectful v
  | -- | Equality constraint
    Eq v v
  deriving stock (Eq, Ord)

prettyConstraint :: (Var vt, Var v) => Constraint vt v loc -> Pretty ColorText
prettyConstraint = \case
  PosCon var con convars ->
    let xs = pc con : fmap (\(trm, typ) -> sep " " [pv trm, ":", TypePrinter.pretty PPE.empty typ]) convars ++ ["<-", pv var]
     in sep " " xs
  NegCon var con -> sep " " [pv var, "≠", pc con]
  PosLit var lit -> sep " " [prettyPmLit lit, "<-", pv var]
  NegLit var lit -> sep " " [pv var, "≠", prettyPmLit lit]
  PosListHead root n el -> sep " " [pv el, "<-", "head", pc n, pv root]
  PosListTail root n el -> sep " " [pv el, "<-", "tail", pc n, pv root]
  NegListInterval var x -> sep " " [pv var, "≠", string (show x)]
  Effectful var -> "!" <> pv var
  Eq v0 v1 -> sep " " [pv v0, "=", pv v1]
  where
    pv = string . show
    pc :: forall a. (Show a) => a -> Pretty ColorText
    pc = string . show
