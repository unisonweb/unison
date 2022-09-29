module Unison.PatternMatchCoverage.Constraint where

import Unison.ConstructorReference (ConstructorReference)
import Unison.PatternMatchCoverage.IntervalSet (IntervalSet)
import Unison.PatternMatchCoverage.PmLit
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.Syntax.TypePrinter as TypePrinter
import Unison.Type (Type)
import Unison.Util.Pretty
import Unison.Var (Var)

data Constraint vt v loc
  = PosCon v ConstructorReference [(v, Type vt loc)]
  | NegCon v ConstructorReference
  | PosLit v PmLit
  | NegLit v PmLit
  | PosListHead
      v
      -- ^ list root
      Int
      -- ^ cons position (0 is head)
      v
      -- ^ element variable
  | PosListTail
      v
      -- ^ list root
      Int
      -- ^ snoc position (0 is last)
      v
      -- ^ element variable
  | NegListInterval v IntervalSet
  | Effectful v
  | Eq v v
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
    pc :: forall a. Show a => a -> Pretty ColorText
    pc = string . show
