module Unison.PatternMatchCoverage.Literal where

import Unison.ConstructorReference (ConstructorReference)
import Unison.PatternMatchCoverage.IntervalSet (IntervalSet)
import Unison.PatternMatchCoverage.PmLit (PmLit, prettyPmLit)
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.Syntax.TermPrinter as TermPrinter
import qualified Unison.Syntax.TypePrinter as TypePrinter
import Unison.Term (Term')
import Unison.Type (Type)
import Unison.Typechecker.TypeVar (TypeVar, lowerTerm)
import Unison.Util.Pretty
import Unison.Var (Var)

data Literal vt v loc
  = T
  | F
  | PosCon v ConstructorReference [(v, Type vt loc)]
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
      (Type vt loc)
  | PosListTail
      v
      -- ^ list root
      Int
      -- ^ snoc position (0 is last)
      v
      -- ^ element variable
      (Type vt loc)
  | NegListInterval v IntervalSet
  | Effectful v
  | Let v (Term' vt v loc) (Type vt loc)
  deriving stock (Show)

prettyLiteral :: (Var v) => Literal (TypeVar b v) v loc -> Pretty ColorText
prettyLiteral = \case
  T -> "✓"
  F -> "⨉"
  PosCon var con convars ->
    let xs = pc con : fmap (\(trm, typ) -> sep " " [pv trm, ":", TypePrinter.pretty PPE.empty typ]) convars ++ ["<-", pv var]
     in sep " " xs
  NegCon var con -> sep " " [pv var, "≠", pc con]
  PosLit var lit -> sep " " [prettyPmLit lit, "<-", pv var]
  NegLit var lit -> sep " " [pv var, "≠", prettyPmLit lit]
  PosListHead root n el _ -> sep " " [pv el, "<-", "head", pc n, pv root]
  PosListTail root n el _ -> sep " " [pv el, "<-", "tail", pc n, pv root]
  NegListInterval var x -> sep " " [pv var, "≠", string (show x)]
  Effectful var -> "!" <> pv var
  Let var expr typ -> sep " " ["let", pv var, "=", TermPrinter.pretty PPE.empty (lowerTerm expr), ":", TypePrinter.pretty PPE.empty typ]
  where
    pv = string . show
    pc :: forall a. (Show a) => a -> Pretty ColorText
    pc = string . show
