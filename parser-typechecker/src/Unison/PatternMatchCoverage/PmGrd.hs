module Unison.PatternMatchCoverage.PmGrd where

import Unison.ConstructorReference (ConstructorReference)
import Unison.PatternMatchCoverage.PmLit (PmLit, prettyPmLit)
import Unison.PatternMatchCoverage.Pretty
import Unison.PrettyPrintEnv qualified as PPE
import Unison.Syntax.TypePrinter qualified as TypePrinter
import Unison.Term (Term')
import Unison.Type (Type)
import Unison.Util.Pretty
import Unison.Var (Var)

data
  PmGrd
    vt -- Type variable
    v -- Term variable
    loc -- annotation
  = -- | @PmCon x Con xs ys@ corresponds to the constraint @Con ys <- x@
    PmCon
      -- | Variable
      v
      -- | Constructor
      ConstructorReference
      -- | Constructor argument values and types
      [(v, Type vt loc)]
  | PmEffect
      -- | Variable
      v
      -- | Constructor
      ConstructorReference
      -- | Constructor argument values and types
      [(v, Type vt loc)]
  | PmEffectPure v (v, Type vt loc)
  | PmLit v PmLit
  | PmListHead
      -- | list root
      v
      -- | cons position (0 is head)
      Int
      -- | element variable
      v
      -- | element type
      (Type vt loc)
  | PmListTail
      -- | list root
      v
      -- | snoc position (0 is last)
      Int
      -- | element variable
      v
      -- | element type
      (Type vt loc)
  | -- | The size of the list must fall within this inclusive range
    PmListInterval v Int Int
  | -- | If a guard performs an effect
    PmBang v
  | -- | @PmLet x expr@ corresponds to a @let x = expr@ guard. This actually
    -- /binds/ @x@.
    PmLet v (Term' vt v loc) (Type vt loc)
  deriving stock (Show)

prettyPmGrd :: (Var vt, Var v) => PPE.PrettyPrintEnv -> PmGrd vt v loc -> Pretty ColorText
prettyPmGrd ppe = \case
  PmCon var con convars ->
    let xs = pc con : fmap (\(trm, typ) -> sep " " ["(" <> prettyVar trm, ":", TypePrinter.pretty ppe typ <> ")"]) convars ++ ["<-", prettyVar var]
     in sep " " xs
  PmEffect var con convars ->
    let xs = pc con : fmap (\(trm, typ) -> sep " " ["(" <> prettyVar trm, ":", TypePrinter.pretty ppe typ <> ")"]) convars ++ ["<-", prettyVar var]
     in sep " " xs
  PmEffectPure v (rv, rt) -> sep " " ["pure", "(" <> prettyVar rv, ":", TypePrinter.pretty ppe rt <> ")", "<-", prettyVar v]
  PmListHead var n el _ -> sep " " ["Cons", string (show n), prettyVar el, "<-", prettyVar var]
  PmListTail var n el _ -> sep " " ["Snoc", string (show n), prettyVar el, "<-", prettyVar var]
  PmListInterval var minLen maxLen -> sep " " ["Interval", string (show (minLen, maxLen)), "<-", prettyVar var]
  PmLit var lit -> sep " " [prettyPmLit lit, "<-", prettyVar var]
  PmBang v -> "!" <> prettyVar v
  PmLet v _expr _ -> sep " " ["let", prettyVar v, "=", "<expr>"]
  where
    pc = prettyConstructorReference ppe
