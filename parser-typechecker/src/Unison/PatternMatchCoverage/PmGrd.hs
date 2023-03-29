module Unison.PatternMatchCoverage.PmGrd where

import Unison.ConstructorReference (ConstructorReference)
import Unison.PatternMatchCoverage.PmLit (PmLit, prettyPmLit)
import Unison.PatternMatchCoverage.Pretty
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.Syntax.TypePrinter as TypePrinter
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
      v
      -- ^ Variable
      ConstructorReference
      -- ^ Constructor
      [(v, Type vt loc)]
      -- ^ Constructor argument values and types
  | PmLit v PmLit
  | PmListHead
      v
      -- ^ list root
      Int
      -- ^ cons position (0 is head)
      v
      -- ^ element variable
      (Type vt loc)
      -- ^ element type
  | PmListTail
      v
      -- ^ list root
      Int
      -- ^ snoc position (0 is last)
      v
      -- ^ element variable
      (Type vt loc)
      -- ^ element type
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
  PmListHead var n el _ -> sep " " ["Cons", string (show n), prettyVar el, "<-", prettyVar var]
  PmListTail var n el _ -> sep " " ["Snoc", string (show n), prettyVar el, "<-", prettyVar var]
  PmListInterval var minLen maxLen -> sep " " ["Interval", string (show (minLen, maxLen)), "<-", prettyVar var]
  PmLit var lit -> sep " " [prettyPmLit lit, "<-", prettyVar var]
  PmBang v -> "!" <> prettyVar v
  PmLet v _expr _ -> sep " " ["let", prettyVar v, "=", "<expr>"]
  where
    pc = prettyConstructorReference ppe
