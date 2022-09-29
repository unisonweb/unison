module Unison.PatternMatchCoverage.PmGrd where

import Unison.ConstructorReference (ConstructorReference)
import Unison.PatternMatchCoverage.PmLit (PmLit, prettyPmLit)
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

prettyPmGrd :: (Var vt, Var v) => PmGrd vt v loc -> Pretty ColorText
prettyPmGrd = \case
  PmCon var con convars ->
    let xs = string (show con) : (formatConVar <$> convars) ++ ["<-", string (show var)]
        formatConVar (v, t) = sep " " ["(", string (show v), "::", TypePrinter.pretty PPE.empty t, ")"]
     in sep " " xs
  PmListHead var n el _ -> sep " " ["Cons", string (show n), string (show el), "<-", string (show var)]
  PmListTail var n el _ -> sep " " ["Snoc", string (show n), string (show el), "<-", string (show var)]
  PmListInterval var minLen maxLen -> sep " " ["Interval", string (show (minLen, maxLen)), "<-", string (show var)]
  PmLit var lit -> sep " " [prettyPmLit lit, "<-", string (show var)]
  PmBang v -> "!" <> string (show v)
  PmLet v _expr _ -> sep " " ["let", string (show v), "=", "<expr>"]
