module Unison.Codebase.Code where

import qualified Unison.Term as Term
import qualified Unison.DataDeclaration as DD

type DataDeclaration v a = DD.DataDeclaration' v a
type Term v a = Term.AnnotatedTerm v a

data Code v a
  = Term (Term v a)
  | DataDeclaration (DataDeclaration v a)

