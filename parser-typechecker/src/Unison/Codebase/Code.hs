module Unison.Codebase.Code where

import qualified Unison.Term as Term
import qualified Unison.Type as Type
import qualified Unison.DataDeclaration as DD

type DataDeclaration v a = DD.DataDeclaration' v a
type EffectDeclaration v a = DD.EffectDeclaration' v a
type Term v a = Term.AnnotatedTerm v a
type Type v a = Type.AnnotatedType v a

data Code v a
  = Term (Term v a) (Type v a)
  | DataDeclaration (DataDeclaration v a)
  | EffectDeclaration (EffectDeclaration v a)

