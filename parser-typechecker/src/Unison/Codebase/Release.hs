module Unison.Codebase.Release where

import Data.Map (Map)
import Unison.Codebase.Code (Code)
import Unison.Codebase.Name (Name)
import Unison.Codebase.TermEdit (TermEdit)
import Unison.Codebase.TypeEdit (TypeEdit)
import Unison.Reference (Reference)
import qualified Unison.DataDeclaration as DD
import qualified Unison.Term as Term

type DataDeclaration v a = DD.DataDeclaration' v a
type Term v a = Term.AnnotatedTerm v a

data Release v a =
  Release { namespace     :: Map Name (Code v a)
          , edited        :: Map Reference TermEdit
          , editedDatas   :: Map Reference TypeEdit
          , editedEffects :: Map Reference TypeEdit }


