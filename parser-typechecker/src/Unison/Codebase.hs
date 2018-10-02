module Unison.Codebase where

import qualified Unison.Term as Term
import qualified Unison.DataDeclaration as DD
import Unison.Reference (Reference)
import Unison.Codebase.Code (Code)
import Unison.Codebase.Branch (Branch)
import Unison.Codebase.Release (Release)

type DataDeclaration v a = DD.DataDeclaration' v a
type Term v a = Term.AnnotatedTerm v a
type Name = String

data Codebase m v a =
  Codebase { getCode :: Reference -> m (Maybe (Code v a))
           , putCode :: Code v a -> m Reference
           , branches :: m [Name]
           , getBranch :: Name -> m (Maybe Branch)
           , putBranch :: Name -> Branch -> m ()
           , releases :: m [Name]
           , getRelease :: Name -> m (Maybe (Release v a))
           , putRelease :: Name -> Release v a -> m ()
           }

