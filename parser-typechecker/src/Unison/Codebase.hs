module Unison.Codebase where

import Data.Text (Text)
import qualified Unison.Term as Term
import qualified Unison.DataDeclaration as DD
import Unison.Reference (Reference)
import Unison.Codebase.Code (Code)
import Unison.Codebase.Branch (Branch)
import System.FilePath (FilePath)
import Unison.Result (Result, Note)
import Unison.Parser (Ann)
import Unison.UnisonFile (UnisonFile')

type DataDeclaration v a = DD.DataDeclaration' v a
type Term v a = Term.AnnotatedTerm v a
type Name = String


data Codebase m v a =
  Codebase { getCode :: Reference -> m (Maybe (Code v a))
           , putCode :: Code v a -> m Reference
           , branches :: m [Name]
           , getBranch :: Name -> m (Maybe Branch)
           -- thought: this merges the given branch with the existing branch
           -- or creates a new branch if there's no branch with that name
           , mergeBranch :: Name -> Branch -> m ()
           }

data Session m v a
  = Session { branch :: m Name
            , switchBranch :: Name -> m ()
            -- Await the next .u file change in the given directory,
            -- and return the path of the thing that changed, its contents,
            -- and the results of parsing / typechecking.
            , watch :: FilePath -> m (FilePath, Text, Result (Note v Ann) (UnisonFile' v Ann)) }
