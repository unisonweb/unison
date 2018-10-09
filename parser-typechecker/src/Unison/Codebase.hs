module Unison.Codebase where

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
           , putBranch :: Name -> Branch -> m ()
           }

-- what about concept of the 'current' branch?
-- this is a property of the tool, not the codebase itself
-- codebase is JUST the store of code
-- thought: possibly have a separate `Session` type for tracking a
-- more stateful interaction with a Codebase?
--
-- TODO: if the watch typechecks, it should returns the list of [(v, Term, Type)] tuples from the file
-- as well the data and effect declarations
-- if it doesn't parse or typecheck, it should return the errors
data Session m v a
  = Session { currentBranch :: m Name
            -- Await the next .u file change in the given directory,
            -- and return the changed path and its contents
            , watch :: FilePath -> m (FilePath, Result (Note v Ann) (UnisonFile' v Ann)) }
