-- | Open codebase error type.
module Unison.Codebase.Init.OpenCodebaseError
  ( OpenCodebaseError (..),
    GetBranchError (..),
  )
where

import qualified Unison.Codebase.Branch as Branch
import Unison.Codebase.Path (Path)
import Unison.Codebase.ShortBranchHash (ShortBranchHash)
import Unison.Prelude

data GetBranchError
  = NoRootBranch
  | CouldntParseRootBranch String
  | CouldntLoadRootBranch (Either ShortBranchHash Branch.Hash)
  | CouldntFindPath Path
  | BranchHashAmbiguous
      ShortBranchHash -- Ambiguous hash
      (Set Branch.Hash) -- Possible completions
  deriving stock (Show)

-- | An error that can occur when attempting to open a codebase.
data OpenCodebaseError
  = -- | The codebase doesn't exist.
    CodebaseDoesntExist
  | -- | The codebase exists, but its schema version is unknown to this application.
    UnknownSchemaVersion Word64
  | GetBranchError GetBranchError
  deriving stock (Show)
