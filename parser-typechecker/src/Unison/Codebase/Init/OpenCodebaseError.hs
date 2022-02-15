-- | Open codebase error type.
module Unison.Codebase.Init.OpenCodebaseError
  ( OpenCodebaseError (..),
    GetRootBranchError (..),
  )
where

import qualified Unison.Codebase.Branch as Branch
import Unison.CodebasePath (CodebasePath)
import Unison.Prelude
import Unison.Util.Pretty (ColorText, Pretty)

data GetRootBranchError
  = NoRootBranch
  | CouldntParseRootBranch String
  | CouldntLoadRootBranch Branch.Hash
  deriving stock (Show)

-- | An error that can occur when attempting to open a codebase.
data OpenCodebaseError
  = -- | The codebase doesn't exist.
    OpenCodebaseDoesntExist CodebasePath
  | -- | The codebase exists, but its schema version is unknown to this application.
    OpenCodebaseUnknownSchemaVersion Word64
  | OpenCodebaseRootBranchError GetRootBranchError
  | OpenCodebaseOther (Pretty ColorText)
  deriving stock (Show)
