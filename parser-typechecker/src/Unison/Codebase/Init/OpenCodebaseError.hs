-- | Open codebase error type.
module Unison.Codebase.Init.OpenCodebaseError
  ( OpenCodebaseError (..),
  )
where

import Unison.Prelude

-- | An error that can occur when attempting to open a codebase.
data OpenCodebaseError
  = -- | The codebase doesn't exist.
    OpenCodebaseDoesntExist
  | -- | The codebase exists, but its schema version is unknown to this application.
    OpenCodebaseUnknownSchemaVersion Word64
  deriving stock (Show)
