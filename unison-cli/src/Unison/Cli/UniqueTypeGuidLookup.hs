-- | This module contains functionality related to computing a "unique type guid lookup" function, which resolves a
-- name to a unique type's GUID to reuse.
module Unison.Cli.UniqueTypeGuidLookup
  ( loadUniqueTypeGuid,
  )
where

import Control.Lens (unsnoc)
import Data.Foldable qualified as Foldable
import Data.Maybe (fromJust)
import U.Codebase.Branch qualified as Codebase.Branch
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ProjectPath (ProjectPath)
import Unison.Codebase.UniqueTypeGuidLookup qualified as Codebase
import Unison.Name (Name)
import Unison.NameSegment (NameSegment)
import Unison.Prelude
import Unison.Sqlite qualified as Sqlite

loadUniqueTypeGuid :: ProjectPath -> Name -> Sqlite.Transaction (Maybe Text)
loadUniqueTypeGuid currentPath name0 = do
  -- First, resolve the current path and the (probably/hopefully relative) name of the unique type to the full path
  -- to the unique type, plus its final distinguished name segment.
  let (branchPath, name) =
        name0
          & Path.fromName'
          & Path.resolve currentPath
          & Path.unabsolute
          & Path.toSeq
          & unsnoc
          -- This is safe because we were handed a Name, which can't be empty
          & fromJust

  -- Define an operation to load a branch by its full path from the root namespace.
  --
  -- This ought to probably lean somewhat on a cache (so long as the caller is aware of the cache, and discrads it at
  -- an appropriate time, such as after the current unison file finishes parsing).
  let loadBranchAtPath :: [NameSegment] -> Sqlite.Transaction (Maybe (Codebase.Branch.Branch Sqlite.Transaction))
      loadBranchAtPath =
        Codebase.getShallowBranchAtProjectPath

  Codebase.loadUniqueTypeGuid loadBranchAtPath (Foldable.toList @Seq branchPath) name
