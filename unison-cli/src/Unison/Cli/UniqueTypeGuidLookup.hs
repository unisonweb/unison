-- | This module contains functionality related to computing a "unique type guid lookup" function, which resolves a
-- name to a unique type's GUID to reuse.
module Unison.Cli.UniqueTypeGuidLookup
  ( loadUniqueTypeGuid,
  )
where

import U.Codebase.Branch qualified as Codebase.Branch
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ProjectPath (ProjectPath)
import Unison.Codebase.ProjectPath qualified as PP
import Unison.Codebase.UniqueTypeGuidLookup qualified as Codebase
import Unison.Name (Name)
import Unison.Prelude
import Unison.Sqlite qualified as Sqlite

loadUniqueTypeGuid :: ProjectPath -> Name -> Sqlite.Transaction (Maybe Text)
loadUniqueTypeGuid pp name0 = do
  let (namePath, finalSegment) = Path.splitFromName name0
  let fullPP = pp & over PP.path_ (<> namePath)

  -- Define an operation to load a branch by its full path from the root namespace.
  --
  -- This ought to probably lean somewhat on a cache (so long as the caller is aware of the cache, and discrads it at
  -- an appropriate time, such as after the current unison file finishes parsing).
  let loadBranchAtPath :: ProjectPath -> Sqlite.Transaction (Maybe (Codebase.Branch.Branch Sqlite.Transaction))
      loadBranchAtPath = Codebase.getMaybeShallowBranchAtProjectPath

  Codebase.loadUniqueTypeGuid loadBranchAtPath fullPP finalSegment
