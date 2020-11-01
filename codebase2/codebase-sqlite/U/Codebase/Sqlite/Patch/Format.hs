module U.Codebase.Sqlite.Patch.Format where

import U.Codebase.Sqlite.Patch.Diff
import U.Codebase.Sqlite.Patch.Full
import U.Codebase.Sqlite.DbId (PatchObjectId)

data PatchFormat = Full Patch | Diff PatchObjectId PatchDiff
