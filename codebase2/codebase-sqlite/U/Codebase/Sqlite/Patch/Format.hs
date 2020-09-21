module U.Codebase.Sqlite.Patch.Format where

import U.Codebase.Sqlite.Patch.Diff
import U.Codebase.Sqlite.Patch.Full

data PatchFormat = Full Patch | Diff PatchDiff
