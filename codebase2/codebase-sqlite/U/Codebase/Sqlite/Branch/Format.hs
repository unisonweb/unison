module U.Codebase.Sqlite.Branch.Format where

import U.Codebase.Sqlite.Branch.Diff ( Diff )
import U.Codebase.Sqlite.Branch.Full ( Branch )
import U.Codebase.Sqlite.DbId (BranchObjectId)

data BranchFormat = Full Branch | Diff BranchObjectId Diff
