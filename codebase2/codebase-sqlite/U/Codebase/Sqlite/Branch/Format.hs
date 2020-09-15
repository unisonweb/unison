module U.Codebase.Sqlite.Branch.Format where

import U.Codebase.Sqlite.Branch.Full
import U.Codebase.Sqlite.Branch.Diff

data BranchFormat 
  = Full Branch 
  | Diff Diff