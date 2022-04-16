module U.Codebase.Sqlite.ReadyEntity where

import qualified U.Codebase.Sqlite.Branch.Format as Namespace
import qualified U.Codebase.Sqlite.Causal as Causal
import U.Codebase.Sqlite.DbId (BranchHashId, CausalHashId)
import qualified U.Codebase.Sqlite.Decl.Format as Decl
import qualified U.Codebase.Sqlite.Patch.Format as Patch
import qualified U.Codebase.Sqlite.Term.Format as Term

data ReadyEntity
  = TC Term.SyncTermFormat
  | DC Decl.SyncDeclFormat
  | N Namespace.SyncBranchFormat
  | P Patch.SyncPatchFormat
  | C (Causal.SyncCausalFormat' CausalHashId BranchHashId)
