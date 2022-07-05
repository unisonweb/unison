module U.Codebase.Sqlite.Entity where

import qualified U.Codebase.Sqlite.Branch.Format as Namespace
import qualified U.Codebase.Sqlite.Causal as Causal
import U.Codebase.Sqlite.DbId (BranchHashId, BranchObjectId, CausalHashId, HashId, ObjectId, PatchObjectId, TextId)
import qualified U.Codebase.Sqlite.Decl.Format as Decl
import qualified U.Codebase.Sqlite.Patch.Format as Patch
import U.Codebase.Sqlite.TempEntityType (TempEntityType (..))
import qualified U.Codebase.Sqlite.Term.Format as Term

-- |
-- data SyncEntity
--   = TC SyncTermFormat
--   | DC SyncDeclFormat
--   | N SyncBranchFormat
--   | P SyncPatchFormat
--   | C SyncCausalFormat
type SyncEntity =
  SyncEntity' TextId HashId ObjectId PatchObjectId BranchHashId BranchObjectId CausalHashId

data SyncEntity' text hash defn patch branchh branch causal
  = TC (Term.SyncTermFormat' text defn)
  | DC (Decl.SyncDeclFormat' text defn)
  | N (Namespace.SyncBranchFormat' branch text defn patch (branch, causal))
  | P (Patch.SyncPatchFormat' patch text hash defn)
  | C (Causal.SyncCausalFormat' causal branchh)

entityType :: SyncEntity' text hash defn patch branchh branch causal -> TempEntityType
entityType = \case
  TC _ -> TermComponentType
  DC _ -> DeclComponentType
  N _ -> NamespaceType
  P _ -> PatchType
  C _ -> CausalType
