module U.Codebase.Sqlite.TempEntity where

import qualified U.Codebase.Sqlite.Branch.Format as Namespace
import qualified U.Codebase.Sqlite.Causal as Causal
import qualified U.Codebase.Sqlite.Decl.Format as Decl
import qualified U.Codebase.Sqlite.Entity as Entity
import U.Codebase.Sqlite.LocalIds (LocalIds')
import qualified U.Codebase.Sqlite.Patch.Format as Patch
import qualified U.Codebase.Sqlite.Term.Format as Term
import U.Util.Hash32 (Hash32)
import Unison.Prelude

-- |
-- data TempEntity
--   = TC TempTermFormat
--   | DC TempDeclFormat
--   | N TempNamespaceFormat
--   | P TempPatchFormat
--   | C TempCausalFormat
type TempEntity =
  Entity.SyncEntity' Text Hash32 Hash32 Hash32 Hash32 Hash32 Hash32

type TempLocalIds = LocalIds' Text Hash32

type TempTermFormat = Term.SyncTermFormat' Text Hash32

type TempDeclFormat = Decl.SyncDeclFormat' Text Hash32

type TempPatchFormat = Patch.SyncPatchFormat' Hash32 Text Hash32 Hash32

type TempPatchLocalIds = Patch.PatchLocalIds' Text Hash32 Hash32

type TempNamespaceFormat = Namespace.SyncBranchFormat' Hash32 Text Hash32 Hash32 (Hash32, Hash32)

type TempNamespaceLocalIds = Namespace.BranchLocalIds' Text Hash32 Hash32 (Hash32, Hash32)

type TempCausalFormat = Causal.SyncCausalFormat' Hash32 Hash32
