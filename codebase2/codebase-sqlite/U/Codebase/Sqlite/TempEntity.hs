module U.Codebase.Sqlite.TempEntity where

import U.Codebase.Sqlite.Branch.Format qualified as Namespace
import U.Codebase.Sqlite.Causal qualified as Causal
import U.Codebase.Sqlite.Decl.Format qualified as Decl
import U.Codebase.Sqlite.Entity qualified as Entity
import U.Codebase.Sqlite.LocalIds (LocalIds')
import U.Codebase.Sqlite.Patch.Format qualified as Patch
import U.Codebase.Sqlite.Term.Format qualified as Term
import Unison.Hash32 (Hash32)
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
