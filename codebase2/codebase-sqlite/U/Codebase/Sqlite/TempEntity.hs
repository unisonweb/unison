module U.Codebase.Sqlite.TempEntity where

import qualified U.Codebase.Sqlite.Branch.Format as Namespace
import qualified U.Codebase.Sqlite.Causal as Causal
import qualified U.Codebase.Sqlite.Decl.Format as Decl
import qualified U.Codebase.Sqlite.Patch.Format as Patch
import U.Codebase.Sqlite.TempEntityType (TempEntityType (..))
import qualified U.Codebase.Sqlite.Term.Format as Term
import U.Util.Base32Hex (Base32Hex)
import Unison.Prelude

-- should just newtype this somewhere
type HashJWT = Text

data TempEntity
  = TC TempTermFormat
  | DC TempDeclFormat
  | N TempNamespaceFormat
  | P TempPatchFormat
  | C TempCausalFormat

tempEntityType :: TempEntity -> TempEntityType
tempEntityType = \case
  TC _ -> TermComponentType
  DC _ -> DeclComponentType
  N _ -> NamespaceType
  P _ -> PatchType
  C _ -> CausalType

type TempTermFormat = Term.SyncTermFormat' Text Base32Hex

type TempDeclFormat = Decl.SyncDeclFormat' Text Base32Hex

type TempPatchFormat = Patch.SyncPatchFormat' Base32Hex Text Base32Hex Base32Hex

type TempPatchLocalIds = Patch.PatchLocalIds' Text Base32Hex Base32Hex

type TempNamespaceFormat = Namespace.SyncBranchFormat' Base32Hex Text Base32Hex Base32Hex (Base32Hex, Base32Hex)

type TempNamespaceLocalIds = Namespace.BranchLocalIds' Text Base32Hex Base32Hex (Base32Hex, Base32Hex)

type TempCausalFormat = Causal.SyncCausalFormat' Base32Hex Base32Hex
