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

type TempTermFormat = Term.SyncTermFormat' Text HashJWT

type TempDeclFormat = Decl.SyncDeclFormat' Text HashJWT

type TempPatchFormat = Patch.SyncPatchFormat' HashJWT Text Base32Hex HashJWT

type TempNamespaceFormat = Namespace.SyncBranchFormat' HashJWT Text HashJWT HashJWT (HashJWT, HashJWT)

type TempCausalFormat = Causal.SyncCausalFormat' HashJWT HashJWT
