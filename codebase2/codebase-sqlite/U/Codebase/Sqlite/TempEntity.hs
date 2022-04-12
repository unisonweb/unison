module U.Codebase.Sqlite.TempEntity where

import Data.Vector (Vector)
import qualified U.Codebase.Sqlite.Branch.Format as Namespace
import qualified U.Codebase.Sqlite.Decl.Format as Decl
import qualified U.Codebase.Sqlite.Patch.Format as Patch
import qualified U.Codebase.Sqlite.Term.Format as Term
import U.Util.Base32Hex (Base32Hex)
import Unison.Prelude

-- should just newtype this somewhere
type HashJWT = Text

data TempEntity
  = TC TempTermFormat
  | DC TempDeclFormat
  | P TempPatchFormat
  | N TempNamespaceFormat
  | C TempCausalFormat

type TempTermFormat = Term.SyncTermFormat' Text HashJWT

type TempDeclFormat = Decl.SyncDeclFormat' Text HashJWT

type TempPatchFormat = Patch.SyncPatchFormat' HashJWT Text Base32Hex HashJWT

type TempNamespaceFormat = Namespace.SyncBranchFormat' HashJWT Text HashJWT HashJWT (HashJWT, HashJWT)

type TempCausalFormat = SyncCausalFormat' HashJWT HashJWT

data SyncCausalFormat' causalHash namespaceHash = SyncCausalFormat {valueHash :: namespaceHash, parents :: Vector causalHash}
