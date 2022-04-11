module U.Codebase.Sqlite.TempEntity where

import qualified U.Codebase.Sqlite.Branch.Format as Namespace
import qualified U.Codebase.Sqlite.Decl.Format as Decl
import qualified U.Codebase.Sqlite.Patch.Format as Patch
import qualified U.Codebase.Sqlite.Term.Format as Term
import U.Util.Base32Hex (Base32Hex)
import Unison.Prelude

-- should just newtype this somewhere
type HashJWT = Text

data TempEntity
  = TC (Term.SyncTermFormat' Text HashJWT)
  | DC (Decl.SyncDeclFormat' Text HashJWT)
  | P (Patch.SyncPatchFormat' HashJWT Text Base32Hex HashJWT)
  | N (Namespace.SyncBranchFormat' HashJWT Text HashJWT HashJWT (HashJWT, HashJWT))

--  | C (Causal hash)
