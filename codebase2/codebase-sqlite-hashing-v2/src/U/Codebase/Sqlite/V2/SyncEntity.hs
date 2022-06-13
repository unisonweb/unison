module U.Codebase.Sqlite.V2.SyncEntity where

import Data.Bytes.Put (runPutS)
import qualified U.Codebase.Decl as V2.Decl
import qualified U.Codebase.Sqlite.Causal as Sqlite.Causal
import qualified U.Codebase.Sqlite.DbId as Db
import qualified U.Codebase.Sqlite.Decl.Format as DeclFormat
import U.Codebase.Sqlite.Decode (unsyncDeclComponent, unsyncTermComponent)
import U.Codebase.Sqlite.Entity (SyncEntity)
import qualified U.Codebase.Sqlite.Entity as Entity
import qualified U.Codebase.Sqlite.ObjectType as ObjectType
import qualified U.Codebase.Sqlite.Operations as Ops
import qualified U.Codebase.Sqlite.Queries as Q
import qualified U.Codebase.Sqlite.Serialization as Serialization
import U.Codebase.Sqlite.Symbol (Symbol)
import qualified U.Codebase.Sqlite.Term.Format as TermFormat
import U.Codebase.Sqlite.V2.Decl (saveDeclComponent)
import U.Codebase.Sqlite.V2.Term (saveTermComponent)
import qualified U.Codebase.Term as V2
import U.Util.Hash32 (Hash32)
import qualified U.Util.Hash32 as Hash32
import Unison.Prelude
import Unison.Sqlite

saveSyncEntity :: Hash32 -> SyncEntity -> Transaction (Either Db.CausalHashId Db.ObjectId)
saveSyncEntity hash entity = do
  case entity of
    Entity.TC stf -> do
      lic :: TermFormat.LocallyIndexedComponent <- do
        let TermFormat.SyncTerm x = stf
        unsafeIO (unsyncTermComponent x)

      tc :: [(V2.Term Symbol, V2.Type Symbol)] <-
        traverse
          (\(a, b, c) -> Ops.s2cTermWithType a b c)
          (toList $ TermFormat.unLocallyIndexedComponent lic)
      let bytes = runPutS (Serialization.recomposeTermFormat stf)
      objId <- saveTermComponent (Just bytes) (Hash32.toHash hash) tc
      pure (Right objId)
    Entity.DC sdf -> do
      lic :: DeclFormat.LocallyIndexedComponent <- do
        let DeclFormat.SyncDecl (DeclFormat.SyncLocallyIndexedComponent xs) = sdf
        unsafeIO (unsyncDeclComponent xs)

      dc :: [V2.Decl.Decl Symbol] <-
        traverse
          (\(localIds, decl) -> Ops.s2cDecl localIds decl)
          (toList $ DeclFormat.unLocallyIndexedComponent lic)

      let bytes = runPutS (Serialization.recomposeDeclFormat sdf)
      objId <- saveDeclComponent (Just bytes) (Hash32.toHash hash) dc

      pure (Right objId)
    Entity.N sbf -> do
      hashId <- Q.saveHash hash
      let bytes = runPutS (Serialization.recomposeBranchFormat sbf)
      Right <$> Q.saveObject hashId ObjectType.Namespace bytes
    Entity.P spf -> do
      hashId <- Q.saveHash hash
      let bytes = runPutS (Serialization.recomposePatchFormat spf)
      Right <$> Q.saveObject hashId ObjectType.Patch bytes
    Entity.C scf -> case scf of
      Sqlite.Causal.SyncCausalFormat {valueHash, parents} -> do
        hashId <- Q.saveHash hash
        let causalHashId = Db.CausalHashId hashId
        Q.saveCausal causalHashId valueHash (toList parents)
        pure $ Left causalHashId
