-- | This module contains decoders for blobs stored in SQLite.
module U.Codebase.Sqlite.Decode
  ( DecodeError,

    -- * @object.bytes@
    decodeBranchFormat,
    decodeComponentLengthOnly,
    decodeDeclElement,
    decodeDeclFormat,
    decodePatchFormat,
    decodeSyncDeclFormat,
    decodeSyncNamespaceFormat,
    decodeSyncPatchFormat,
    decodeSyncTermFormat,
    decodeSyncTermAndType,
    decodeTermElementDiscardingTerm,
    decodeTermElementDiscardingType,
    decodeTermElementWithType,
    decodeTermFormat,

    -- * @temp_entity.blob@
    decodeTempCausalFormat,
    decodeTempDeclFormat,
    decodeTempNamespaceFormat,
    decodeTempPatchFormat,
    decodeTempTermFormat,

    -- * @watch_result.result@
    decodeWatchResultFormat,

    -- * unsyncs
    unsyncTermComponent,
    unsyncDeclComponent,
  )
where

import Control.Exception (throwIO)
import Data.Bytes.Get (runGetS)
import qualified Data.Bytes.Get as Get
import qualified U.Codebase.Reference as C.Reference
import qualified U.Codebase.Sqlite.Branch.Format as NamespaceFormat
import qualified U.Codebase.Sqlite.Decl.Format as DeclFormat
import U.Codebase.Sqlite.LocalIds (LocalIds)
import qualified U.Codebase.Sqlite.Patch.Format as PatchFormat
import U.Codebase.Sqlite.Serialization as Serialization
import U.Codebase.Sqlite.Symbol (Symbol)
import qualified U.Codebase.Sqlite.TempEntity as TempEntity
import qualified U.Codebase.Sqlite.Term.Format as TermFormat
import U.Util.Serialization (Get)
import qualified U.Util.Serialization as Serialization (lengthFramedArray)
import Unison.Prelude
import Unison.Sqlite

------------------------------------------------------------------------------------------------------------------------
-- Decode error

data DecodeError = DecodeError
  { decoder :: Text, -- the name of the decoder
    err :: String -- the error message
  }
  deriving stock (Show)
  deriving anyclass (SqliteExceptionReason, Exception)

getFromBytesOr :: Text -> Get a -> ByteString -> Either DecodeError a
getFromBytesOr decoder get bs = case runGetS get bs of
  Left err -> Left (DecodeError decoder err)
  Right a -> Right a

------------------------------------------------------------------------------------------------------------------------
-- object.bytes

{-# DEPRECATED decodeBranchFormat "load namespaces from sqlite relations instead. This is kept around for old migration code" #-}
decodeBranchFormat :: ByteString -> Either DecodeError NamespaceFormat.BranchFormat
decodeBranchFormat =
  getFromBytesOr "getBranchFormat" Serialization.getBranchFormat

decodeComponentLengthOnly :: ByteString -> Either DecodeError Word64
decodeComponentLengthOnly =
  getFromBytesOr "lengthFramedArray" (Get.skip 1 >> Serialization.lengthFramedArray)

decodeDeclElement :: Word64 -> ByteString -> Either DecodeError (LocalIds, DeclFormat.Decl Symbol)
decodeDeclElement i =
  getFromBytesOr ("lookupDeclElement " <> tShow i) (Serialization.lookupDeclElement i)

decodeDeclFormat :: ByteString -> Either DecodeError DeclFormat.DeclFormat
decodeDeclFormat =
  getFromBytesOr "getDeclFormat" Serialization.getDeclFormat

decodePatchFormat :: ByteString -> Either DecodeError PatchFormat.PatchFormat
decodePatchFormat =
  getFromBytesOr "getPatchFormat" Serialization.getPatchFormat

decodeSyncDeclFormat :: ByteString -> Either DecodeError DeclFormat.SyncDeclFormat
decodeSyncDeclFormat =
  getFromBytesOr "decomposeDeclFormat" Serialization.decomposeDeclFormat

decodeSyncNamespaceFormat :: ByteString -> Either DecodeError NamespaceFormat.SyncBranchFormat
decodeSyncNamespaceFormat =
  getFromBytesOr "decomposeNamespaceFormat" Serialization.decomposeBranchFormat

decodeSyncPatchFormat :: ByteString -> Either DecodeError PatchFormat.SyncPatchFormat
decodeSyncPatchFormat =
  getFromBytesOr "decomposePatchFormat" Serialization.decomposePatchFormat

decodeSyncTermFormat :: ByteString -> Either DecodeError TermFormat.SyncTermFormat
decodeSyncTermFormat =
  getFromBytesOr "decomposeTermFormat" Serialization.decomposeTermFormat

-- | N.B. The bytestring here is not the entire object.bytes column --
-- it's just the serialized term and type from 'TermFormat.SyncTermFormat'.
decodeSyncTermAndType :: ByteString -> Either DecodeError (TermFormat.Term, TermFormat.Type)
decodeSyncTermAndType =
  getFromBytesOr "getTermAndType" Serialization.getTermAndType

-- | N.B. The bytestring here is not the entire object.bytes column --
-- it's just the serialized decl from 'DeclFormat.SyncDeclFormat'.
decodeDecl :: ByteString -> Either DecodeError (DeclFormat.Decl Symbol)
decodeDecl =
  getFromBytesOr "getDeclElement" Serialization.getDeclElement

decodeTermFormat :: ByteString -> Either DecodeError TermFormat.TermFormat
decodeTermFormat =
  getFromBytesOr "getTermFormat" Serialization.getTermFormat

decodeTermElementDiscardingTerm :: C.Reference.Pos -> ByteString -> Either DecodeError (LocalIds, TermFormat.Type)
decodeTermElementDiscardingTerm i =
  getFromBytesOr ("lookupTermElementDiscardingTerm " <> tShow i) (Serialization.lookupTermElementDiscardingTerm i)

decodeTermElementDiscardingType :: C.Reference.Pos -> ByteString -> Either DecodeError (LocalIds, TermFormat.Term)
decodeTermElementDiscardingType i =
  getFromBytesOr ("lookupTermElementDiscardingType " <> tShow i) (Serialization.lookupTermElementDiscardingType i)

decodeTermElementWithType ::
  C.Reference.Pos ->
  ByteString ->
  Either DecodeError (LocalIds, TermFormat.Term, TermFormat.Type)
decodeTermElementWithType i =
  getFromBytesOr ("lookupTermElement" <> tShow i) (Serialization.lookupTermElement i)

------------------------------------------------------------------------------------------------------------------------
-- temp_entity.blob

decodeTempCausalFormat :: ByteString -> Either DecodeError TempEntity.TempCausalFormat
decodeTempCausalFormat =
  getFromBytesOr "getTempCausalFormat" Serialization.getTempCausalFormat

decodeTempDeclFormat :: ByteString -> Either DecodeError TempEntity.TempDeclFormat
decodeTempDeclFormat =
  getFromBytesOr "getTempDeclFormat" Serialization.getTempDeclFormat

decodeTempNamespaceFormat :: ByteString -> Either DecodeError TempEntity.TempNamespaceFormat
decodeTempNamespaceFormat =
  getFromBytesOr "getTempNamespaceFormat" Serialization.getTempNamespaceFormat

decodeTempPatchFormat :: ByteString -> Either DecodeError TempEntity.TempPatchFormat
decodeTempPatchFormat =
  getFromBytesOr "getTempPatchFormat" Serialization.getTempPatchFormat

decodeTempTermFormat :: ByteString -> Either DecodeError TempEntity.TempTermFormat
decodeTempTermFormat =
  getFromBytesOr "getTempTermFormat" Serialization.getTempTermFormat

------------------------------------------------------------------------------------------------------------------------
-- watch_result.result

decodeWatchResultFormat :: ByteString -> Either DecodeError TermFormat.WatchResultFormat
decodeWatchResultFormat =
  getFromBytesOr "getWatchResultFormat" Serialization.getWatchResultFormat

------------------------------------------------------------------------------------------------------------------------
-- unsyncs

unsyncTermComponent :: TermFormat.SyncLocallyIndexedComponent' t d -> IO (TermFormat.LocallyIndexedComponent' t d)
unsyncTermComponent (TermFormat.SyncLocallyIndexedComponent terms) = do
  let phi (localIds, bs) = do
        (a, b) <- decodeSyncTermAndType bs
        pure (localIds, a, b)
  case traverse phi terms of
    Left err -> throwIO err
    Right x -> pure (TermFormat.LocallyIndexedComponent x)

unsyncDeclComponent :: DeclFormat.SyncLocallyIndexedComponent' t d -> IO (DeclFormat.LocallyIndexedComponent' t d)
unsyncDeclComponent (DeclFormat.SyncLocallyIndexedComponent decls) = do
  let phi (localIds, bs) = do
        decl <- decodeDecl bs
        pure (localIds, decl)
  case traverse phi decls of
    Left err -> throwIO err
    Right x -> pure (DeclFormat.LocallyIndexedComponent x)
