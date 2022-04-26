-- | This module contains decoders for blobs stored in SQLite.
module U.Codebase.Sqlite.Decode
  ( DecodeError,

    -- * @object.bytes@
    decodeBranchFormat,
    decodeComponentLengthOnly,
    decodeDeclElement,
    decodeDeclFormat,
    decodePatchFormat,
    decodeTermFormat,
    decodeTermElementDiscardingTerm,
    decodeTermElementDiscardingType,
    decodeTermElementWithType,

    -- * @temp_entity.blob@
    decodeTempCausalFormat,
    decodeTempDeclFormat,
    decodeTempNamespaceFormat,
    decodeTempPatchFormat,
    decodeTempTermFormat,

    -- * @watch_result.result@
    decodeWatchResultFormat,
  )
where

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
  deriving anyclass (SqliteExceptionReason)

getFromBytesOr :: Text -> Get a -> ByteString -> Either DecodeError a
getFromBytesOr decoder get bs = case runGetS get bs of
  Left err -> Left (DecodeError decoder err)
  Right a -> Right a

------------------------------------------------------------------------------------------------------------------------
-- object.bytes

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
