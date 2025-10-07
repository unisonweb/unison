module Unison.SyncV3.Types
  ( InitMsg (..),
    EntityRequestMsg (..),
    FromReceiverMessage (..),
    FromEmitterMessage (..),
    MsgOrError (..),
    SyncError (..),
    Entity (..),
    EntityKind (..),
    EntityDepth (..),
    HashTag (..),
    BranchRef (..),
  )
where

import Codec.Serialise (Serialise)
import Codec.Serialise qualified as CBOR
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Int (Int32, Int64)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Network.WebSockets (WebSocketsData)
import Network.WebSockets qualified as WS
import U.Codebase.Sqlite.Orphans ()
import U.Codebase.Sqlite.TempEntity
import Unison.Hash32 (Hash32)
import Unison.Prelude (tShow)
import Unison.Server.Orphans ()
import Unison.Sqlite qualified as Sqlite
import Unison.SyncCommon.Types
import Unison.Util.Servant.CBOR qualified as CBOR

data InitMsg authedHash = InitMsg
  { initMsgClientVersion :: Int32,
    initMsgBranchRef :: BranchRef,
    initMsgRootCausal :: authedHash,
    initMsgRequestedDepth :: Maybe Int64
  }
  deriving (Show, Eq)

instance (ToJSON authedHash) => ToJSON (InitMsg authedHash) where
  toJSON (InitMsg {initMsgClientVersion, initMsgBranchRef, initMsgRootCausal, initMsgRequestedDepth}) =
    object
      [ "clientVersion" .= initMsgClientVersion,
        "branchRef" .= initMsgBranchRef,
        "rootCausal" .= initMsgRootCausal,
        "requestedDepth" .= initMsgRequestedDepth
      ]

instance (FromJSON authedHash) => FromJSON (InitMsg authedHash) where
  parseJSON = withObject "InitMsg" $ \o ->
    InitMsg
      <$> o .: "clientVersion"
      <*> o .: "branchRef"
      <*> o .: "rootCausal"
      <*> o .:? "requestedDepth"

data EntityRequestMsg hash = EntityRequestMsg
  { hashes :: [(EntityKind, hash)]
  }
  deriving (Show, Eq)

-- | Roundtrip test:
-- >>> import qualified Codec.Serialise as CBOR
-- >>> let msg = EntityRequestMsg {hashes = [(CausalEntity, "hash1"), (NamespaceEntity, "hash2")]}
-- >>> CBOR.deserialise (CBOR.serialise msg) == msg
-- True
instance (CBOR.Serialise sh) => CBOR.Serialise (EntityRequestMsg sh) where
  encode (EntityRequestMsg {hashes}) =
    CBOR.encode hashes

  decode = do
    hashes <- CBOR.decode @[(EntityKind, sh)]
    pure $ EntityRequestMsg {hashes}

data FromReceiverMessageTag
  = ReceiverInitStreamTag
  | ReceiverEntityRequestTag
  deriving (Show, Eq)

-- | Roundtrip test:
-- >>> import qualified Codec.Serialise as CBOR
-- >>> CBOR.deserialise (CBOR.serialise ReceiverInitStreamTag) == ReceiverInitStreamTag
-- True
-- >>> CBOR.deserialise (CBOR.serialise ReceiverEntityRequestTag) == ReceiverEntityRequestTag
-- True
instance CBOR.Serialise FromReceiverMessageTag where
  encode = \case
    ReceiverInitStreamTag -> CBOR.encode (0 :: Int)
    ReceiverEntityRequestTag -> CBOR.encode (1 :: Int)

  decode = do
    tag <- CBOR.decode @Int
    case tag of
      0 -> pure ReceiverInitStreamTag
      1 -> pure ReceiverEntityRequestTag
      _ -> fail $ "Unknown FromReceiverMessageTag: " <> show tag

-- A message sent from the downloader to the emitter.
data FromReceiverMessage ah hash
  = -- Initialize the stream
    ReceiverInitStream (InitMsg ah)
  | -- Request more entities by hash.
    ReceiverEntityRequest (EntityRequestMsg hash)
  deriving (Show, Eq)

instance (ToJSON ah, FromJSON ah) => CBOR.Serialise (InitMsg ah) where
  encode msg = do
    -- This is dumb, but there's currently no reasonable way to encode a heterogenous Map
    -- using Haskell's CBOR library :|
    --
    -- See https://github.com/well-typed/cborg/issues/369
    CBOR.encode @BS.ByteString $ BL.toStrict $ Aeson.encode msg

  decode = do
    bs <- CBOR.decode @BS.ByteString
    case Aeson.eitherDecode $ BL.fromStrict bs of
      Left err -> fail $ "Error decoding InitMsg from JSON: " <> err
      Right msg -> pure msg

-- | Roundtrip test:
-- >>> import qualified Codec.Serialise as CBOR
-- >>> let msg = InitMsg {initMsgClientVersion = 1, initMsgBranchRef = BranchRef "main", initMsgRootCausal = "hash123", initMsgRequestedDepth = Just 10}
-- >>> CBOR.deserialise (CBOR.serialise msg) == msg
-- True
-- >>> let initMsg :: FromReceiverMessage Text Text = ReceiverInitStream msg
-- >>> CBOR.deserialise (CBOR.serialise initMsg) == initMsg
-- True
-- >>> let entityReq :: FromReceiverMessage Text Text = ReceiverEntityRequest (EntityRequestMsg {hashes = [(CausalEntity, "h1")]})
-- >>> CBOR.deserialise (CBOR.serialise entityReq) == entityReq
-- True
instance (CBOR.Serialise h, ToJSON ah, FromJSON ah) => CBOR.Serialise (FromReceiverMessage ah h) where
  encode = \case
    ReceiverInitStream initMsg ->
      CBOR.encode ReceiverInitStreamTag
        <> CBOR.encode initMsg
    ReceiverEntityRequest msg ->
      CBOR.encode ReceiverEntityRequestTag
        <> CBOR.encode msg
  decode = do
    tag <- CBOR.decode @FromReceiverMessageTag
    case tag of
      ReceiverInitStreamTag -> ReceiverInitStream <$> CBOR.decode @(InitMsg ah)
      ReceiverEntityRequestTag -> ReceiverEntityRequest <$> CBOR.decode @(EntityRequestMsg h)

data SyncError
  = InitializationError Text
  | UnexpectedMessage BL.ByteString
  | EncodingFailure Text
  | -- The caller asked for a Hash they shouldn't have access to.
    ForbiddenEntityRequest (Set (EntityKind, Hash32))
  | ConnectionError Text
  | ProjectNotFound BranchRef
  | UserNotFound BranchRef
  | NoReadPermission BranchRef
  | HashJWTVerificationError Text
  | InvalidBranchRef Text BranchRef
  deriving (Show, Eq)

-- | Roundtrip test:
-- >>> import qualified Codec.Serialise as CBOR
-- >>> import qualified Data.Set as Set
-- >>> CBOR.deserialise (CBOR.serialise (InitializationError "test")) == InitializationError "test"
-- True
-- >>> CBOR.deserialise (CBOR.serialise (EncodingFailure "fail")) == EncodingFailure "fail"
-- True
-- >>> let forbidden = ForbiddenEntityRequest (Set.fromList [(CausalEntity, undefined)])
-- >>> CBOR.deserialise (CBOR.serialise (ConnectionError "err")) == ConnectionError "err"
-- True
instance CBOR.Serialise SyncError where
  encode = \case
    InitializationError msg ->
      CBOR.encode (0 :: Int) <> CBOR.encode msg
    UnexpectedMessage msg ->
      CBOR.encode (1 :: Int) <> CBOR.encode (BL.toStrict msg)
    EncodingFailure msg ->
      CBOR.encode (2 :: Int) <> CBOR.encode msg
    ForbiddenEntityRequest hashes ->
      CBOR.encode (3 :: Int) <> CBOR.encode hashes
    ConnectionError err ->
      CBOR.encode (4 :: Int) <> CBOR.encode err
    ProjectNotFound branchRef ->
      CBOR.encode (5 :: Int) <> CBOR.encode branchRef
    UserNotFound branchRef ->
      CBOR.encode (6 :: Int) <> CBOR.encode branchRef
    NoReadPermission branchRef ->
      CBOR.encode (7 :: Int) <> CBOR.encode branchRef
    HashJWTVerificationError err ->
      CBOR.encode (8 :: Int) <> CBOR.encode err
    InvalidBranchRef err branchRef ->
      CBOR.encode (9 :: Int) <> CBOR.encode err <> CBOR.encode branchRef

  decode = do
    tag <- CBOR.decode @Int
    case tag of
      0 -> InitializationError <$> CBOR.decode
      1 -> do
        bs <- CBOR.decode @BS.ByteString
        pure $ UnexpectedMessage (BL.fromStrict bs)
      2 -> EncodingFailure <$> CBOR.decode
      3 -> ForbiddenEntityRequest . Set.fromList <$> CBOR.decode
      4 -> do
        err <- CBOR.decode @Text
        pure $ ConnectionError err
      5 -> ProjectNotFound <$> CBOR.decode
      6 -> UserNotFound <$> CBOR.decode
      7 -> NoReadPermission <$> CBOR.decode
      8 -> HashJWTVerificationError <$> CBOR.decode
      9 -> do
        err <- CBOR.decode @Text
        branchRef <- CBOR.decode @BranchRef
        pure $ InvalidBranchRef err branchRef
      _ -> fail $ "Unknown SyncError tag: " <> show tag

-- A message sent from the emitter to the downloader.
data FromEmitterMessage hash text
  = EmitterEntityMsg (Entity hash text)
  deriving (Show, Eq)

data EntityKind
  = CausalEntity
  | NamespaceEntity
  | DefnComponentEntity
  | PatchEntity
  deriving stock (Show, Eq, Ord)

instance Sqlite.ToField EntityKind where
  toField =
    Sqlite.toField . \case
      CausalEntity -> (0 :: Int)
      NamespaceEntity -> 1
      DefnComponentEntity -> 2
      PatchEntity -> 3

instance Sqlite.FromField EntityKind where
  fromField field = do
    tag <- Sqlite.fromField field
    case tag of
      (0 :: Int) -> pure CausalEntity
      1 -> pure NamespaceEntity
      2 -> pure DefnComponentEntity
      3 -> pure PatchEntity
      _ -> fail $ "Unknown EntityKind tag: " <> show tag

-- | Roundtrip test:
-- >>> import qualified Codec.Serialise as CBOR
-- >>> CBOR.deserialise (CBOR.serialise CausalEntity) == CausalEntity
-- True
-- >>> CBOR.deserialise (CBOR.serialise NamespaceEntity) == NamespaceEntity
-- True
-- >>> CBOR.deserialise (CBOR.serialise DefnComponentEntity) == DefnComponentEntity
-- True
-- >>> CBOR.deserialise (CBOR.serialise PatchEntity) == PatchEntity
-- True
instance CBOR.Serialise EntityKind where
  encode = \case
    CausalEntity -> CBOR.encode (0 :: Int)
    NamespaceEntity -> CBOR.encode (1 :: Int)
    DefnComponentEntity -> CBOR.encode (2 :: Int)
    PatchEntity -> CBOR.encode (3 :: Int)

  decode = do
    tag <- CBOR.decode @Int
    case tag of
      0 -> pure CausalEntity
      1 -> pure NamespaceEntity
      2 -> pure DefnComponentEntity
      3 -> pure PatchEntity
      _ -> fail $ "Unknown EntityKind tag: " <> show tag

-- | The number of _levels_ of dependencies an entity has,
-- this has no real semantic meaning on its own, but provides the
-- property that out of a given set of synced entities, if you process
-- them in order of increasing EntityDepth, you will always have
-- processed an entity's dependencies before you see the entity itself.
newtype EntityDepth = EntityDepth {unEntityDepth :: Int64}
  deriving (Show, Eq, Ord)
  deriving newtype (CBOR.Serialise)

data Entity hash text = Entity
  { entityHash :: hash,
    entityKind :: EntityKind,
    entityDepth :: EntityDepth,
    entityData :: CBOR.CBORBytes TempEntity
  }
  deriving (Show, Eq)

-- | Roundtrip test:
-- >>> import qualified Codec.Serialise as CBOR
-- >>> import U.Codebase.Sqlite.TempEntity (TempEntity(..))
-- >>> let ent :: Entity Text Text = Entity {entityHash = "hash", entityKind = CausalEntity, entityDepth = EntityDepth 5, entityData = CBOR.CBORBytes "abc"}
-- >>> CBOR.deserialise (CBOR.serialise ent) == ent
-- True
instance (CBOR.Serialise smallHash, CBOR.Serialise text) => CBOR.Serialise (Entity smallHash text) where
  encode (Entity {entityHash, entityKind, entityDepth, entityData}) =
    CBOR.encode entityHash
      <> CBOR.encode entityKind
      <> CBOR.encode entityDepth
      <> CBOR.encode entityData

  decode = do
    entityHash <- CBOR.decode @smallHash
    entityKind <- CBOR.decode @EntityKind
    entityDepth <- CBOR.decode @EntityDepth
    entityData <- CBOR.decode @(CBOR.CBORBytes TempEntity)

    pure $ Entity {entityHash, entityKind, entityData, entityDepth}

-- | Roundtrip test:
-- >>> import qualified Codec.Serialise as CBOR
-- >>> import U.Codebase.Sqlite.TempEntity (TempEntity(..))
-- >>> let ent :: Entity Text Text = Entity {entityHash = "hash", entityKind = CausalEntity, entityDepth = EntityDepth 5, entityData = CBOR.CBORBytes "abc"}
-- >>> let msg = EmitterEntityMsg ent
-- >>> CBOR.deserialise (CBOR.serialise msg) == msg
-- True
instance (CBOR.Serialise hash, CBOR.Serialise text) => CBOR.Serialise (FromEmitterMessage hash text) where
  encode = \case
    EmitterEntityMsg msg -> CBOR.encode EmitterEntityTag <> CBOR.encode msg

  decode = do
    tag <- CBOR.decode @FromEmitterMessageTag
    case tag of
      EmitterEntityTag -> EmitterEntityMsg <$> CBOR.decode

data FromEmitterMessageTag
  = EmitterEntityTag
  deriving (Show, Eq)

-- | Roundtrip test:
-- >>> import qualified Codec.Serialise as CBOR
-- >>> CBOR.deserialise (CBOR.serialise EmitterEntityTag) == EmitterEntityTag
-- True
instance CBOR.Serialise FromEmitterMessageTag where
  encode = \case
    EmitterEntityTag -> CBOR.encode (0 :: Int)

  decode = do
    tag <- CBOR.decode @Int
    case tag of
      0 -> pure EmitterEntityTag
      _ -> fail $ "Unknown FromEmitterMessageTag: " <> show tag

data MsgOrError err a
  = Msg a
  | Err err
  deriving (Show, Eq, Ord)

-- | Roundtrip test:
-- >>> import qualified Codec.Serialise as CBOR
-- >>> CBOR.deserialise (CBOR.serialise (Msg "test" :: MsgOrError Text Text)) == Msg "test"
-- True
-- >>> CBOR.deserialise (CBOR.serialise (Err "error" :: MsgOrError Text Text)) == Err "error"
-- True
instance (CBOR.Serialise a, CBOR.Serialise err) => CBOR.Serialise (MsgOrError err a) where
  encode = \case
    Msg a -> CBOR.encode (0 :: Int) <> CBOR.encode a
    Err e -> CBOR.encode (1 :: Int) <> CBOR.encode e

  decode = do
    tag <- CBOR.decode @Int
    case tag of
      0 -> Msg <$> CBOR.decode
      1 -> Err <$> CBOR.decode
      _ -> fail $ "Unknown MsgOrError tag: " <> show tag

-- | Roundtrip test:
-- >>> import qualified Network.WebSockets as WS
-- >>> let msgVal = Msg "test" :: MsgOrError SyncError Text
-- >>> WS.fromLazyByteString (WS.toLazyByteString msgVal) == msgVal
-- True
-- >>> let errVal = Err (InitializationError "init error") :: MsgOrError SyncError Text
-- >>> WS.fromLazyByteString (WS.toLazyByteString errVal) == errVal
-- True
-- >>> let dataMsg = WS.Binary (WS.toLazyByteString msgVal)
-- >>> WS.fromDataMessage dataMsg == msgVal
-- True
instance (Serialise msg) => WebSocketsData (MsgOrError SyncError msg) where
  fromLazyByteString bytes =
    CBOR.deserialiseOrFail bytes
      & either (\err -> Err . EncodingFailure $ "Error decoding CBOR message from bytes: " <> tShow err) id

  toLazyByteString = CBOR.serialise

  fromDataMessage dm = do
    case dm of
      WS.Text bytes _ -> WS.fromLazyByteString bytes
      WS.Binary bytes -> WS.fromLazyByteString bytes

-- Application level compression of Hash references.
-- We can send a mapping of Hash <-> HashTag at the start of the stream,
-- and then use the smaller HashTag in all subsequent messages.
data HashTag = HashTag (EntityKind, Int64)
  deriving (Show, Eq, Ord)

-- | Roundtrip test:
-- >>> import qualified Codec.Serialise as CBOR
-- >>> let tag = HashTag (CausalEntity, 42)
-- >>> CBOR.deserialise (CBOR.serialise tag) == tag
-- True
instance CBOR.Serialise HashTag where
  encode (HashTag (kind, idx)) =
    CBOR.encode (kind, idx)

  decode = do
    (kind, idx) <- CBOR.decode @(EntityKind, Int64)
    pure $ HashTag (kind, idx)
