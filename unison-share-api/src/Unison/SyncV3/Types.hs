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
    HashMappings (..),
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
import Data.Map (Map)
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

instance (CBOR.Serialise sh) => CBOR.Serialise (EntityRequestMsg sh) where
  encode (EntityRequestMsg {hashes}) =
    CBOR.encode hashes

  decode = do
    hashes <- CBOR.decode @[(EntityKind, sh)]
    pure $ EntityRequestMsg {hashes}

data FromReceiverMessageTag
  = ReceiverInitStreamTag
  | ReceiverEntityRequestTag
  | ReceiverDoneTag

instance CBOR.Serialise FromReceiverMessageTag where
  encode = \case
    ReceiverInitStreamTag -> CBOR.encode (0 :: Int)
    ReceiverEntityRequestTag -> CBOR.encode (1 :: Int)
    ReceiverDoneTag -> CBOR.encode (2 :: Int)

  decode = do
    tag <- CBOR.decode @Int
    case tag of
      0 -> pure ReceiverInitStreamTag
      1 -> pure ReceiverEntityRequestTag
      2 -> pure ReceiverDoneTag
      _ -> fail $ "Unknown FromReceiverMessageTag: " <> show tag

-- A message sent from the downloader to the emitter.
data FromReceiverMessage ah hash
  = -- Initialize the stream
    ReceiverInitStream (InitMsg ah)
  | -- Request more entities by hash.
    ReceiverEntityRequest (EntityRequestMsg hash)
  | -- Sent when the receiver has no outstanding requests.
    ReceiverDone
  deriving (Show, Eq)

instance (ToJSON ah, FromJSON ah) => CBOR.Serialise (InitMsg ah) where
  encode msg = do
    -- This is dumb, but there's currently no reasonable way to encode a heterogenous Map
    -- using Haskell's CBOR library :|
    --
    -- See https://github.com/well-typed/cborg/issues/369
    CBOR.encode $ Aeson.encode msg

  decode = do
    bs <- CBOR.decode @BL.ByteString
    case Aeson.eitherDecode bs of
      Left err -> fail $ "Error decoding InitMsg from JSON: " <> err
      Right msg -> pure msg

instance (CBOR.Serialise h, ToJSON ah, FromJSON ah) => CBOR.Serialise (FromReceiverMessage ah h) where
  encode = \case
    ReceiverInitStream initMsg ->
      CBOR.encode ReceiverInitStreamTag
        <> CBOR.encode initMsg
    ReceiverEntityRequest msg ->
      CBOR.encode ReceiverEntityRequestTag
        <> CBOR.encode msg
    ReceiverDone -> CBOR.encode ReceiverDoneTag
  decode = do
    tag <- CBOR.decode @FromReceiverMessageTag
    case tag of
      ReceiverInitStreamTag -> ReceiverInitStream <$> CBOR.decode @(InitMsg ah)
      ReceiverEntityRequestTag -> ReceiverEntityRequest <$> CBOR.decode @(EntityRequestMsg h)
      ReceiverDoneTag -> pure ReceiverDone

data SyncError
  = InitializationError Text
  | UnexpectedMessage BL.ByteString
  | EncodingFailure Text
  | -- The caller asked for a Hash they shouldn't have access to.
    ForbiddenEntityRequest (Set (EntityKind, Hash32))

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

  decode = do
    tag <- CBOR.decode @Int
    case tag of
      0 -> InitializationError <$> CBOR.decode
      1 -> do
        bs <- CBOR.decode @BS.ByteString
        pure $ UnexpectedMessage (BL.fromStrict bs)
      2 -> EncodingFailure <$> CBOR.decode
      3 -> ForbiddenEntityRequest . Set.fromList <$> CBOR.decode
      _ -> fail $ "Unknown SyncError tag: " <> show tag

-- A message sent from the emitter to the downloader.
data FromEmitterMessage hash text
  = EmitterErrorMsg SyncError
  | EmitterEntityMsg (Entity hash text)
  | EmitterDoneMsg

instance (CBOR.Serialise hash, CBOR.Serialise text) => WebSocketsData (FromEmitterMessage hash text) where
  fromLazyByteString bytes =
    CBOR.deserialiseOrFailCBORBytes (CBOR.CBORBytes bytes)
      & either (\err -> EmitterErrorMsg . EncodingFailure $ "Error decoding CBOR message from bytes: " <> tShow err) id

  toLazyByteString = CBOR.serialise

  fromDataMessage dm = do
    case dm of
      WS.Text bytes _ -> WS.fromLazyByteString bytes
      WS.Binary bytes -> WS.fromLazyByteString bytes

data HashMappings hash smallHash = HashMappings
  { hashMappings :: Map smallHash hash
  }

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

-- entityTexts_ :: Traversal (Entity smallHash text) (Entity smallHash text') text text'
-- entityTexts_ f (Entity {entityData, ..}) =
--   (\entityData' -> Entity {entityData = entityData', ..}) <$> Entity.texts_ f entityData

-- entityHashesSetter_ :: (Monad m) => LensLike m (Entity smallHash text) (Entity smallHash' text) smallHash smallHash'
-- entityHashesSetter_ f (Entity {entityHash, entityData, ..}) =
--   (\entityHash' entityData' -> Entity {entityHash = entityHash', entityData = entityData', ..})
--     <$> f entityHash
--     <*> ( entityData
--             & Entity.hashes_ f
--             >>= Entity.defns_ f
--             >>= Entity.patches_ f
--             >>= Entity.branchHashes_ f
--             >>= Entity.branches_ f
--             >>= Entity.causalHashes_ f
--         )

-- -- | It's technically possible to implement entityHashesGetter_ and entityHashesSetter_
-- -- as a single Traversal, but it's a ton of extra unpacking/packing that's probably not worth
-- -- it.
-- entityHashesGetter_ :: Fold (Entity smallHash text) smallHash
-- entityHashesGetter_ f (Entity {entityHash, entityData}) =
--   phantom (f entityHash)
--     *> phantom (Entity.hashes_ f entityData)
--     *> phantom (Entity.defns_ f entityData)
--     *> phantom (Entity.patches_ f entityData)
--     *> phantom (Entity.branchHashes_ f entityData)
--     *> phantom (Entity.branches_ f entityData)
--     *> phantom (Entity.causalHashes_ f entityData)

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

instance (Ord smallHash, CBOR.Serialise hash, CBOR.Serialise smallHash) => CBOR.Serialise (HashMappings hash smallHash) where
  encode (HashMappings {hashMappings}) =
    CBOR.encode hashMappings

  decode = do
    hashMappings <- CBOR.decode @(Map smallHash hash)
    pure $ HashMappings {hashMappings}

instance (CBOR.Serialise hash, CBOR.Serialise text) => CBOR.Serialise (FromEmitterMessage hash text) where
  encode = \case
    EmitterErrorMsg err -> CBOR.encode EmitterErrorMsgTag <> CBOR.encode err
    -- HashMappingsMsg msg -> CBOR.encode HashMappingsTag <> CBOR.encode msg
    EmitterEntityMsg msg -> CBOR.encode EmitterEntityTag <> CBOR.encode msg
    EmitterDoneMsg -> CBOR.encode EmitterDoneTag

  decode = do
    tag <- CBOR.decode @FromEmitterMessageTag
    case tag of
      EmitterErrorMsgTag -> EmitterErrorMsg <$> CBOR.decode
      -- HashMappingsTag -> HashMappingsMsg <$> CBOR.decode
      EmitterEntityTag -> EmitterEntityMsg <$> CBOR.decode
      EmitterDoneTag -> pure EmitterDoneMsg

data FromEmitterMessageTag
  = EmitterErrorMsgTag
  | -- | HashMappingsTag
    EmitterEntityTag
  | EmitterDoneTag

instance CBOR.Serialise FromEmitterMessageTag where
  encode = \case
    EmitterErrorMsgTag -> CBOR.encode (0 :: Int)
    -- HashMappingsTag -> CBOR.encode (1 :: Int)
    EmitterEntityTag -> CBOR.encode (2 :: Int)
    EmitterDoneTag -> CBOR.encode (3 :: Int)

  decode = do
    tag <- CBOR.decode @Int
    case tag of
      0 -> pure EmitterErrorMsgTag
      -- 1 -> pure HashMappingsTag
      2 -> pure EmitterEntityTag
      _ -> fail $ "Unknown FromEmitterMessageTag: " <> show tag

data MsgOrError err a
  = Msg a
  | Err err

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

instance (CBOR.Serialise sh, ToJSON ah, FromJSON ah) => WebSocketsData (MsgOrError SyncError (FromReceiverMessage ah sh)) where
  fromLazyByteString bytes =
    CBOR.deserialiseOrFailCBORBytes (CBOR.CBORBytes bytes)
      & either (\err -> Err . EncodingFailure $ "Error decoding CBOR message from bytes: " <> tShow err) Msg

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

instance CBOR.Serialise HashTag where
  encode (HashTag (kind, idx)) =
    CBOR.encode (kind, idx)

  decode = do
    (kind, idx) <- CBOR.decode @(EntityKind, Int64)
    pure $ HashTag (kind, idx)

newtype BranchRef = BranchRef {unBranchRef :: Text}
  deriving (Serialise, Eq, Show, Ord, ToJSON, FromJSON) via Text
