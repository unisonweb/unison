-- | Servant configuration for the CBOR media type
--
-- Adapted from https://hackage.haskell.org/package/servant-serialization-0.3/docs/Servant-API-ContentTypes-SerialiseCBOR.html via MIT license
module Unison.Util.Servant.CBOR
  ( CBOR,
    UnknownCBORBytes,
    CBORBytes (..),
    CBORStream (..),
    unpackCBORBytesStream,
    deserialiseOrFailCBORBytes,
    serialiseCBORBytes,
    decodeCBORBytes,
    decodeUnknownCBORBytes,
    serialiseUnknownCBORBytes,
    CBORStreamError (..),
    decodeUnframedEntities,
  )
where

import Codec.CBOR.Read (DeserialiseFailure (..))
import Codec.Serialise (Serialise, deserialiseOrFail, serialise)
import Codec.Serialise qualified as CBOR
import Codec.Serialise.Decoding qualified as CBORDecode
import Conduit
import Control.Monad.Except
import Control.Monad.ST (ST, stToIO)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Conduit.Combinators qualified as Conduit
import Data.List.NonEmpty qualified as NonEmpty
import Network.HTTP.Media.MediaType qualified as MediaType
import Servant
import Unison.Prelude

-- | Content-type for encoding and decoding objects as their CBOR representations
data CBOR

-- | Mime-type for CBOR and additional ones using the word "hackage" and the
-- name of the package "serialise".
instance Accept CBOR where
  contentTypes Proxy =
    NonEmpty.singleton ("application" MediaType.// "cbor")

-- |
--
-- >>> mimeRender (Proxy :: Proxy CBOR) ("Hello" :: String)
-- "eHello"
instance (Serialise a) => MimeRender CBOR a where
  mimeRender Proxy = serialise

-- |
--
-- >>> let bsl = mimeRender (Proxy :: Proxy CBOR) (3.14 :: Float)
-- >>> mimeUnrender (Proxy :: Proxy CBOR) bsl :: Either String Float
-- Right 3.14
--
-- >>> mimeUnrender (Proxy :: Proxy CBOR) (bsl <> "trailing garbage") :: Either String Float
-- Right 3.14
--
-- >>> mimeUnrender (Proxy :: Proxy CBOR) ("preceding garbage" <> bsl) :: Either String Float
-- Left "Codec.Serialise.deserialiseOrFail: expected float at byte-offset 0"
instance (Serialise a) => MimeUnrender CBOR a where
  mimeUnrender Proxy = mapLeft prettyErr . deserialiseOrFail
    where
      mapLeft f = either (Left . f) Right
      prettyErr (DeserialiseFailure offset err) =
        "Codec.Serialise.deserialiseOrFail: " ++ err ++ " at byte-offset " ++ show offset

-- | Wrapper for CBOR data that has already been serialized.
-- In our case, we use this because we may load pre-serialized CBOR directly from the database,
-- but it's also useful in allowing us to more quickly seek through a CBOR stream, since we only need to decode the CBOR when/if we actually need to use it, and can skip past it using a byte offset otherwise.
--
-- The 't' phantom type is the type of the data encoded in the bytestring.
newtype CBORBytes t = CBORBytes BL.ByteString
  deriving (Serialise) via (BL.ByteString)
  deriving (Eq, Show, Ord)

-- | Deserialize a 'CBORBytes' value into its tagged type, throwing an error if the deserialization fails.
deserialiseOrFailCBORBytes :: (Serialise t) => CBORBytes t -> Either CBOR.DeserialiseFailure t
deserialiseOrFailCBORBytes (CBORBytes bs) = CBOR.deserialiseOrFail bs

decodeCBORBytes :: (Serialise t) => CBORBytes t -> CBORDecode.Decoder s t
decodeCBORBytes (CBORBytes bs) = decodeUnknownCBORBytes (CBORBytes bs)

decodeUnknownCBORBytes :: (Serialise t) => UnknownCBORBytes -> CBORDecode.Decoder s t
decodeUnknownCBORBytes (CBORBytes bs) = case deserialiseOrFailCBORBytes (CBORBytes bs) of
  Left err -> fail (show err)
  Right t -> pure t

serialiseCBORBytes :: (Serialise t) => t -> CBORBytes t
serialiseCBORBytes = CBORBytes . CBOR.serialise

serialiseUnknownCBORBytes :: (Serialise t) => t -> UnknownCBORBytes
serialiseUnknownCBORBytes = CBORBytes . CBOR.serialise

data Unknown

type UnknownCBORBytes = CBORBytes Unknown

-- | Wrapper for a stream of CBOR data. Each chunk may not be a complete CBOR value, but the concatenation of all the chunks is a valid CBOR stream.
newtype CBORStream a = CBORStream BL.ByteString
  deriving (Serialise) via (BL.ByteString)
  deriving (Eq, Show, Ord)

instance MimeRender OctetStream (CBORStream a) where
  mimeRender Proxy (CBORStream bs) = bs

instance MimeUnrender OctetStream (CBORStream a) where
  mimeUnrender Proxy bs = Right (CBORStream bs)

unpackCBORBytesStream :: (CBOR.Serialise o, MonadIO m) => ConduitT (CBORStream o) o (ExceptT CBORStreamError m) ()
unpackCBORBytesStream =
  Conduit.map (BL.toStrict . coerce @_ @BL.ByteString) Conduit..| decodeUnframedEntities

data CBORStreamError
  = CBORStreamDeserializationError CBOR.DeserialiseFailure
  | CBORStreamInitializationError Text
  | CBORStreamUnexpectedEndOfInput
  deriving (Eq, Show)

-- | Unpacks a stream of tightly-packed CBOR entities without any framing/separators.
decodeUnframedEntities :: forall a m. (MonadIO m) => (CBOR.Serialise a) => ConduitT BS.ByteString a (ExceptT CBORStreamError m) ()
decodeUnframedEntities = Conduit.transPipe (mapExceptT (liftIO . stToIO)) $ do
  Conduit.await >>= \case
    Nothing -> pure ()
    Just bs -> do
      d <- newDecoder
      loop bs d
  where
    newDecoder :: ConduitT BS.ByteString a (ExceptT CBORStreamError (ST s)) (Maybe BS.ByteString -> ST s (CBOR.IDecode s a))
    newDecoder = do
      (lift . lift) CBOR.deserialiseIncremental >>= \case
        CBOR.Done _ _ _ -> throwError $ CBORStreamInitializationError "Decoder unexpectedly finished immediately"
        CBOR.Fail _ _ err -> throwError $ CBORStreamDeserializationError err
        CBOR.Partial k -> pure k
    loop :: BS.ByteString -> (Maybe BS.ByteString -> ST s (CBOR.IDecode s a)) -> ConduitT BS.ByteString a (ExceptT CBORStreamError (ST s)) ()
    loop bs k = do
      (lift . lift) (k (Just bs)) >>= \case
        CBOR.Fail _ _ err -> throwError $ CBORStreamDeserializationError err
        CBOR.Partial k' -> do
          -- We need more input, try to get some
          nextBS <- Conduit.await
          case nextBS of
            Nothing -> do
              -- No more input, try to finish up the decoder.
              (lift . lift) (k' Nothing) >>= \case
                CBOR.Done _ _ a -> Conduit.yield a
                CBOR.Fail _ _ err -> throwError $ CBORStreamDeserializationError err
                CBOR.Partial _ -> throwError CBORStreamUnexpectedEndOfInput
            Just bs' ->
              -- Have some input, keep going.
              loop bs' k'
        CBOR.Done rem _ a -> do
          Conduit.yield a
          if BS.null rem
            then do
              -- If we had no leftovers, we can check if there's any input left.
              Conduit.await >>= \case
                Nothing -> pure ()
                Just bs'' -> do
                  -- If we have input left, start up a new decoder.
                  k <- newDecoder
                  loop bs'' k
            else do
              -- We have leftovers, start a new decoder and use those.
              k <- newDecoder
              loop rem k
