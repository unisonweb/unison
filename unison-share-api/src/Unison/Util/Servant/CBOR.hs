-- | Servant configuration for the CBOR media type
--
-- Adapted from https://hackage.haskell.org/package/servant-serialization-0.3/docs/Servant-API-ContentTypes-SerialiseCBOR.html via MIT license
module Unison.Util.Servant.CBOR (CBOR) where

import Codec.CBOR.Read (DeserialiseFailure (..))
import Codec.Serialise (Serialise, deserialiseOrFail, serialise)
import Data.ByteString.Lazy qualified as BL
import Data.List.NonEmpty qualified as NonEmpty
import Network.HTTP.Media.MediaType qualified as MediaType
import Servant

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

-- | Wrapper for CBOR data that has already been serialized
newtype RawCBOR = RawCBOR BL.ByteString

instance {-# OVERLAPPING #-} MimeRender CBOR RawCBOR where
  mimeRender Proxy (RawCBOR bs) = bs

instance {-# OVERLAPPING #-} MimeUnrender CBOR RawCBOR where
  mimeUnrender Proxy bs = Right (RawCBOR bs)
