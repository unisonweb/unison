{-# LANGUAGE DeriveGeneric #-}

module Unison.Hash (Hash, toBytes, base58, fromBase58, fromBytes, unsafeFromBase58) where

import Data.ByteString (ByteString)
import Data.ByteString.Builder (doubleBE, word64BE, int64BE, toLazyByteString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics
import qualified Data.ByteArray as BA

import qualified Crypto.Hash as CH
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import qualified Unison.Hashable as H
import qualified Data.ByteString.Base58 as Base58
import qualified Data.Text as Text

-- | Hash which uniquely identifies a Unison type or term
newtype Hash = Hash { toBytes :: ByteString } deriving (Eq,Ord,Generic)

instance Show Hash where
  show h = take 8 $ Text.unpack (base58 h)

fromBytesImpl :: ByteString -> Hash
fromBytesImpl = fromBytes

toBytesImpl :: Hash -> ByteString
toBytesImpl = toBytes

instance H.Accumulate Hash where
  accumulate = fromBytes . BA.convert . CH.hashFinalize . go CH.hashInit where
    go :: CH.Context CH.SHA3_512 -> [H.Token Hash] -> CH.Context CH.SHA3_512
    go acc tokens = CH.hashUpdates acc $ (tokens >>= toBS)
    toBS (H.Tag b) = [B.singleton b]
    toBS (H.Bytes bs) = [encodeLength (B.length $ bs), bs]
    toBS (H.Int64 i) = BL.toChunks . toLazyByteString . int64BE $ i
    toBS (H.UInt64 i) = BL.toChunks . toLazyByteString . word64BE $ i
    toBS (H.Double d) = BL.toChunks . toLazyByteString . doubleBE $ d
    toBS (H.Text txt) =
      let tbytes = encodeUtf8 txt
      in [encodeLength (B.length tbytes), tbytes]
    toBS (H.Hashed h) = [toBytes h]
    encodeLength :: Integral n => n -> B.ByteString
    encodeLength len =
      BL.toStrict . toLazyByteString . word64BE . fromIntegral $ len
  fromBytes = fromBytesImpl
  toBytes = toBytesImpl

-- | Return the base58 encoding of this 'Hash'
base58 :: Hash -> Text
base58 (Hash h) = decodeUtf8 (Base58.encodeBase58 Base58.bitcoinAlphabet h)

-- | Produce a 'Hash' from a base58-encoded version of its binary representation
fromBase58 :: Text -> Maybe Hash
fromBase58 txt = Hash <$> Base58.decodeBase58 Base58.bitcoinAlphabet (encodeUtf8 txt)

unsafeFromBase58 :: Text -> Hash
unsafeFromBase58 txt = case fromBase58 txt of
  Just h -> h
  Nothing -> error $ "invalid base58: " ++ Text.unpack txt

fromBytes :: ByteString -> Hash
fromBytes = Hash
