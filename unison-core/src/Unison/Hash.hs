{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Unison.Hash (Hash, toBytes, base32Hex, base32Hexs, fromBase32Hex, fromBytes, unsafeFromBase32Hex, showBase32Hex, validBase32HexChars) where

import qualified Codec.Binary.Base32Hex as Base32Hex
import qualified Crypto.Hash as CH
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import Data.ByteString.Builder (doubleBE, int64BE, toLazyByteString, word64BE)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Unison.Hashable as H
import Unison.Prelude

-- | Hash which uniquely identifies a Unison type or term
newtype Hash = Hash {toBytes :: ByteString} deriving (Eq, Ord, Generic)

instance Show Hash where
  show h = take 999 $ Text.unpack (base32Hex h)

instance H.Hashable Hash where
  tokens h = [H.Bytes (toBytes h)]

fromBytesImpl :: ByteString -> Hash
fromBytesImpl = fromBytes

toBytesImpl :: Hash -> ByteString
toBytesImpl = toBytes

instance H.Accumulate Hash where
  accumulate = fromBytes . BA.convert . CH.hashFinalize . go CH.hashInit
    where
      go :: CH.Context CH.SHA3_512 -> [H.Token Hash] -> CH.Context CH.SHA3_512
      go acc tokens = CH.hashUpdates acc (tokens >>= toBS)
      toBS (H.Tag b) = [B.singleton b]
      toBS (H.Bytes bs) = [encodeLength $ B.length bs, bs]
      toBS (H.Int i) = BL.toChunks . toLazyByteString . int64BE $ i
      toBS (H.Nat i) = BL.toChunks . toLazyByteString . word64BE $ i
      toBS (H.Double d) = BL.toChunks . toLazyByteString . doubleBE $ d
      toBS (H.Text txt) =
        let tbytes = encodeUtf8 txt
         in [encodeLength (B.length tbytes), tbytes]
      toBS (H.Hashed h) = [toBytes h]
      encodeLength :: Integral n => n -> B.ByteString
      encodeLength = BL.toStrict . toLazyByteString . word64BE . fromIntegral
  fromBytes = fromBytesImpl
  toBytes = toBytesImpl

-- | Return the lowercase unpadded base32Hex encoding of this 'Hash'.
-- Multibase prefix would be 'v', see https://github.com/multiformats/multibase
base32Hex :: Hash -> Text
base32Hex (Hash h) =
  -- we're using an uppercase encoder that adds padding, so we drop the
  -- padding and convert it to lowercase
  Text.toLower . Text.dropWhileEnd (== '=') . decodeUtf8 $
    Base32Hex.encode h

validBase32HexChars :: Set Char
validBase32HexChars = Set.fromList $ ['0' .. '9'] ++ ['a' .. 'v']

-- | Produce a 'Hash' from a base32hex-encoded version of its binary representation
fromBase32Hex :: Text -> Maybe Hash
fromBase32Hex txt = case Base32Hex.decode (encodeUtf8 $ Text.toUpper txt <> paddingChars) of
  Left (_, _rem) -> Nothing
  Right h -> pure $ Hash h
  where
    -- The decoder we're using is a base32 uppercase decoder that expects padding,
    -- so we provide it with the appropriate number of padding characters for the
    -- expected hash length.
    --
    -- The decoder requires 40 bit (8 5-bit characters) chunks, so if the number
    -- of characters of the input is not a multiple of 8, we add '=' padding chars
    -- until it is.
    --
    -- See https://tools.ietf.org/html/rfc4648#page-8
    paddingChars :: Text
    paddingChars = case Text.length txt `mod` 8 of
      0 -> ""
      n -> Text.replicate (8 - n) "="

    hashLength :: Int
    hashLength = 512

    _paddingChars :: Text
    _paddingChars = case hashLength `mod` 40 of
      0 -> ""
      8 -> "======"
      16 -> "===="
      24 -> "==="
      32 -> "="
      i -> error $ "impossible hash length `mod` 40 not in {0,8,16,24,32}: " <> show i

base32Hexs :: Hash -> String
base32Hexs = Text.unpack . base32Hex

unsafeFromBase32Hex :: Text -> Hash
unsafeFromBase32Hex txt =
  fromMaybe (error $ "invalid base32Hex value: " ++ Text.unpack txt) $ fromBase32Hex txt

fromBytes :: ByteString -> Hash
fromBytes = Hash

showBase32Hex :: H.Hashable t => t -> String
showBase32Hex = base32Hexs . H.accumulate'
