{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}


module U.Util.Base32Hex where

import Data.Text (Text)
import qualified Codec.Binary.Base32Hex
import Data.ByteString (ByteString)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Maybe (fromMaybe)

newtype Base32Hex = UnsafeBase32Hex { toText :: Text }
  deriving (Eq, Ord, Show)

-- | Return the lowercase unpadded base32Hex encoding of this 'ByteString'.
-- Multibase prefix would be 'v', see https://github.com/multiformats/multibase
fromByteString :: ByteString -> Base32Hex
fromByteString bs =
  -- we're using an uppercase encoder that adds padding, so we drop the
  -- padding and convert it to lowercase
  UnsafeBase32Hex . Text.toLower . Text.dropWhileEnd (== '=') . decodeUtf8 $
  Codec.Binary.Base32Hex.encode bs

toByteString :: Base32Hex -> ByteString
toByteString = fromMaybe err . textToByteString . toText
  where err = "invalid base32Hex presumably created via \"unsafe\" constructors"

-- | Produce a 'Hash' from a base32hex-encoded version of its binary representation
textToByteString :: Text -> Maybe ByteString
textToByteString txt =
  case Codec.Binary.Base32Hex.decode (encodeUtf8 $ Text.toUpper txt <> paddingChars) of
    Left (_, _rem) -> Nothing
    Right h -> pure h
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
    0  -> ""
    8  -> "======"
    16 -> "===="
    24 -> "==="
    32 -> "="
    i  -> error $ "impossible hash length `mod` 40 not in {0,8,16,24,32}: " <> show i
