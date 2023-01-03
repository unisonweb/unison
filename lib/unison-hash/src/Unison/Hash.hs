module Unison.Hash
  ( Hash (Hash),
    HashFor (..),

    -- ** ShortByteString conversions
    toShort,

    -- ** ByteString conversions
    fromByteString,
    toByteString,

    -- ** Base32Hex conversions
    fromBase32Hex,
    toBase32Hex,

    -- ** Base32Hex Text conversions
    fromBase32HexText,
    unsafeFromBase32HexText,
    toBase32HexText,
  )
where

import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as B.Short
import U.Util.Base32Hex (Base32Hex)
import qualified U.Util.Base32Hex as Base32Hex
import Unison.Prelude

-- | A hash.
newtype Hash = Hash {toShort :: ShortByteString}
  deriving stock (Eq, Ord, Generic)

instance Show Hash where
  show = show . toBase32HexText

-- | A hash tagged with the type it's a hash of, useful for maintaining type safety guarantees.
newtype HashFor t = HashFor {genericHash :: Hash}
  deriving newtype (Show, Eq, Ord, Generic)

-- | Convert a hash to a byte string.
toByteString :: Hash -> ByteString
toByteString = B.Short.fromShort . toShort

-- | Convert a byte string to a hash.
fromByteString :: ByteString -> Hash
fromByteString = Hash . B.Short.toShort

-- | Convert base32 hex to a hash.
fromBase32Hex :: Base32Hex -> Hash
fromBase32Hex = fromByteString . Base32Hex.toByteString

-- | Convert a hash to base32 hex.
toBase32Hex :: Hash -> Base32Hex
toBase32Hex = Base32Hex.fromByteString . toByteString

-- | Produce a 'Hash' from a base32hex-encoded version of its binary representation
fromBase32HexText :: Text -> Maybe Hash
fromBase32HexText = fmap fromBase32Hex . Base32Hex.fromText

-- | Convert a hash from base32 hex without any validation.
unsafeFromBase32HexText :: Text -> Hash
unsafeFromBase32HexText = fromBase32Hex . Base32Hex.UnsafeFromText

-- | Return the lowercase unpadded base32Hex encoding of this 'Hash'.
-- Multibase prefix would be 'v', see https://github.com/multiformats/multibase
toBase32HexText :: Hash -> Text
toBase32HexText = Base32Hex.toText . toBase32Hex
