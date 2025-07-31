module Unison.Hash
  ( Hash (Hash),
    HashFor (..),

    -- ** ShortByteString conversions
    toShort,
    fromShortByteStringInterned,

    -- ** ByteString conversions
    fromByteString,
    fromByteStringInterned,
    toByteString,

    -- ** Base32Hex conversions
    fromBase32Hex,
    fromBase32HexInterned,
    toBase32Hex,

    -- ** Base32Hex Text conversions
    fromBase32HexText,
    fromBase32HexTextInterned,
    unsafeFromBase32HexText,
    unsafeFromBase32HexTextInterned,
    toBase32HexText,
  )
where

import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified as B.Short
import Data.Hashable (Hashable)
import System.IO.Unsafe (unsafePerformIO)
import U.Util.Base32Hex (Base32Hex)
import U.Util.Base32Hex qualified as Base32Hex
import Unison.Prelude
import Unison.Util.InternCache (InternCache)
import Unison.Util.InternCache qualified as IC

-- An in memory cache for interning hashes.
-- This allows us to avoid creating multiple in-memory instances of the same hash bytes;
-- but also has the benefit that equality checks for equal hashes are O(1) instead of O(n), since
-- they'll be pointer-equal.
hashCache :: (MonadIO m) => InternCache m Hash Hash
hashCache = unsafePerformIO $ IC.hoist liftIO <$> IC.newInternCache @IO @Hash @Hash
{-# NOINLINE hashCache #-}

fromShortByteStringInterned :: (MonadIO m) => ShortByteString -> m Hash
fromShortByteStringInterned short = do
  IC.intern hashCache (Hash short)

-- | A hash.
newtype Hash = Hash {toShort :: ShortByteString}
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (Hashable)
{-# DEPRECATED Hash "Intern this!" #-}

instance Show Hash where
  show = show . toBase32HexText

-- | A hash tagged with the type it's a hash of, useful for maintaining type safety guarantees.
newtype HashFor t = HashFor {genericHash :: Hash}
  deriving newtype (Show, Eq, Ord, Generic)

instance From Hash Text where
  from = toBase32HexText

-- | Convert a hash to a byte string.
toByteString :: Hash -> ByteString
toByteString = B.Short.fromShort . toShort

-- | Convert a byte string to a hash.
fromByteString :: ByteString -> Hash
fromByteString = Hash . B.Short.toShort
{-# DEPRECATED fromByteString "Intern this!" #-}

fromByteStringInterned :: (MonadIO m) => ByteString -> m Hash
fromByteStringInterned bs = do
  IC.intern hashCache (Hash (B.Short.toShort bs))

-- | Convert base32 hex to a hash.
fromBase32Hex :: Base32Hex -> Hash
fromBase32Hex = fromByteString . Base32Hex.toByteString
{-# DEPRECATED fromBase32Hex "Intern this!" #-}

fromBase32HexInterned :: (MonadIO m) => Base32Hex -> m Hash
fromBase32HexInterned base32Hex = do
  IC.intern hashCache (fromBase32Hex base32Hex)

-- | Convert a hash to base32 hex.
toBase32Hex :: Hash -> Base32Hex
toBase32Hex = Base32Hex.fromByteString . toByteString

-- | Produce a 'Hash' from a base32hex-encoded version of its binary representation
fromBase32HexText :: Text -> Maybe Hash
fromBase32HexText = fmap fromBase32Hex . Base32Hex.fromText
{-# DEPRECATED fromBase32HexText "Intern this!" #-}

fromBase32HexTextInterned :: (MonadIO m) => Text -> m (Maybe Hash)
fromBase32HexTextInterned text = do
  for (fromBase32HexText text) (IC.intern hashCache)

-- | Convert a hash from base32 hex without any validation.
unsafeFromBase32HexText :: Text -> Hash
unsafeFromBase32HexText = fromBase32Hex . Base32Hex.UnsafeFromText
{-# DEPRECATED unsafeFromBase32HexText "Intern this!" #-}

unsafeFromBase32HexTextInterned :: (MonadIO m) => Text -> m Hash
unsafeFromBase32HexTextInterned text = do
  IC.intern hashCache (unsafeFromBase32HexText text)

-- | Return the lowercase unpadded base32Hex encoding of this 'Hash'.
-- Multibase prefix would be 'v', see https://github.com/multiformats/multibase
toBase32HexText :: Hash -> Text
toBase32HexText = Base32Hex.toText . toBase32Hex
