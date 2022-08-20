{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module U.Util.Hash
  ( Hash (Hash, toShort),
    unsafeFromBase32HexText,
    fromBase32Hex,
    fromByteString,
    toBase32Hex,
    toBase32HexText,
    toByteString,
    HashFor (..),
  )
where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString, fromShort)
import qualified Data.ByteString.Short as B.Short
import Data.Text (Text)
import GHC.Generics (Generic)
import U.Util.Base32Hex (Base32Hex)
import qualified U.Util.Base32Hex as Base32Hex

-- | Hash which uniquely identifies a Unison type or term
newtype Hash = Hash {toShort :: ShortByteString}
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (NFData)

toBase32Hex :: Hash -> Base32Hex
toBase32Hex = Base32Hex.fromByteString . toByteString

toBase32HexText :: Hash -> Text
toBase32HexText = Base32Hex.toText . toBase32Hex

fromBase32Hex :: Base32Hex -> Hash
fromBase32Hex = Hash . B.Short.toShort . Base32Hex.toByteString

-- | Constructs a hash from base32 checks without any validation.
-- Note that this converts Text -> ByteString -> ShortByteString and so is slower than
-- we'd prefer.
unsafeFromBase32HexText :: Text -> Hash
unsafeFromBase32HexText = fromBase32Hex . Base32Hex.UnsafeFromText

toByteString :: Hash -> ByteString
toByteString = fromShort . toShort

fromByteString :: ByteString -> Hash
fromByteString = Hash . B.Short.toShort

instance Show Hash where
  show h = (show . toBase32HexText) h

-- | A hash tagged with the type it's a hash of, useful for maintaining type safety
-- guarantees.
newtype HashFor t = HashFor {genericHash :: Hash}
  deriving newtype (Show, Eq, Ord, Generic, NFData)
