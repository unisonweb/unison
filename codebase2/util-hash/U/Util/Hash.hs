{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module U.Util.Hash where

-- (Hash, toBytes, base32Hex, base32Hexs, fromBase32Hex, fromBytes, unsafeFromBase32Hex, showBase32Hex, validBase32HexChars) where

-- import Unison.Prelude

import Data.ByteString (ByteString)
import qualified Data.ByteString.Short as B.Short
import GHC.Generics (Generic)
import Data.ByteString.Short (fromShort, ShortByteString)
import qualified U.Util.Base32Hex as Base32Hex
import U.Util.Base32Hex (Base32Hex)

-- | Hash which uniquely identifies a Unison type or term
newtype Hash = Hash {toShort :: ShortByteString} deriving (Eq, Ord, Generic)

fromBase32Hex :: Base32Hex -> Hash
fromBase32Hex = Hash . B.Short.toShort . Base32Hex.toByteString

toBase32Hex :: Hash -> Base32Hex
toBase32Hex = Base32Hex.fromByteString . toBytes

fromBytes :: ByteString -> Hash
fromBytes = Hash . B.Short.toShort

toBytes :: Hash -> ByteString
toBytes = fromShort . toShort

instance Show Hash where
  show h = "fromBase32Hex " ++ (show . Base32Hex.toText . toBase32Hex) h
