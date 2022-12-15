{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Unison.Hash
  ( Hash (Hash),
    HashFor (..),
    base32Hex,
    fromBase32Hex,
    Hash.fromByteString,
    Hash.toByteString,
    validBase32HexChars,
  )
where

import qualified U.Util.Base32Hex as Base32Hex
import U.Util.Hash (Hash (Hash), HashFor (..))
import qualified U.Util.Hash as Hash
import Unison.Prelude

-- | Return the lowercase unpadded base32Hex encoding of this 'Hash'.
-- Multibase prefix would be 'v', see https://github.com/multiformats/multibase
base32Hex :: Hash -> Text
base32Hex = Base32Hex.toText . Hash.toBase32Hex

-- | Produce a 'Hash' from a base32hex-encoded version of its binary representation
fromBase32Hex :: Text -> Maybe Hash
fromBase32Hex = fmap Hash.fromBase32Hex . Base32Hex.fromText

validBase32HexChars :: Set Char
validBase32HexChars = Base32Hex.validChars
