-- | A 512-bit hash, internally represented as base32hex.
module U.Util.Hash32
  ( -- * Hash32 type
    Hash32,

    -- * Conversions

    -- ** Base32Hex
    fromBase32Hex,
    toBase32Hex,

    -- ** The other Hash :)
    fromHash,
    toHash,
  )
where

import U.Util.Base32Hex (Base32Hex)
import U.Util.Hash (Hash)
import qualified U.Util.Hash as Hash
import Unison.Prelude

-- | A 512-bit hash, internally represented as base32hex.
newtype Hash32 = Hash32 Base32Hex

fromBase32Hex :: Base32Hex -> Hash32
fromBase32Hex =
  Hash32

toBase32Hex :: Hash32 -> Base32Hex
toBase32Hex =
  coerce

fromHash :: Hash -> Hash32
fromHash =
  fromBase32Hex . Hash.toBase32Hex

toHash :: Hash32 -> Hash
toHash =
  Hash.fromBase32Hex . toBase32Hex
