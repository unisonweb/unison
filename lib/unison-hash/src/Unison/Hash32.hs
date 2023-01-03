-- | A 512-bit hash, internally represented as base32hex.
module Unison.Hash32
  ( -- * Hash32 type
    Hash32 (..),

    -- * Conversions

    -- ** The other Hash :)
    fromHash,
    toHash,

    -- ** Base32Hex
    unsafeFromBase32Hex,
    toBase32Hex,

    -- ** Text
    toText,
  )
where

import U.Util.Base32Hex (Base32Hex (..))
import Unison.Hash (Hash)
import qualified Unison.Hash as Hash
import Unison.Prelude

-- | A 512-bit hash, internally represented as base32hex.
--
-- Some orphan instances provided in:
--
--   * @unison-util-base32hex-orphans-aeson@
--   * @unison-util-base32hex-orphans-sqlite@
newtype Hash32 = UnsafeFromBase32Hex Base32Hex
  deriving (Eq, Ord, Show) via (Text)

fromHash :: Hash -> Hash32
fromHash =
  unsafeFromBase32Hex . Hash.toBase32Hex

toHash :: Hash32 -> Hash
toHash =
  Hash.fromBase32Hex . toBase32Hex

-- | Convert base32hex to a hash32 (asserting that it is a 512-bit hash).
unsafeFromBase32Hex :: Base32Hex -> Hash32
unsafeFromBase32Hex =
  coerce

-- | Convert a hash32 to base32hex.
toBase32Hex :: Hash32 -> Base32Hex
toBase32Hex =
  coerce

toText :: Hash32 -> Text
toText =
  coerce
