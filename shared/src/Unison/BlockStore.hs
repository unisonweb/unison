module Unison.BlockStore where

import Data.ByteString (ByteString)

newtype Series = Series ByteString

-- | Represents an immutable content-addressed storage layer. We can insert some bytes, getting back a `h` which represents a hash`.
-- Given `bs : BlockStore`
--   `insert bs bytes >>= \h -> lookup bs h`
-- also `insert bs bytes >> insert bs bytes`
-- that the returned hash be purely a hash of the content of `bytes`
data BlockStore h = BlockStore {
  insert :: ByteString -> IO h,
  lookup :: h -> IO (Maybe ByteString),
  -- | Will return a random hash if Series not already declared, otherwise returns the result of `resolve`
  declareSeries :: Series -> IO h,
  -- this fails if the hash does not match the last written block for this series
  -- but if it succeeds, the input hash is marked as garbage and can be deleted
  update :: Series -> h -> ByteString -> IO (Maybe h),
  -- obtain the current hash for a series
  resolve :: Series -> IO (Maybe h)
  -- todo: stream inserts
}
