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
  insertSeries :: ByteString -> IO (h, Series),
  -- this fails if the hash does not match the last written block for this series
  -- but if it succeeds, the input hash is marked as garbage and can be deleted
  update :: Series -> h -> ByteString -> IO (Maybe h)
  -- todo: stream inserts
}
