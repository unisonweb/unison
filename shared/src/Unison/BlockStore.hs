{-# Language DeriveGeneric #-}

module Unison.BlockStore where

import Data.ByteString (ByteString)
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics
import qualified Data.ByteString.Base64.URL as Base64

newtype Series = Series ByteString deriving (Generic, Eq, Ord)

instance Show Series where
  show (Series s) = unpack . decodeUtf8 . Base64.encode $ s

-- | Represents an immutable content-addressed storage layer.
-- We can insert some bytes, getting back a hash which can be used for lookup:
-- That is, given `bs : BlockStore`, `insert bs bytes >>= \h -> lookup bs h`
-- should return `bs`.
--
-- `insert` is idempotent: `insert bs bytes >> insert bs bytes`, implying that
-- the returned hash is a pure function of the content of `bytes`.
--
-- Though the store is immutable, a `Series` represents a mutable pointer to a
-- hash. A successful `update` of a `Series` tells the implementation that the
-- previous hash may be deleted from the store.
data BlockStore h = BlockStore {
  insert :: ByteString -> IO h,
  lookup :: h -> IO (Maybe ByteString),
  -- | Will return a random hash if Series not already declared, otherwise returns the result of `resolve`
  declareSeries :: Series -> IO h,
  -- | Marks the `Series` as garbage, allowing it to be collected
  deleteSeries :: Series -> IO (),
  -- | Update the value associated with this series. Any previous value(s) for the series
  -- are considered garbage after the `update` and may be deleted by the store.
  update :: Series -> h -> ByteString -> IO (Maybe h),
  -- | Like `update`, but does not delete the previous block written to the series
  append :: Series -> h -> ByteString -> IO (Maybe h),
  -- | Obtain the most recent hash for a series
  resolve :: Series -> IO (Maybe h),
  -- | Obtain all the hashes for a series
  resolves :: Series -> IO [h]
}
