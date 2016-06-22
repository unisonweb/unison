module Unison.Namespace where

import Data.Word (Word64)
import Data.ByteString (ByteString)

newtype Name = Name ByteString
newtype Nonce = Nonce Word64

data Authorization = Authorization { nonce :: Nonce, bytes :: ByteString }

-- | A mutable namespace of hashes.
data Namespace key fingerprint h = Namespace {
  -- | Associate the given name with the provided key, returning a hash of key
  declare :: key -> Name -> IO fingerprint,
  -- | Resolve a fingerprint + name to a hash
  resolve :: fingerprint -> Name -> IO (Maybe (h, Nonce)),
  -- | Requires proof of knowledge of the key passed to `declare`
  publish :: fingerprint -> Name -> Authorization -> h -> IO (Maybe Nonce)
}
