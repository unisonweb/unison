module Unison.Storage where

import Unison.Namespace (Namespace, Name, Nonce, Authorization)
import Unison.BlockStore (BlockStore)

data Storage key fingerprint h = Storage {
  blocks :: BlockStore h,
  namespace :: Namespace key fingerprint h,
  keygen :: IO key,
  staticKey :: key,
  sign :: Name -> h -> Nonce -> Authorization
}
