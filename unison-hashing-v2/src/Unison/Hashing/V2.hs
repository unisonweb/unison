module Unison.Hashing.V2
  ( V2 (..),
  )
where

import Unison.ContentAddressable (ContentAddressable (..))
import Unison.Hash (Hash)
import Unison.Hashing.V2.Tokenizable (Tokenizable, hashTokenizable)

newtype V2 a
  = V2 a

-- FIXME get version in here
instance Tokenizable a => ContentAddressable (V2 a) where
  contentHash :: V2 a -> Hash
  contentHash (V2 x) = hashTokenizable x
