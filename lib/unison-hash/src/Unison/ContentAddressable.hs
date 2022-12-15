module Unison.ContentAddressable
  ( ContentAddressable (..),
  )
where

import U.Util.Hash (Hash)

-- | A type class that is inhabited by types that can compute a hash of their content.
--
-- Instances of this class should only live in dedicated "hashing packages" such as @unison-hashing-v2@.
class ContentAddressable a where
  contentHash :: a -> Hash
