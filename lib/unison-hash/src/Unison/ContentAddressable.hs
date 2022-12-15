module Unison.ContentAddressable
  ( ContentAddressable (..),
  )
where

import U.Util.Hash (Hash)

-- | A type class that is inhabited by types that can compute a hash of their content.
--
-- The base instances of this class should only live in dedicated "hashing packages" such as @unison-hashing-v2@, whose
-- types and implementations should never change.
--
-- Trivial wrapper instances can be written around these, but note these pipelines from
-- @MyType ==> SomeHashingType ==> Hash@ must take care not to change the @MyType ==> SomeHashingType@ conversion, once
-- written.
--
-- For example, we might have a representation of some namespace in memory
--
-- @
-- data Namespace = Namespace Terms Types OtherStuff CachesAndWhatnot
-- @
--
-- with a somewhat equivalent "hashing" type in some "hashing package", with a ContentAddressable instance
--
-- @
-- data HashingNamespace = Namespace Terms Types
-- @
--
-- We can of course make our own convenience instance
--
-- @
-- instance ContentAddressable Namespace where
--   contentHash = contentHash . namespaceToHashingNamespace
-- @
--
-- But we must make sure that the implementation of @namespaceToHashingNamespace@ never changes the fields in the
-- corresponding @HashingNamespace@, even as features are added to or removed from @Namespace@.
class ContentAddressable a where
  contentHash :: a -> Hash
