module Unison.Hashing.V2.Reference
  ( Reference (..),
    pattern ReferenceDerived,
    ReferenceId (..),
    components,
  )
where

import qualified Data.Text as Text
import Unison.Hash (Hash)
import qualified Unison.Hash as Hash
import Unison.Hashing.V2.Tokenizable (Tokenizable)
import qualified Unison.Hashing.V2.Tokenizable as Hashable
import Unison.Prelude

-- | Either a builtin or a user defined (hashed) top-level declaration.
--
-- Used for both terms and types. Doesn't distinguish between them.
--
-- Other used defined things like local variables don't get @Reference@s.
data Reference
  = ReferenceBuiltin Text.Text
  | -- `Derived` can be part of a strongly connected component.
    -- The `Pos` refers to a particular element of the component
    -- and the `Size` is the number of elements in the component.
    -- Using an ugly name so no one tempted to use this
    ReferenceDerivedId ReferenceId
  deriving stock (Eq, Ord, Show)

type Pos = Word64

pattern ReferenceDerived :: Hash -> Pos -> Reference
pattern ReferenceDerived h i = ReferenceDerivedId (ReferenceId h i)

{-# COMPLETE ReferenceBuiltin, ReferenceDerived #-}

-- | @Pos@ is a position into a cycle of size @Size@, as cycles are hashed together.
data ReferenceId
  = ReferenceId Hash Pos
  deriving stock (Eq, Ord, Show)

component :: Hash -> [k] -> [(k, ReferenceId)]
component h ks =
  [(k, (ReferenceId h i)) | (k, i) <- ks `zip` [0 ..]]

components :: [(Hash, [k])] -> [(k, ReferenceId)]
components sccs = uncurry component =<< sccs

instance Tokenizable Reference where
  tokens (ReferenceBuiltin txt) = [Hashable.Tag 0, Hashable.Text txt]
  tokens (ReferenceDerivedId (ReferenceId h i)) = [Hashable.Tag 1, Hashable.Bytes (Hash.toByteString h), Hashable.Nat i]
