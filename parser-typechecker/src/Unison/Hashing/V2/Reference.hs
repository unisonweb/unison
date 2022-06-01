{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Hashing.V2.Reference
  ( Reference,
    pattern Builtin,
    pattern Derived,
    pattern DerivedId,
    Id (..),
    components,
  )
where

import qualified Data.Text as Text
import qualified Unison.Hash as H
import Unison.Hashing.V2.Tokenizable (Tokenizable)
import qualified Unison.Hashing.V2.Tokenizable as Hashable
import Unison.Prelude
import Unison.ShortHash (ShortHash)
import qualified Unison.ShortHash as SH

-- | Either a builtin or a user defined (hashed) top-level declaration.
--
-- Used for both terms and types. Doesn't distinguish between them.
--
-- Other used defined things like local variables don't get @Reference@s.
data Reference
  = Builtin Text.Text
  | -- `Derived` can be part of a strongly connected component.
    -- The `Pos` refers to a particular element of the component
    -- and the `Size` is the number of elements in the component.
    -- Using an ugly name so no one tempted to use this
    DerivedId Id
  deriving (Eq, Ord)

type Pos = Word64

pattern Derived :: H.Hash -> Pos -> Reference
pattern Derived h i = DerivedId (Id h i)

{-# COMPLETE Builtin, Derived #-}

-- | @Pos@ is a position into a cycle of size @Size@, as cycles are hashed together.
data Id = Id H.Hash Pos deriving (Eq, Ord)

-- todo: delete these, but `instance Show Reference` currently depends on SH
toShortHash :: Reference -> ShortHash
toShortHash (Builtin b) = SH.Builtin b
toShortHash (Derived h 0) = SH.ShortHash (H.base32Hex h) Nothing Nothing
toShortHash (Derived h i) = SH.ShortHash (H.base32Hex h) (Just $ showSuffix i) Nothing

showSuffix :: Pos -> Text
showSuffix = Text.pack . show

-- | Builds 'Id's for the definitions in a component.
component :: H.Hash -> [k] -> [(k, Id)]
component h ks =
  let
   in [(k, (Id h i)) | (k, i) <- ks `zip` [0 ..]]

-- | Build 'Id's for each piece of each component.
components :: [(H.Hash, [k])] -> [(k, Id)]
components sccs = uncurry component =<< sccs

instance Show Id where show = SH.toString . SH.take 5 . toShortHash . DerivedId

instance Show Reference where show = SH.toString . SH.take 5 . toShortHash

instance Tokenizable Reference where
  tokens (Builtin txt) = [Hashable.Tag 0, Hashable.Text txt]
  tokens (DerivedId (Id h i)) = [Hashable.Tag 1, Hashable.Bytes (H.toByteString h), Hashable.Nat i]
