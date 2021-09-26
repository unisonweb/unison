{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE ViewPatterns   #-}

module Unison.Hashing.V1.Reference
  ( Reference (Builtin, Derived),
    pattern DerivedId,
    Id (Id),
    CycleSize,
    components,
  )
where

import Unison.Prelude

import qualified Data.Text as Text
import qualified Unison.Hash as H
import Unison.Hashable as Hashable (Hashable (..), Token (Bytes, Nat, Tag, Text))
import Unison.ShortHash (ShortHash)
import qualified Unison.ShortHash as SH

-- | Either a builtin or a user defined (hashed) top-level declaration.
--
-- Used for both terms and types. Doesn't distinguish between them.
--
-- Other used defined things like local variables don't get @Reference@s.
data Reference
  = Builtin Text.Text
  -- `Derived` can be part of a strongly connected component.
  -- The `Pos` refers to a particular element of the component
  -- and the `Size` is the number of elements in the component.
  -- Using an ugly name so no one tempted to use this
  | DerivedId Id deriving (Eq,Ord,Generic)

pattern Derived :: H.Hash -> Pos -> CycleSize -> Reference
pattern Derived h i n = DerivedId (Id h i n)

{-# COMPLETE Builtin, Derived #-}

-- | @Pos@ is a position into a cycle of size @Size@, as cycles are hashed together.
data Id = Id H.Hash Pos CycleSize deriving (Generic)

-- todo: move these to ShortHash module?
-- but Show Reference currently depends on SH
toShortHash :: Reference -> ShortHash
toShortHash (Builtin b) = SH.Builtin b
toShortHash (Derived h _ 1) = SH.ShortHash (H.base32Hex h) Nothing Nothing
toShortHash (Derived h i n) = SH.ShortHash (H.base32Hex h) index Nothing
  where
    -- todo: remove `n` parameter; must also update readSuffix
    index = Just $ showSuffix i n

-- (3,10) encoded as "3c10"
-- (0,93) encoded as "0c93"
showSuffix :: Pos -> CycleSize -> Text
showSuffix i n = Text.pack $ show i <> "c" <> show n

type Pos = Word64
type CycleSize = Word64

component :: H.Hash -> [k] -> [(k, Id)]
component h ks = let
  size = fromIntegral (length ks)
  in [ (k, (Id h i size)) | (k, i) <- ks `zip` [0..]]

components :: [(H.Hash, [k])] -> [(k, Id)]
components sccs = uncurry component =<< sccs

instance Show Id where show = SH.toString . SH.take 5 . toShortHash . DerivedId
instance Show Reference where show = SH.toString . SH.take 5 . toShortHash

instance Hashable.Hashable Reference where
  tokens (Builtin txt) = [Hashable.Tag 0, Hashable.Text txt]
  tokens (DerivedId (Id h i n)) = [Hashable.Tag 1, Hashable.Bytes (H.toBytes h), Hashable.Nat i, Hashable.Nat n]

-- | Two references mustn't differ in cycle length only.
instance Eq Id where x == y = compare x y == EQ
instance Ord Id where Id h i _ `compare` Id h2 i2 _  = compare h h2 <> compare i i2
