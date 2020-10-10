{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PatternSynonyms #-}

module U.Codebase.Reference where

import Data.Text (Text)
import Data.Word (Word64)
import qualified U.Util.Hash as Hash
import U.Util.Hash (Hash)
import U.Util.Hashable (Hashable (..))
import qualified U.Util.Hashable as Hashable

-- |This is the canonical representation of Reference
type Reference = Reference' Text Hash
type Id = Id' Hash

data Reference' t h
  = ReferenceBuiltin t
  | ReferenceDerived (Id' h)
  deriving (Eq, Ord, Show, Functor)

pattern Derived :: h -> ComponentIndex -> Reference' t h
pattern Derived h i = ReferenceDerived (Id h i)

{-# COMPLETE ReferenceBuiltin, Derived #-}

type ComponentIndex = Word64
data Id' h = Id h ComponentIndex
  deriving (Eq, Ord, Show, Functor)

instance Hashable Reference where
  tokens (ReferenceBuiltin txt) =
    [Hashable.Tag 0, Hashable.Text txt]
  tokens (ReferenceDerived (Id h i)) =
    [Hashable.Tag 1, Hashable.Bytes (Hash.toBytes h), Hashable.Nat i]

instance Hashable (Reference' Text (Maybe Hash)) where
  tokens (ReferenceBuiltin txt) =
    [Hashable.Tag 0, Hashable.Text txt]
  tokens (ReferenceDerived (Id h i)) =
    [Hashable.Tag 1, Hashable.accumulateToken h, Hashable.Nat i]
