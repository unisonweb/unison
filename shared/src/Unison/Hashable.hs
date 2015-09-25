{-# Language FlexibleInstances #-}

module Unison.Hashable where

import Data.Word (Word8)
import Data.ByteString (ByteString)
import Data.Text (Text)

data Token h
  = Tag !Word8
  | Bytes !ByteString
  | VarInt !Int
  | Text !Text
  | Double !Double
  | Hashed !h

class Hash h where
  hash :: [Token h] -> h
  fromBytes :: ByteString -> h
  toBytes :: h -> ByteString

hash' :: (Hash h, Hashable t) => t -> h
hash' = hash . tokens

class Hashable t where
  tokens :: Hash h => t -> [Token h]

class Functor f => Hashable1 f where
  -- | Produce a hash for an `f a`, given a hashing function for `a`.
  -- The first argument can be used by instances to hash `a` values
  -- whose order should not affect hash results. Its second result
  -- can be used to hash order-dependent `a` values at this layer that
  -- are not part of the cycle.
  hash1 :: (Ord h, Hash h) => ([a] -> ([h], a -> h)) -> (a -> h) -> f a -> h

instance Hashable () where
  tokens _ = []

instance Hashable Double where
  tokens d = [Double d]

instance Hashable Text where
  tokens s = [Text s]

instance Hashable ByteString where
  tokens bs = [Bytes bs]
