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

class Hashable h where
  hash :: [Token h] -> h

class Functor f => Hashable1 f where
  -- | Produce a hash for an `f a`, given a hashing function for `a`.
  -- The first argument can be used by instances to hash `a` values
  -- whose order should not affect hash results. Its second result
  -- can be used to hash order-dependent `a` values at this layer that
  -- are not part of the cycle.
  hash1 :: Hashable h => ([a] -> ([h], a -> h)) -> (a -> h) -> f a -> h
