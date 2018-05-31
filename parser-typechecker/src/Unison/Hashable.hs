{-# Language FlexibleInstances #-}

module Unison.Hashable where

import Data.Int (Int64)
import Data.Word (Word8, Word64)
import Data.ByteString (ByteString)
import Data.Text (Text)

data Token h
  = Tag !Word8
  | Bytes !ByteString
  | Int64 !Int64
  | Text !Text
  | Double !Double
  | Hashed !h
  | UInt64 !Word64

class Accumulate h where
  accumulate :: [Token h] -> h
  fromBytes :: ByteString -> h
  toBytes :: h -> ByteString

accumulateToken :: (Accumulate h, Hashable t) => t -> Token h
accumulateToken = Hashed . accumulate'

accumulate' :: (Accumulate h, Hashable t) => t -> h
accumulate' = accumulate . tokens

class Hashable t where
  tokens :: Accumulate h => t -> [Token h]

instance Hashable a => Hashable [a] where
  tokens = map accumulateToken

instance (Hashable a, Hashable b) => Hashable (a,b) where
  tokens (a,b) = [accumulateToken a, accumulateToken b]

class Functor f => Hashable1 f where
  -- | Produce a hash for an `f a`, given a hashing function for `a`.
  -- The first argument can be used by instances to hash `a` values
  -- whose order should not affect hash results. Its second result
  -- can be used to hash order-dependent `a` values at this layer that
  -- are not part of the cycle.
  hash1 :: (Ord h, Accumulate h) => ([a] -> ([h], a -> h)) -> (a -> h) -> f a -> h

instance Hashable () where
  tokens _ = []

instance Hashable Double where
  tokens d = [Double d]

instance Hashable Text where
  tokens s = [Text s]

instance Hashable ByteString where
  tokens bs = [Bytes bs]

instance Hashable Word64 where
  tokens w = [UInt64 w]

instance Hashable Int64 where
  tokens w = [Int64 w]

instance Hashable Bool where
  tokens b = [Tag . fromIntegral $ fromEnum b]
