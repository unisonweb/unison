module Unison.Dimensions where

newtype X = X Word deriving (Eq,Ord)
newtype Y = Y Word deriving (Eq,Ord)
newtype Width = Width Word deriving (Eq,Ord)
newtype Height = Height Word deriving (Eq,Ord)

class Ord t => Size t where
  plus :: t -> t -> t
  minus :: t -> t -> t
  zero :: t

instance Size Width where
  plus (Width w) (Width w2) = Width (w + w2)
  minus w w2 | w2 >= w = zero
  minus (Width w) (Width w2) = Width (w - w2)
  zero = Width 0

instance Size Height where
  plus (Height w) (Height w2) = Height (w + w2)
  minus w w2 | w2 >= w = zero
  minus (Height w) (Height w2) = Height (w - w2)
  zero = Height 0

instance Size X where
  plus (X w) (X w2) = X (w + w2)
  minus w w2 | w2 >= w = zero
  minus (X w) (X w2) = X (w - w2)
  zero = X 0

instance Size Y where
  plus (Y w) (Y w2) = Y (w + w2)
  minus w w2 | w2 >= w = zero
  minus (Y w) (Y w2) = Y (w - w2)
  zero = Y 0

instance Monoid Height where
  mempty = zero
  mappend = plus
