module Unison.Dimensions where

import Data.List

newtype X = X Word deriving (Eq,Ord,Show)
newtype Y = Y Word deriving (Eq,Ord,Show)
newtype Width = Width Word deriving (Eq,Ord,Show)
newtype Height = Height Word deriving (Eq,Ord,Show)

within :: (X,Y) -> (X,Y,Width,Height) -> Bool
within (X x0, Y y0) (X x,Y y,Width w,Height h) =
  x0 >= x && x0 <= x+w && y0 >= y && y0 <= y+h

hcombine, vcombine :: (Width,Height) -> (Width,Height) -> (Width,Height)
hcombine (w1,h1) (w2,h2) = (plus w1 w2, h1 `max` h2)
vcombine (w1,h1) (w2,h2) = (w1 `max` w2, plus h1 h2)

hcombines, vcombines :: [(Width,Height)] -> (Width,Height)
hcombines = foldl' hcombine (Width 0, Height 0)
vcombines = foldl' vcombine (Width 0, Height 0)

class Ord t => Natural t where
  plus :: t -> t -> t
  minus :: t -> t -> t
  half :: t -> t
  zero :: t
  one :: t

instance Natural Width where
  plus (Width w) (Width w2) = Width (w + w2)
  minus w w2 | w2 >= w = zero
  minus (Width w) (Width w2) = Width (w - w2)
  zero = Width 0
  one = Width 1
  half (Width w) = Width (w `quot` 2)

instance Natural Height where
  plus (Height w) (Height w2) = Height (w + w2)
  minus w w2 | w2 >= w = zero
  minus (Height w) (Height w2) = Height (w - w2)
  zero = Height 0
  one = Height 1
  half (Height h) = Height (h `quot` 2)

instance Natural X where
  plus (X w) (X w2) = X (w + w2)
  minus w w2 | w2 >= w = zero
  minus (X w) (X w2) = X (w - w2)
  zero = X 0
  one = X 1
  half (X x) = X (x `quot` 2)

instance Natural Y where
  plus (Y w) (Y w2) = Y (w + w2)
  minus w w2 | w2 >= w = zero
  minus (Y w) (Y w2) = Y (w - w2)
  zero = Y 0
  one = Y 1
  half (Y y) = Y (y `quot` 2)

instance Monoid Height where
  mempty = zero
  mappend = plus
