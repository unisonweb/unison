module Unison.Signals where

import Data.These
import Reflex
import Reflex.Dom

mergeThese :: Reflex t => Event t a -> Event t b -> Event t (These a b)
mergeThese a b = mergeWith g [fmap This a, fmap That b] where
  g (This a) (That b) = These a b
  g _ _ = error "not possible"

mergeLeft :: Reflex t => Event t a -> Event t b -> Event t (Either a b)
mergeLeft a b = mergeWith const [fmap Left a, fmap Right b]

keypress :: Reflex t => El t -> Event t Int
keypress e = domEvent Keypress e

upKeypress, downKeypress, leftKeypress, rightKeypress :: Reflex t => El t -> Event t Int
leftKeypress e  = ffilter (== 37) (keypress e)
upKeypress e    = ffilter (== 38) (keypress e)
rightKeypress e = ffilter (== 39) (keypress e)
downKeypress e  = ffilter (== 40) (keypress e)

