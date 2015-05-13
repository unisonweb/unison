{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Unison.Distance where

import Data.Aeson.TH
import GHC.Generics

{-| A data type for absolute and relative distances on a quantized,
    finite segment of the real number line. -}
data Distance
  = Pixel -- the minimum distance
  | Fraction Float
  | Scale Float Distance
  | Ceiling Distance
  | Floor Distance
  | Max Distance Distance
  | Min Distance Distance deriving (Eq,Ord,Show,Generic)

deriveJSON defaultOptions ''Distance
