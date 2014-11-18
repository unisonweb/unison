{-# LANGUAGE TemplateHaskell #-}

module Unison.Syntax.Distance where

import Data.Aeson.TH

data D d
  = Quantum
  | Centimeters Float
  | Scale Float d
  | Ceiling d
  | Floor d
  | Max d d
  | Min d d deriving (Eq,Ord,Show)

{-| A data type for absolute and relative real world distances
    on a a quantized, finite segment of the real number line. -}
data Relative = Fraction Float | Embed (D Relative) deriving (Eq,Ord,Show)

{-| A data type for absolute real world distances on a a quantized,
    finite segment of the real number line. -}
data Absolute = Absolute (D Absolute) deriving (Eq,Ord,Show)

deriveJSON defaultOptions ''D
deriveJSON defaultOptions ''Relative
deriveJSON defaultOptions ''Absolute
