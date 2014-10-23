module Elmz.Distance where

{-| A data type for absolute and relative real world distances
    on a a quantized, finite segment of the real number line. -}
data Distance
  = Quantum
  | Centimeters Float
  | Scale Float Distance
  | Ceiling Distance
  | Floor Distance
  | Fraction Float
  | Max Distance Distance
  | Min Distance Distance

{-| A distance is a denotation for a function that receives the quantum, and
    the maximum distance supported, and returns a distance in centimeters. -}
centimeters : Distance -> Float -> Float -> Float
centimeters d quantum dmax = case d of
  Quantum       -> quantum
  Centimeters d -> d
  Scale k d     -> k * centimeters d quantum dmax
  Ceiling d     -> toFloat (ceiling (centimeters d quantum dmax / quantum)) * quantum
  Floor d       -> toFloat (floor (centimeters d quantum dmax / quantum)) * quantum
  Fraction k    -> dmax * k
  Max d1 d2     -> centimeters d1 quantum dmax `max` centimeters d2 quantum dmax
  Min d1 d2     -> centimeters d1 quantum dmax `min` centimeters d2 quantum dmax
