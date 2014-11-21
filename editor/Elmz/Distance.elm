module Elmz.Distance where

{-| A data type for absolute and relative distances on a quantized,
    finite segment of the real number line. -}
data Distance
  = Pixel -- the minimum distance
  | Fraction Float
  | Scale Float Distance
  | Ceiling Distance
  | Floor Distance
  | Max Distance Distance
  | Min Distance Distance

{-| Given the max distance as a multiple of the quantum distance,
    return the distance of `d` as a multiple of the quantum. -}
pixels : Distance -> Float -> Float
pixels d dmax = case d of
  Fraction k    -> dmax * k
  Pixel         -> 1.0
  Scale k d     -> k * pixels d dmax
  Ceiling d     -> toFloat (ceiling (pixels d dmax))
  Floor d       -> toFloat (floor (pixels d dmax))
  Max d1 d2     -> pixels d1 dmax `max` pixels d2 dmax
  Min d1 d2     -> pixels d1 dmax `min` pixels d2 dmax
