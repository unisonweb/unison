module Elmz.Distance (Absolute, absolutePixels, centimeters, full, half, Relative, relativePixels) where

data D d
  = Quantum
  | Centimeters Float
  | Scale Float d
  | Ceiling d
  | Floor d
  | Max d d
  | Min d d

{-| A data type for absolute and relative real world distances
    on a a quantized, finite segment of the real number line. -}
data Relative = Fraction Float | Embed (D Relative)

{-| A data type for absolute real world distances on a a quantized,
    finite segment of the real number line. -}
data Absolute = Absolute (D Absolute)

full : Relative
full = Fraction 1.0

half : Relative
half = Fraction 0.5

{-| A distance is denoted by a function that receives the quantum, and
    the maximum distance supported, and returns a distance in centimeters. -}
centimeters : Relative -> Float -> Float -> Float
centimeters d quantum dmax = case d of
  Fraction k    -> dmax * k
  Embed d       -> case d of
    Quantum       -> quantum
    Centimeters d -> d
    Scale k d     -> k * centimeters d quantum dmax
    Ceiling d     -> toFloat (ceiling (centimeters d quantum dmax / quantum)) * quantum
    Floor d       -> toFloat (floor (centimeters d quantum dmax / quantum)) * quantum
    Max d1 d2     -> centimeters d1 quantum dmax `max` centimeters d2 quantum dmax
    Min d1 d2     -> centimeters d1 quantum dmax `min` centimeters d2 quantum dmax

{-| Convert a `Distance` to a number of pixels (rounding up if inexact),
    given an available number of pixels and a pixels per inch. -}
relativePixels : Relative -> Int -> Int -> Int
relativePixels d availablePixels pixelsPerInch =
  let widthInInches = toFloat availablePixels / toFloat pixelsPerInch
      widthInCm = widthInInches * 2.54
      pixelCm = widthInCm / toFloat availablePixels
      cm = centimeters d pixelCm widthInCm
  in ceiling (cm / pixelCm)

absoluteCentimeters : Absolute -> Float -> Float
absoluteCentimeters (Absolute d) quantum = case d of
  Quantum       -> quantum
  Centimeters d -> d
  Scale k d     -> k * absoluteCentimeters d quantum
  Ceiling d     -> toFloat (ceiling (absoluteCentimeters d quantum  / quantum)) * quantum
  Floor d       -> toFloat (floor (absoluteCentimeters d quantum  / quantum)) * quantum
  Max d1 d2     -> absoluteCentimeters d1 quantum  `max` absoluteCentimeters d2 quantum
  Min d1 d2     -> absoluteCentimeters d1 quantum  `min` absoluteCentimeters d2 quantum

{-| Convert a relative `Distance` to a number of pixels (rounding up if inexact),
    given a number of pixels per inch. -}
absolutePixels : Absolute -> Int -> Int
absolutePixels d pixelsPerInch =
  let pixelsPerCm = toFloat pixelsPerInch / 2.54
  in ceiling (absoluteCentimeters d (1.0 / pixelsPerCm))
