module Unison.Util.Range where

import Unison.Lexer (Pos(..))

-- | True if `_x` contains `_y`
contains :: Range -> Range -> Bool
contains _x@(Range a b) _y@(Range c d) = a <= c && c <= b && a <= d && d <= b

overlaps :: Range -> Range -> Bool
overlaps (Range a b) (Range c d) = (a <= c && c <= b) || (c <= a && a <= d)

inRange :: Pos -> Range -> Bool
inRange p r = contains r (Range p p)

isMultiLine :: Range -> Bool
isMultiLine (Range (Pos startLine _) (Pos endLine _)) = startLine < endLine

data Range = Range { start :: Pos, end :: Pos } deriving (Eq, Ord, Show)

instance Semigroup Range where
  (Range start end) <> (Range start2 end2) =
    Range (min start start2) (max end end2)
