{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
module Unison.Util.Range where

import Unison.Lexer.Pos (Pos(..))

-- | True if `_x` contains `_y`
contains :: Range -> Range -> Bool
_x@(Range a b) `contains` _y@(Range c d) = a <= c && d <= b

overlaps :: Range -> Range -> Bool
overlaps (Range a b) (Range c d) = a < d && c < b

inRange :: Pos -> Range -> Bool
inRange p (Range a b) = p >= a && p < b

isMultiLine :: Range -> Bool
isMultiLine (Range (Pos startLine _) (Pos endLine _)) = startLine < endLine

data Range = Range { start :: Pos, end :: Pos } deriving (Eq, Ord, Show)

startingLine :: Range -> Range
startingLine r@(Range start@(Pos startLine _) (Pos stopLine _)) =
  if stopLine == startLine then r
  else Range start (Pos (startLine+1) 0)

instance Semigroup Range where
  (Range start end) <> (Range start2 end2) =
    Range (min start start2) (max end end2)
