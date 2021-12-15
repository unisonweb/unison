{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
module Unison.Test.Range where

import EasyTest
import Unison.Lexer (Pos (..))
import Unison.Util.Range


test :: Test ()
test = scope "range" . tests $
  [ scope "contains 11 11" . expect $ contains zero zero
  , antisymmetric "contains 11 12" (not . uncurry contains) $ (zero, one)
  , scope "contains 12 23" . expect . not $ contains one one'
  , scope "contains 23 12" . expect . not $ contains one' one

  , symmetric "overlaps 11 11" (not . uncurry overlaps) $ (zero, zero)
  , symmetric "overlaps 12 11" (not . uncurry overlaps) $ (one, zero)
  , symmetric "overlaps 12 23" (not . uncurry overlaps) $ (one, one')
  , symmetric "overlaps 12 13" (uncurry overlaps) $ (one, two)
  , symmetric "overlaps 23 13" (uncurry overlaps) $ (one', two)

  , scope "inrange 1 12" . expect $ inRange (Pos 1 1) (Range (Pos 1 1) (Pos 1 2))
  , scope "inrange 2 12" . expect . not $ inRange (Pos 1 2) (Range (Pos 1 1) (Pos 1 2))
  ]
  where symmetric s f (a,b) =
          tests [ scope s . expect $ f (a, b)
                , scope (s ++ " (symmetric)") . expect $ f (b, a)]
        antisymmetric s f (a,b) =
          tests [ scope s . expect $ f (a, b)
                , scope (s ++ " (antisymmetric)") . expect . not $ f (b, a)]
        zero = Range (Pos 1 1) (Pos 1 1)
        one = Range (Pos 1 1) (Pos 1 2)
        one' = Range (Pos 1 2) (Pos 1 3)
        two = Range (Pos 1 1) (Pos 1 3)
