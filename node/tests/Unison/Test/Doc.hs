module Unison.Test.Doc where

import Unison.Doc
import Test.Tasty
import Unison.Dimensions
-- import Test.Tasty.SmallCheck as SC
-- import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

fmt :: Word -> Doc String [Int] -> String
fmt w d = formatString (Width $ fromIntegral w) d

tests :: TestTree
tests = testGroup "Doc"
  [ testCase "fits (1)" $ assertEqual "should fit on one line"
      "a b c"
      (fmt 10 (sep' " " ["a", "b", "c"]))
  , testCase "breaks (1)" $ assertEqual "should break onto 3 lines"
      "a\nb\nc"
      (fmt 4 (sep' " " ["a", "b", "c"]))
  , testCase "fits (2)" $ assertEqual "should fit on one line"
      "a b c d e"
      (fmt 9 (sep " " [embed "a", sep' " " ["b", "c", "d"], embed "e"]))
  , testCase "breaks (2)" $ assertEqual "should break onto 3 lines"
      "a\nb c d\ne"
      (fmt 8 (sep " " [embed "a", sep' " " ["b", "c", "d"], embed "e"]))
  , testCase "breaks (3)" $ assertEqual "should break onto 3 lines with indent"
      "a\n  b c d\ne"
      (fmt 8 (sep " " [embed "a", nest "  " $ sep' " " ["b", "c", "d"], embed "e"]))
  , testCase "fits (3)" $ assertEqual "should fit on one line"
      "a b c d e"
      (fmt 9 (sep " " [embed "a", nest "  " $ sep' " " ["b", "c", "d"], embed "e"]))
  ]

main = defaultMain tests
