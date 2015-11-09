module Unison.Test.Doc where

import Unison.Doc
import Test.Tasty
import Unison.Dimensions
-- import Test.Tasty.SmallCheck as SC
-- import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import qualified Unison.Test.Common as Common

fmt :: Word -> Doc String [Int] -> String
fmt w d = formatString (Width $ fromIntegral w) d

tests :: TestTree
tests = testGroup "Doc"
  [ testCase "ex (1)" $ assertEqual "should fit on one line"
      "a b c"
      (fmt 10 (sep' " " ["a", "b", "c"]))
  , testCase "ex (1a)" $ assertEqual "should break onto 3 lines"
      "a\nb\nc"
      (fmt 4 (sep' " " ["a", "b", "c"]))
  , testCase "ex (2)" $ assertEqual "should fit on one line"
      "a b c d e"
      (fmt 9 (sep " " [embed "a", sep' " " ["b", "c", "d"], embed "e"]))
  , testCase "ex (2a)" $ assertEqual "should break onto 3 lines"
      "a\nb c d\ne"
      (fmt 8 (sep " " [embed "a", sep' " " ["b", "c", "d"], embed "e"]))
  , testCase "ex (3)" $ assertEqual "should break onto 3 lines with indent"
      "a\n  b c d\ne"
      (fmt 8 (sep " " [embed "a", nest "  " $ sep' " " ["b", "c", "d"], embed "e"]))
  , testCase "ex (3a)" $ assertEqual "should fit on one line"
      "a b c d e"
      (fmt 9 (sep " " [embed "a", nest "  " $ sep' " " ["b", "c", "d"], embed "e"]))
  , testCase "parenthesize (1)" $ assertEqual "should fit on one line"
      "(a b c d)"
      (fmt 9 $ parenthesize True (sep " " [embed "a", nest "  " $ sep' " " ["b", "c", "d"]]))
  , testCase "parenthesize (2)" $ assertEqual "should break onto two lines with indent and no parens"
      "a\n  b"
      (fmt 3 $ parenthesize True (docs [embed "a", breakable " ", nest "  " $ embed "b"]))
  , testCase "parenthesize (3)" $ assertEqual "should break onto two lines with indent and no parens"
      "a\n  b c d"
      (fmt 7 $ parenthesize True (docs [embed "a", breakable " ", nest "  " $ sep' " " ["b", "c", "d"]]))
  ]

main = defaultMain tests
