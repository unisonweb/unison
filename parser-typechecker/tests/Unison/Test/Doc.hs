module Unison.Test.Doc where

import EasyTest
import Unison.Doc
import Unison.Dimensions

fmt :: Word -> Doc String [Int] -> String
fmt w d = formatString (Width $ fromIntegral w) d

test :: Test ()
test = scope "Doc" $ tests
  [ scope "ex (1)" $ expect ("a b c" == fmt 10 (sep' " " ["a", "b", "c"]))
  , scope "ex (1a)" $ expect ("a\nb\nc" == fmt 4 (sep' " " ["a", "b", "c"]))
  , scope "ex (2)" $ expect ("a b c d e" == fmt 9 (sep " " [embed "a", sep' " " ["b", "c", "d"], embed "e"]))
  , scope "ex (2a)" $ expect ("a\nb c d\ne" == fmt 8 (sep " " [embed "a", sep' " " ["b", "c", "d"], embed "e"]))
  , scope "ex (3)" . expect $
     "a\n  b c d\ne" == fmt 8 (sep " " [embed "a", nest "  " $ sep' " " ["b", "c", "d"], embed "e"])
  , scope "ex (3a)" . expect $
     "a b c d e" == fmt 9 (sep " " [embed "a", nest "  " $ sep' " " ["b", "c", "d"], embed "e"])
  , scope "parenthesize (1)" . expect $
      "(a b c d)" == fmt 9 (parenthesize True (sep " " [embed "a", nest "  " $ sep' " " ["b", "c", "d"]]))
  , scope "parenthesize (2)" . expect $
      "a\n  b" == fmt 3 (parenthesize True (docs [embed "a", breakable " ", nest "  " $ embed "b"]))
  , scope "parenthesize (3)" . expect $
      "a\n  b c d"
      ==
      fmt 7 (parenthesize True (docs [embed "a", breakable " ", nest "  " $ sep' " " ["b", "c", "d"]]))
  , scope "parenthesize (4)" . expect $
      "a\n  b c d"
      ==
      fmt 8 (parenthesize True (sep " " [embed "a", nest "  " $ sep' " " ["b", "c", "d"]]))
  ]

main = run test
