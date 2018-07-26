module Unison.Test.Lexer where

import EasyTest
import Unison.Lexer

test :: Test ()
test = scope "lexer" . tests $
  [ t "1" $ [Numeric "1"]
  , t "+1" $ [Numeric "+1"]
  , t "-1" $ [Numeric "-1"]
  , t "-1.0" $ [Numeric "-1.0"]
  , t "+1.0" $ [Numeric "+1.0"]
  , t "-- a comment 1.0" $ []
  , t "\"woot\" -- a comment 1.0" $ [Textual "woot"]
  , t "0:Int" $ [Numeric "0", Reserved ":", WordyId "Int"]
  , t "0 : Int" $ [Numeric "0", Reserved ":", WordyId "Int"]

  -- note - these are all the same, just with different spacing
  , let ex1 = "if x then y else z"
        ex2 = unlines
          [ "if"
          , "  x"
          , "then"
          , "  y"
          , "else z" ]
        ex3 = unlines
          [ "if"
          , "  x"
          , "  then"
          , "    y"
          , "else z" ]
        ex4 = unlines
          [ "if"
          , "  x"
          , "  then"
          , "  y"
          , "else z" ]
        expected = [Open "if", WordyId "x", Close, Open "then", WordyId "y", Close, Open "else", WordyId "z", Close]
    in tests $ map (`t` expected) [ex1, ex2, ex3, ex4]

  , let ex = unlines [ "if a then b"
                     , "else if c then d"
                     , "else if e then f"
                     , "else g" ]
    in t ex [ Open "if", WordyId "a", Close, Open "then", WordyId "b", Close, Open "else"
            , Open "if", WordyId "c", Close, Open "then", WordyId "d", Close, Open "else"
            , Open "if", WordyId "e", Close, Open "then", WordyId "f", Close, Open "else"
            , WordyId "g", Close, Close, Close ] -- close of the three `else` blocks

  -- In an empty `then` clause, the `else` is interpreted as a `Reserved` token
  , t "if x then else" [Open "if", WordyId "x", Close, Open "then", Reserved "else", Close]
  -- Empty `else` clause
  , t "if x then 1 else" [Open "if", WordyId "x", Close, Open "then", Numeric "1", Close, Open "else", Close]
  ]

t :: String -> [Lexeme] -> Test ()
t s expected =
  let actual0 = payload <$> lexer "ignored filename" s
      actual = take (length actual0 - 2) . drop 1 $ actual0
  in scope s $
      if actual == expected then ok
      else do note $ "expected: " ++ show expected
              note $ "actual  : "   ++ show actual
              crash "actual != expected"
