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
  , t "1-1" $ [Numeric "1", SymbolyId "-", Numeric "1"]
  , t "1+1" $ [Numeric "1", SymbolyId "+", Numeric "1"]
  , t "1 +1" $ [Numeric "1", Numeric "+1"]
  , t "1+ 1" $ [Numeric "1", SymbolyId "+", Numeric "1"]
  , t "x+y" $ [WordyId "x", SymbolyId "+", WordyId "y"]
  , t "[+1,+1]" $ [Reserved "[", Numeric "+1", Reserved ",", Numeric "+1", Reserved "]"]
  , t "[ +1 , +1 ]" $ [Reserved "[", Numeric "+1", Reserved ",", Numeric "+1", Reserved "]"]
  , t "-- a comment 1.0" $ []
  , t "\"woot\" -- a comment 1.0" $ [Textual "woot"]
  , t "0:Int" $ [Numeric "0", Reserved ":", WordyId "Int"]
  , t "0 : Int" $ [Numeric "0", Reserved ":", WordyId "Int"]
  , t ".Foo Foo . .foo.bar.baz" [WordyId ".Foo", WordyId "Foo", SymbolyId ".", WordyId ".foo.bar.baz"]
  , t ".Foo.Bar.+" [SymbolyId ".Foo.Bar.+"]

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

  -- directly close empty = block
  , let ex = unlines
          [ "test ="
          , ""
          , "x = 1"]
    in t ex [ WordyId "test", Open "=", Close, Semi,
              WordyId "x", Open "=", Numeric "1", Close]

  -- directly close nested empty blocks
  , let ex = unlines
          [ "test ="
          , "  test2 ="
          , ""
          , "x = 1"]
    in t ex [ WordyId "test",Open "=",WordyId "test2",Open "=",Close,Close,Semi,
              WordyId "x",Open "=",Numeric "1",Close]

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
  -- Test string literals
  , t "\"simple string without escape characters\"" [Textual "simple string without escape characters"]
  , t "\"test escaped quotes \\\"in quotes\\\"\"" [Textual "test escaped quotes \"in quotes\""]
  , t "\"\\n \\t \\b \\a\"" [Textual "\n \t \b \a"]
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
