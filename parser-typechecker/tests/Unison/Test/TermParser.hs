{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Unison.Test.TermParser where

import qualified Data.Map as Map
import           EasyTest
import           Text.RawString.QQ
import           Unison.Parsers (unsafeParseTerm)
import qualified Unison.Reference as R
import           Unison.Symbol (Symbol)
import           Unison.Term (Term)


test = scope "termparser" . tests . map parses $
  [ "1"
  , "1.0"
  , "+1"
  , "-1"
  , "-1.0"
  , "4th"
  , "()"
  , "forty"
  , "forty two"
  , "\"forty two\""
  , "( one ; two )"
  , "( one ; two )"
  , "( one ; two ; three )"
  , "( one ; two ; 42 )"
  , "[1,2,3]"
  , "\"abc\""
  , "x + 1"
  , "1 + 1"
  , "1 +_UInt64 1"
  , "( x + 1 )"
  , "foo 42"

  -- Block tests
  , "let x = 1\n" ++
    "    x"
  , "let\n" ++
    " y = 1\n" ++
    " x"
  , unlines [
    "let y = 1  ",
    "    x = 2  ",
    "    x + y"]
  , "(let \n" ++
    "  x = 23 + 42\n" ++
    "  x + 1 \n)"

  -- Handlers
  ,"handle foo in \n" ++
    "  x = 23 + 42\n" ++
    "  x + foo 8 102.0 +4"
  ,"handle foo in \n" ++
    "  x = 1\n" ++
    "  x"
  , "handle foo in x"

  -- Patterns
  , "case x of 0 -> 1"
  , "case x of\n" ++
    "  0 -> 1"
  , "case x of\n" ++
    "  x -> 1\n" ++
    "  2 -> 7\n" ++
    "  _ -> 3\n" ++
    "  Pair x y -> x + y\n" ++
    "  Pair (Pair x y) _ -> x + y \n"
  , "case x of\n" ++
    "  {Pair x y} -> 1\n" ++
    "  {State.set 42 -> k} -> k 42\n"
  , "case x of\n" ++
    "  0 ->\n" ++
    "    z = 0\n" ++
    "    z"
  , "case x of\n" ++
    " 0 | 1 == 2 -> 123"

  -- Conditionals
  , "if x then y else z"
  , "if\n" ++
    "  s = 0\n" ++
    "  s > 0\n" ++
    "then\n" ++
    "  s = 0\n" ++
    "  s + 1\n" ++
    "else\n" ++
    "  s = 0\n" ++
    "  s + 2\n"
  , "if\n" ++
    "  s = 0\n" ++
    "  s > 0\n" ++
    "then\n" ++
    "  s : Int64\n" ++
    "  s = (0: Int64)\n" ++
    "  s + 1\n" ++
    "else\n" ++
    "  s = 0\n" ++
    "  s + 2\n"
  , "and x y"
  , "or x y"
  , [r|let
        increment = (+_UInt64) 1

        (|>) : forall a . a -> (a -> b) -> b
        a |> f = f a

        Stream.from-int64 -3
          |> Stream.take 10
          |> Stream.fold-left 0 increment
        |]
  ]

builtins = Map.fromList
  [("Pair", (R.Builtin "Pair", 0)),
   ("State.set", (R.Builtin "State", 0))]

parses s = scope s $ do
  let p = unsafeParseTerm s builtins :: Term Symbol
  noteScoped $ "parsing: " ++ s ++ "\n  " ++ show p
  ok
