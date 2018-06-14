{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Unison.Test.Typechecker where

import  EasyTest
import  Data.Char (isSpace)
import  Data.Either (isRight)
import  Unison.FileParsers (parseAndSynthesizeAsFile)
import  Unison.Symbol
import  Unison.Test.Common
import  Text.RawString.QQ

test = scope "typechecker" . tests $
  [
    c "x -> x"
      "forall a . a -> a"

  , c "x y -> x"
      "forall a b . a -> b -> a"

  , c "(+_Int64)"
      "Int64 -> Int64 -> Int64"

  , c "3"
      "UInt64"

  , c "+3"
      "Int64"

  , c "3.0"
      "Float"

  , c "Boolean.not true"
      "Boolean"

  , c "Boolean.not"
      "Boolean -> Boolean"

  , c "\"Hello, world!\""
      "Text"

  , c "if true then 1 else 2" "UInt64"
  , c "if true then (x -> x) else (x -> x)" "forall a . a -> a"
  , c "or true false" "Boolean"
  , c "and true false" "Boolean"
  , c "[1,2,3]" "Sequence UInt64"
  , c "Stream.from-int64 +0" "Stream Int64"
  , c "(+_UInt64) 1" "UInt64 -> UInt64"
  , c [r|let
        |  (|>) : forall a b . a -> (a -> b) -> b
        |  a |> f = f a
        |
        |  Stream.from-int64 -3
        |    |> Stream.take 10
        |    |> Stream.fold-left +0 (+_Int64) |] "Int64"
  -- some pattern-matching tests we want to perform:
--  Unbound
  -- , c [r|type Optional a = None | Some a
  --        case Some 3 of
  --          x -> 1
  --      |] "UInt64"
  , bombs [r|type Optional a = None | Some a
            |
            |case Optional.Some 3 of
            |  x -> 1
            |  y -> "boo" |]
  , checks [r|type Optional a = None | Some a
             |
             |r : UInt64
             |r = case Optional.Some 3 of
             |      x -> 1
             |() |]

--  Var
--  Boolean !Bool
--  Int64 !Int64
--  UInt64 !Word64
--  Float !Double
--  Constructor !Reference !Int [Pattern]
--  As Pattern
--  nested ones
--  multiple cases
--  guards

--  EffectPure Pattern
--  EffectBind !Reference !Int [Pattern] Pattern--
  ]
  where c tm typ = scope tm . expect $ check (stripMargin tm) typ
        bombs s = scope s (expect . not . fileTypechecks $ s)
        checks s = scope s (typer $ s)
        typeFile = (parseAndSynthesizeAsFile @ Symbol) "<test>" .  stripMargin
        typer = either crash (const ok) . snd . typeFile
        fileTypechecks = isRight . snd . typeFile
        stripMargin =
          unlines . map (dropWhile (== '|'). dropWhile isSpace) . lines


