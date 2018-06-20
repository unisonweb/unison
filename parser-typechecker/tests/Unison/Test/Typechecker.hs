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
             |r1 : UInt64
             |r1 = case Optional.Some 3 of
             |  x -> 1
             |
             |r2 : UInt64
             |r2 = case Optional.Some true of
             |  Optional.Some true -> 1
             |  Optional.Some false -> 0
             |
             |r3 : UInt64
             |r3 = case Optional.Some true of
             |  Optional.Some true -> 1
             |  Optional.Some false -> 0
             |
             |r4 : Int64 -> Int64
             |r4 x = case x of
             |  +1 -> -1
             |  _  -> Int64.negate x
             |
             |r5 : Float
             |r5 = case 2.2 of
             |  2.2 -> 3.0
             |  _  -> 1.0
             |
             |r6 : ()
             |r6 = case () of
             |  () -> ()
             |
             |r7 : ()
             |r7 = case () of
             |  x@() -> x
             |
             |r8 : UInt64
             |r8 = case (1,(2,(3,(4,(5,(6,(7,8))))))) of
             |  (x,(y,(_,_))) -> 0
             |
             |r9 : UInt64
             |r9 = case 1 of
             |  9 -> 9
             |  8 -> 8
             |  7 -> 7
             |  6 -> 6
             |  5 -> 5
             |  _ -> 1
             |
             |r10 : UInt64
             |r10 = case 1 of
             |  1 | true -> 3
             |  _ -> 4
             |
             |r11 : UInt64
             |r11 = case 1 of
             |  1 | 2 ==_UInt64 3 -> 4
             |  _ -> 5
             |
             |r12 : UInt64
             |r12 = (x -> x) 64
             |
             |() |]

  , checks [r|effect Abort where
             |  Abort : forall a . () -> {Abort} a
             |
             |eff : forall a b . (a -> b) -> b -> Effect Abort a -> b
             |eff f z e = case e of
             |  { Abort.Abort _ -> k } -> z
             |  { a } -> f a
             |
             |() |]

  , checks [r|-- lol
             |effect Abort where
             |  Abort : forall a . () -> {Abort} a
             |
             |eff : forall a b . (a -> b) -> b -> Effect Abort a -> b
             |eff f z e = case e of
             |  { Abort.Abort _ -> k } -> z
             |  { a } -> f a
             |
             |-- heff : UInt64
             |heff = handle eff (x -> x +_UInt64 2) 1 in Abort.Abort ()
             |
             |hudy : UInt64
             |hudy = handle eff (x -> x +_UInt64 2) 1 in 42
             |
             |bork : () -> {Abort} UInt64
             |bork = u -> 1 +_UInt64 (Abort.Abort ())
             |
             |() |]
  , checks [r|--State effect
             |effect State s where
             |  put : ∀ s . s -> {State s} ()
             |  get : ∀ s . () -> {State s} s
             |
             |state : ∀ s a . s -> Effect (State s) a -> (s, a)
             |state s eff = case eff of
             |  { State.get () -> k } -> handle (state s) in (k s)
             |  { State.put s -> k } -> handle (state s) in (k ())
             |  { a } -> (s, a)
             |
             |ex : (UInt64, UInt64)
             |ex = handle (state 42) in State.get ()
             |
             |ex
             |]
  ]
  where c tm typ = scope tm . expect $ check (stripMargin tm) typ
        bombs s = scope s (expect . not . fileTypechecks $ s)
        checks s = scope s (typer $ s)
        typeFile = (parseAndSynthesizeAsFile @ Symbol) "<test>" .  stripMargin
        typer = either crash (const ok) . typeFile
        fileTypechecks = isRight . typeFile
        stripMargin =
          unlines . map (dropWhile (== '|'). dropWhile isSpace) . lines


