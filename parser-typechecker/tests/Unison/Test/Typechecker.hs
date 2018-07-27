{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Unison.Test.Typechecker where

import           Control.Monad (join)
import           Data.Char (isSpace)
import           Data.List (intercalate)
import           EasyTest
import           Text.RawString.QQ
import           Unison.FileParsers (parseAndSynthesizeAsFile)
import           Unison.PrintError (printNoteWithSource)
import qualified Unison.Result as Result
import           Unison.Symbol
import           Unison.Test.Common

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
  , bombs [r|--unresolved symbol
            |let
            |  (|>) : forall a b . a -> (a -> WHat) -> b -- unresolved symbol
            |  a |> f = f a
            |
            |  Stream.from-int64 -3
            |    |> Stream.take 10
            |    |> Stream.fold-left +0 (+_Int64) |]
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
  --       |case Some 3 of
  --       |  x -> 1
  --       |] "UInt64"
  , bombs [r|--mismatched case result types
            |type Optional a = None | Some a
            |
            |case Optional.Some 3 of
            |  x -> 1
            |  y -> "boo" |]
  , checks [r|--r1
             |type Optional a = None | Some a
             |r1 : UInt64
             |r1 = case Optional.Some 3 of
             |  x -> 1
             |() |]
  , checks [r|--r0
             |r1 : UInt64
             |r1 = case Optional.Some 3 of
             |  x -> 1
             |42|]
  , checks [r|--r2
             |type Optional a = None | Some a
             |r2 : UInt64
             |r2 = case Optional.Some true of
             |  Optional.Some true -> 1
             |  Optional.Some false -> 0
             |() |]
  , checks [r|--r3
             |type Optional a = None | Some a
             |r3 : UInt64
             |r3 = case Optional.Some true of
             |  Optional.Some true -> 1
             |  Optional.Some false -> 0
             |() |]
  , checks [r|r4 : Int64 -> Int64
             |r4 x = case x of
             |  +1 -> -1
             |  _  -> Int64.negate x
             |() |]
  , checks [r|r5 : Float
             |r5 = case 2.2 of
             |  2.2 -> 3.0
             |  _  -> 1.0
             |() |]
  , checks [r|r6 : ()
             |r6 = case () of
             |  () -> ()
             |() |]
  , checks [r|r7 : ()
             |r7 = case () of
             |  x@() -> x
             |() |]
  , checks [r|r8 : UInt64
             |r8 = case (1,(2,(3,(4,(5,(6,(7,8))))))) of
             |  (x,(y,(_,_))) -> 0
             |() |]
  , checks [r|r9 : UInt64
             |r9 = case 1 of
             |  9 -> 9
             |  8 -> 8
             |  7 -> 7
             |  6 -> 6
             |  5 -> 5
             |  _ -> 1
             |() |]
  , checks [r|r10 : UInt64
             |r10 = case 1 of
             |  1 | true -> 3
             |  _ -> 4
             |() |]
  , checks [r|r11 : UInt64
             |r11 = case 1 of
             |  1 | 2 ==_UInt64 3 -> 4
             |  _ -> 5
             |() |]
  , checks [r|r12 : UInt64
             |r12 = (x -> x) 64
             |() |]
  , checks [r|id : forall a . a -> a
             |id x = x
             |() |]
  , checks [r|r13 : (UInt64, Text)
             |r13 =
             |  id = ((x -> x): forall a . a -> a)
             |  (id 10, id "foo")
             |() |]
  , checks [r|r14 : (forall a . a -> a) -> (UInt64, Text)
             |r14 id = (id 10, id "foo")
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

  , checks [r|--Abort
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
  , checks [r|--State1 effect
             |effect State se2 where
             |  put : ∀ se . se -> {State se} ()
             |  get : ∀ se . () -> {State se} se
             |
             |-- state : ∀ s a . s -> Effect (State s) a -> (s, a)
             |state woot eff = case eff of
             |  { State.put snew -> k } -> handle (state snew) in k ()
             |  { State.get () -> k } -> handle state woot in k woot
             |  { a } -> (woot, a)
             |
             |blah : ∀ s a . s -> Effect (State s) a -> (s, a)
             |blah = state
             |()
             |]
   , checks [r|--State1a effect
             |effect State se2 where
             |  put : ∀ se . se -> {State se} ()
             |  get : ∀ se . {State se} se
             |
             |id : Int64 -> Int64
             |id i = i
             |
             |foo : () -> {State Int64} Int64
             |foo unit = id (State.get +_Int64 State.get)
             |
             |()
             |]
  , checks [r|--State2 effect
             |effect State se2 where
             |  put : ∀ se . se -> {State se} ()
             |  get : ∀ se . () -> {State se} se
             |
             |state : ∀ s a . s -> Effect (State s) a -> (s, a)
             |state woot eff = case eff of
             |  { State.get () -> k } -> handle (state woot) in (k woot)
             |  { State.put snew -> k } -> handle (state snew) in (k ())
             |  { a } -> (woot, a)
             |
             |()
             |]
  , checks [r|--State3 effect
             |effect State se2 where
             |  put : ∀ se . se -> {State se} ()
             |  get : ∀ se . () -> {State se} se
             |
             |state : ∀ s a . s -> Effect (State s) a -> (s, a)
             |state woot eff = case eff of
             |  { State.get () -> k } -> handle (state woot) in (k woot)
             |  { State.put snew -> k } -> handle (state snew) in (k ())
             |
             |ex1 : (UInt64, UInt64)
             |ex1 = handle (state 42) in State.get ()
             |
             |ex1a : (UInt64, UInt64)
             |ex1a = handle (state 42) in 49
             |
             |ex1b = handle (x -> 10) in 0
             |
             |ex1c : UInt64
             |ex1c = handle (x -> 10) in 0
             |
             |ex1d = handle (state 42) in 49
             |
             |ex2 = handle (state 42) in State.get ()
             |
             |ex3 : (UInt64, UInt64)
             |ex3 = ex2
             |
             |()
             |]
  , bombs  [r|--State4 effect
             |effect State se2 where
             |  put : ∀ se . se -> {State se} ()
             |  get : ∀ se . () -> {State se} se
             |
             |-- binding is not guarded by a lambda, it only can access
             |-- ambient abilities (which will be empty)
             |ex1 : {State Int64} ()
             |ex1 =
             |  y = State.get
             |  State.put (y +_Int64 +1)
             |  ()
             |
             |()
             |]
  , bombs  [r|--IO effect
             |effect IO where
             |  launch-missiles : () -> {IO} ()
             |
             |-- binding is not guarded by a lambda, it only can access
             |-- ambient abilities (which will be empty)
             |ex1 : {IO} ()
             |ex1 = launch-missiles()
             |
             |()
             |]
  , bombs  [r|--IO/State1 effect
             |effect IO where
             |  launch-missiles : {IO} ()
             |
             |effect State se2 where
             |  put : ∀ se . se -> {State se} ()
             |  get : ∀ se . () -> {State se} se
             |
             |foo : () -> {IO} ()
             |foo unit =
             |-- inner binding can't access outer abilities unless it declares
             |-- them explicitly
             |  inc-by : Int64 -> {State Int} ()
             |  inc-by i =
             |    launch-missiles -- not allowed
             |    y = State.get()
             |    State.put (y +_Int64 i)
             |  ()
             |
             |()
             |]
  , checks [r|--IO/State2 effect
             |effect IO where
             |  launch-missiles : {IO} ()
             |
             |effect State se2 where
             |  put : ∀ se . se -> {State se} ()
             |  get : ∀ se . {State se} se
             |
             |foo : () -> {IO} ()
             |foo unit =
             |  inc-by : Int64 -> {IO, State Int64} ()
             |  inc-by i =
             |    IO.launch-missiles -- OK, since declared by `inc-by` signature
             |    y = State.get
             |    State.put (y +_Int64 i)
             |  ()
             |
             |()
             |]
  , checks [r|--IO3 effect
             |effect IO where
             |  launch-missiles : () -> {IO} ()
             |
             |-- binding IS guarded, so its body can access whatever abilities
             |-- are declared by the type of the binding
             |-- ambient abilities (which will be empty)
             |ex1 : () -> {IO} ()
             |ex1 unit = IO.launch-missiles()
             |
             |()
             |]
  , bombs [r|--Type.apply
            |type List a = Nil | Cons a (List a)
            |
            |map : ∀ a b . (a -> b) -> List a -> List b
            |map f as = case as of
            |  List.Nil -> List.Nil
            |  List.Cons h t -> List.Cons h (map f t) -- should not typecheck, missing (f h)
            |
            |-- definitely should not typecheck!
            |map2 : ∀ a . a
            |map2 = map
            |
            |c = List.Cons
            |z = List.Nil
            |
            |ex = c 1 (c 2 (c 3 z))
            |
            |pure-map : List Int64 -- should fail, output is a `List Text`
            |pure-map = map (a -> "hi") ex
            |
            |()
            |]
  ,broken [r|--handle inference
            |effect State s where
            |  get : ∀ s . () -> {State s} s
            |  set : ∀ s . s -> {State s} ()
            |
            |state : ∀ a s . s -> Effect (State s) a -> a
            |state s e = case e of
            |  {a} -> a
            |  {State.get _ -> k} -> handle state s in k s
            |  {State.set s -> k} -> handle state s in k ()
            |
            |-- modify : ∀ s . (s -> s) -> {State s} ()
            |-- modify f = State.set (f (State.get()))
            |
            |ex : () -> {State UInt64} UInt64
            |ex blah =
            |  State.get() +_UInt64 42
            |
            |-- note this currently succeeds, the handle block
            |-- gets an inferred type of ∀ a . a, it appears that
            |-- the existential `a` which gets instantiated for the
            |-- state call never gets refined, most likely due to
            |-- missing a subtype check in handle
            |y : Text
            |y = handle state 5 in ex ()
            |
            |() |]
  , checks [r|--map/traverse
             |effect Noop where
             |  noop : ∀ a . a -> {Noop} a
             |
             |effect Noop2 where
             |  noop2 : ∀ a . a -> a -> {Noop2} a
             |
             |type List a = Nil | Cons a (List a)
             |
             |map : ∀ a b e . (a -> {e} b) -> List a -> {e} (List b)
             |map f as = case as of
             |  List.Nil -> List.Nil
             |  List.Cons h t -> List.Cons (f h) (map f t)
             |
             |c = List.Cons
             |z = List.Nil
             |
             |ex = (c 1 (c 2 (c 3 z)))
             |
             |pure-map : List Text
             |pure-map = map (a -> "hello") ex
             |
             |zappy : () -> {Noop} (List UInt64)
             |zappy u = map (zap -> (Noop.noop zap +_UInt64 1)) ex
             |
             |zappy2 : () -> {Noop, Noop2} (List UInt64)
             |zappy2 u = map (zap -> Noop.noop zap +_UInt64 Noop2.noop2 2 7) ex
             |
             |()
             |]
  , checks [r|--data references builtins
             |type StringOrInt = S Text | I UInt64
             |[StringOrInt.S "YO", StringOrInt.I 1]
             |]
  ]
  where -- test entry points `c`, `checks`, `bombs`, `broken` call stripMargin
        -- before doing anything else; no other functions should have to.
        c tm typ = scope1 tm . expect $ check (stripMargin tm) typ
        scope1 s = scope (join . take 1 $ lines s)
        bombs s = scope1 s (crasher $ stripMargin s)
        broken :: String -> Test ()
        broken s = scope1 s $ pending (checks s)
        checks :: String -> Test ()
        checks s = scope1 s (typer . stripMargin $ s)
        typeFile = (parseAndSynthesizeAsFile @ Symbol) "<test>"
        crash' s e = crash $ printError s e
        typer s = either (crash' s) (const ok) . Result.toEither $ typeFile s
        crasher s =
          either (const ok) (const (crash "succeeded unexpectedly"))
            . Result.toEither $ typeFile s
        drop1If _p [] = []
        drop1If p (h:t) = if p h then t else h:t
        stripMargin =
          unlines . map (drop1If (== '|'). dropWhile isSpace) . lines

printError s = intercalate "\n------\n" . map (printNoteWithSource s)
