This transcript verifies that the pretty-printer produces code that can be successfully parsed, for a variety of examples. Terms or types that fail to round-trip can be added here as regression tests. Add tests at the bottom of this

```ucm:hide
.> builtins.mergeio
.> load unison-src/transcripts-using-base/base.u
```

## How to use this transcript: checking round-trip for inline definitions

```unison:hide
x = 1 + 1
```

```ucm
.> add
.> edit x
.> reflog
.> reset-root 2
```

Resetting the namespace after each example ensures they don't interact at all, which is probably what you want.

The `load` command which does parsing and typechecking of the `edit`'d definitions needs to be in a separate stanza from the `edit` command.

```ucm
.> load scratch.u
```

## How to use this transcript: checking round-trip for definitions from a file

Examples can also be loaded from `.u` files:

```ucm
.> load unison-src/transcripts-round-trip/ex2.u
.> add
```

When loading definitions from a file, an empty stanza like this will ensure that this empty file is where the definitions being `edit`'d will get dumped.

```unison:hide
-- empty scratch file, `edit` will target this
```

Without the above stanza, the `edit` will send the definition to the most recently loaded file, which would be `ex2.u`, making the transcript not idempotent.

```ucm
.> edit b
.> reflog
.> reset-root 2
```

```ucm
.> load scratch.u
```

No reason you can't load a bunch of definitions from a single `.u` file in one go, the only thing that's annoying is you'll have to `find` and then `edit 1-11` in the transcript to load all the definitions into the file.

## Destructuring binds

Regression test for https://github.com/unisonweb/unison/issues/2337

```unison:hide
unique type Blah = Blah Boolean Boolean

f : Blah -> Boolean
f x = let
  (Blah.Blah a b) = x
  a
```

```ucm
.> add
.> edit Blah f
.> reflog
.> reset-root 2
```

``` ucm
.> load scratch.u
```

## Parens around infix patterns

Regression test for https://github.com/unisonweb/unison/issues/2224

```unison:hide
f : [()] -> ()
f xs = match xs with
  x +: (x' +: rest) -> x
  _ -> ()

g : [()] -> ()
g xs = match xs with
  (rest :+ x') :+ x -> ()
  _ -> ()

h : [[()]] -> ()
h xs = match xs with
  (rest :+ (rest' :+ x)) -> x
  _ -> ()
```

```ucm
.> add
.> edit f g
.> reflog
.> reset-root 2
```

``` ucm
.> load scratch.u
```

## Type application inserts necessary parens

Regression test for https://github.com/unisonweb/unison/issues/2392

```unison:hide
unique ability Zonk where zonk : Nat
unique type Foo x y =

foo : Nat -> Foo ('{Zonk} a) ('{Zonk} b) -> Nat
foo n _ = n
```

```ucm
.> add
.> edit foo Zonk Foo
.> reflog
.> reset-root 2
```

``` ucm
.> load scratch.u
```

## Long lines with repeated operators

Regression test for https://github.com/unisonweb/unison/issues/1035

```unison:hide
foo : Text
foo =
  "aaaaaaaaaaaaaaaaaaaaaa" ++ "bbbbbbbbbbbbbbbbbbbbbb" ++ "cccccccccccccccccccccc" ++ "dddddddddddddddddddddd"
```

```ucm
.> add
.> edit foo
.> reflog
.> reset-root 2
```

``` ucm
.> load scratch.u
```

## Emphasis in docs inserts the right number of underscores

Regression test for https://github.com/unisonweb/unison/issues/2408

```unison:hide
myDoc = {{ **my text** __my text__ **MY_TEXT** ___MY__TEXT___ ~~MY~TEXT~~ **MY*TEXT** }}
```

```ucm
.> add
.> edit myDoc
.> undo
```

``` ucm
.> load scratch.u
```

## Parenthesized let-block with operator

Regression test for https://github.com/unisonweb/unison/issues/1778

```unison:hide

structural ability base.Abort where
  abort : a

(|>) : a -> (a ->{e} b) -> {e} b
a |> f = f a

handler : a -> Request {Abort} a -> a
handler default = cases
  { a }        -> a
  {abort -> _} -> default

Abort.toOptional : '{g, Abort} a -> '{g} Optional a
Abort.toOptional thunk = '(toOptional! thunk)

Abort.toOptional! : '{g, Abort} a ->{g} (Optional a)
Abort.toOptional! thunk = toDefault! None '(Some !thunk)

Abort.toDefault! : a -> '{g, Abort} a ->{g} a
Abort.toDefault! default thunk =
  h x = Abort.toDefault! (handler default x) thunk
  handle (thunk ()) with h

x = '(let
  abort
  0) |> Abort.toOptional
```

```ucm
.> add
.> edit x base.Abort |> handler Abort.toOptional Abort.toOptional! Abort.toDefault!
.> undo
```

``` ucm
.> load scratch.u
```

## Line breaks before 'let

Regression test for https://github.com/unisonweb/unison/issues/1536

```unison:hide
r = 'let
 y = 0
 y
```

```ucm
.> add
.> edit r
.> undo
```

```ucm
.> load scratch.u
```

## Raw codeblocks add indentation

Regression test for https://github.com/unisonweb/unison/issues/2271

```ucm
.> load unison-src/transcripts-round-trip/docTest2.u
.> add
```

```unison:hide
x = 2
```

```ucm
.> edit docTest2
```

```ucm
.> load scratch.u
.> add
```

## Unison Cloud roundtrip issues

Regression tests for  https://github.com/unisonweb/unison/issues/2650

```unison:hide
broken =
    addNumbers: 'Nat
    addNumbers = 'let
      use Nat +
      y = 12
      13 + y
    !addNumbers
```

``` ucm
.> add
.> edit broken
.> undo
```

``` ucm
.> load scratch.u
```

```unison:hide
tvarmodify tvar fun = ()

broken tvar =
  '(tvarmodify tvar (cases
     Some _ -> "oh boy isn't this a very very very very very very very long string?"
     None -> ""))
```

``` ucm
.> add
.> edit tvarmodify broken
.> undo
```

``` ucm
.> load scratch.u
```

```unison:hide
broken = cases
  Some loooooooooooooooooooooooooooooooooooooooooooooooooooooooong | loooooooooooooooooooooooooooooooooooooooooooooooooooooooong == 1 -> ()
  _ -> ()
```

``` ucm
.> add
.> edit broken
.> undo
```

``` ucm
.> load scratch.u
```

## Guard patterns on long lines

```unison:hide
structural type SomethingUnusuallyLong = SomethingUnusuallyLong Text Text Text

foo = let
  go x =
    'match (a -> a) x with
      SomethingUnusuallyLong lijaefliejalfijelfj aefilaeifhlei liaehjffeafijij |
        lijaefliejalfijelfj == aefilaeifhlei -> 0
      SomethingUnusuallyLong lijaefliejalfijelfj aefilaeifhlei liaehjffeafijij |
        lijaefliejalfijelfj == liaehjffeafijij -> 1
      _ -> 2
  go (SomethingUnusuallyLong "one" "two" "three")
```

```ucm
.> add
.> edit SomethingUnusuallyLong foo
.> undo
```

```ucm
.> load scratch.u
```

## Nested fences

```ucm
.> load unison-src/transcripts-round-trip/nested.u
.> add
```

```ucm
.> edit nested
.> undo
```

```ucm
.> load unison-src/transcripts-round-trip/nested.u
```

## Multiline expressions in multiliine lists

```unison:hide
foo a b c d e f g h i j = 42

use Nat +
x = [ 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
    , foo 12939233 2102020 329292 429292 522020 62929292 72020202 820202 920202 1020202 ]
```

```ucm
.> add
.> edit foo x
.> undo
```

```ucm
.> load scratch.u
```

## Delayed computations passed to a function as the last argument

When a delayed computation block is passed to a function as the last argument
in a context where the ambient precedence is low enough, we can elide parentheses
around it and use a "soft hang" to put the `'let` on the same line as the function call.
This looks nice.

    forkAt usEast do
      x = thing1
      y = thing2
      ...

vs the not as pretty but still correct:

    forkAt
      usEast
      (do
          x = thing1
          y = thing2
          ...)

Okay, here's the test, showing that we use the prettier version when possible:

```unison:hide
(+) a b = ##Nat.+ a b

foo a b = 42

bar0 x = do
  a = 1
  b = 2
  foo a 'let
    c = 3
    a + b

bar1 x = do
  a = 1
  b = 2
  foo (100 + 200 + 300 + 400 + 500 + 600 + 700 + 800 + 900 + 1000 + 1100 + 1200 + 1300 + 1400 + 1500) 'let
    c = 3
    a + b

bar2 x = do
  a = 1
  b = 2
  1 + foo a do
    c = 3
    a + b

bar3 x = do
  a = 1
  b = 2
  c = foo do
    c = 3
    a + b
  c
```

```ucm
.> add
.> edit foo bar0 bar1 bar2 bar3
.> undo
```

```ucm
.> load scratch.u
```

# Lambda as the last argument where the bound var is not free in the body

If a lambda's argument is not free in the body, the term printer counts this as
a "delay" instead of a lambda. This test makes sure that detecting this
condition lines up with the printing, so we don't detect a delay but then
go ahead and print it as a normal lambda.

```unison:hide
(+) a b = ##Nat.+ a b

afun x f = f x

roundtripLastLam =
  afun "foo" (n -> let
    _ = 1 + 1
    3
  )
```

```ucm
.> add
.> edit roundtripLastLam afun
.> undo
```

```ucm
.> load scratch.u
```

# Comment out builtins in the edit command

Regression test for https://github.com/unisonweb/unison/pull/3548

```ucm
.> alias.term ##Nat.+ plus
.> edit plus
.> undo
```

```ucm
.> load scratch.u
```

# Indent long pattern lists to avoid virtual semicolon

Regression test for https://github.com/unisonweb/unison/issues/3627

```unison:hide
(+) a b = ##Nat.+ a b

foo = cases
  aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,
   bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
    -> aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa + bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
```

```ucm
.> add
.> edit foo
.> undo
```

```ucm
.> load scratch.u
```

# Multi-line lambda let

Regression test for #3110 and #3801

```unison:hide
foreach x f = 
  _ = List.map f x
  ()

ignore x = ()

test1 : ()
test1 =
  foreach [1, 2, 3] let x -> let
      y = Nat.increment x
      ()

test2 = foreach [1, 2, 3] let x -> ignore (Nat.increment x) 

test3 = foreach [1, 2, 3] do x -> do
  y = Nat.increment x
  ()
```

```ucm
.> add
.> edit test1 test2 test3 foreach ignore
.> undo
```

```ucm
.> load scratch.u
```

# Destructuring bind in delay or lambda

Regression test for https://github.com/unisonweb/unison/issues/3710

```unison:hide
d1 = do
  (a,b) = (1,2)
  (c,d) = (3,4)
  (e,f) = (5,6)
  (a,b,c,d,e,f)

d2 = let
  (a,b) = (1,2)
  (c,d) = (3,4)
  (e,f) = (5,6)
  (a,b,c,d,e,f)

d3 x = let
  (a,b) = (1,x)
  (c,d) = (3,4)
  (e,f) = (5,6)
  (a,b,c,d,e,f)

d4 x = do
  (a,b) = (1,x)
  (c,d) = (3,4)
  (e,f) = (5,6)
  (a,b,c,d,e,f)

d5 x = match x with
  Some x -> x
  None -> bug "oops"
```

```ucm
.> add
.> edit d1 d2 d3 d4 d5
.> undo
```

```ucm
.> load scratch.u
```
