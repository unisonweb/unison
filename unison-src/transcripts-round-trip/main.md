This transcript verifies that the pretty-printer produces code that can be successfully parsed, for a variety of examples. Terms or types that fail to round-trip can be added here as regression tests. Add tests at the bottom of this

```ucm:hide
.> builtins.mergeio
.> load unison-src/transcripts-using-base/base.u
```

## How to use this transcript: checking round-trip for inline definitions

```unison:hide roundtrip.u
x = 1 + 1
```

```ucm
.> add
.> edit x
.> undo
```

Resetting the namespace after each example ensures they don't interact at all, which is probably what you want.

The `load` command which does parsing and typechecking of the `edit`'d definitions needs to be in a separate stanza from the `edit` command.

```ucm
.> load roundtrip.u
```

## How to use this transcript: checking round-trip for definitions from a file

Examples can also be loaded from `.u` files:

```ucm
.> load unison-src/transcripts-round-trip/ex2.u
.> add
```

When loading definitions from a file, an empty stanza like this will ensure that this empty file is where the definitions being `edit`'d will get dumped.

```unison:hide roundtrip.u
-- empty scratch file, `edit` will target this
```

Without the above stanza, the `edit` will send the definition to the most recently loaded file, which would be `ex2.u`, making the transcript not idempotent.

```ucm
.> edit b
.> undo
```

```ucm
.> load roundtrip.u
```

No reason you can't load a bunch of definitions from a single `.u` file in one go, the only thing that's annoying is you'll have to `find` and then `edit 1-11` in the transcript to load all the definitions into the file.

## Destructuring binds

Regression test for https://github.com/unisonweb/unison/issues/2337

```unison:hide roundtrip.u
unique type Blah = Blah Boolean Boolean

f : Blah -> Boolean
f x = let
  (Blah.Blah a b) = x
  a
```

```ucm
.> add
.> edit Blah f
.> undo
```

``` ucm
.> load roundtrip.u
```

## Parens around infix patterns

Regression test for https://github.com/unisonweb/unison/issues/2224

```unison:hide roundtrip.u
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
.> undo
```

``` ucm
.> load roundtrip.u
```

## Type application inserts necessary parens

Regression test for https://github.com/unisonweb/unison/issues/2392

```unison:hide roundtrip.u
unique ability Zonk where zonk : Nat
unique type Foo x y =

foo : Nat -> Foo ('{Zonk} a) ('{Zonk} b) -> Nat
foo n _ = n
```

```ucm
.> add
.> edit foo Zonk Foo
.> undo
```

``` ucm
.> load roundtrip.u
```

## Long lines with repeated operators

Regression test for https://github.com/unisonweb/unison/issues/1035

```unison:hide roundtrip.u
foo : Text
foo =
  "aaaaaaaaaaaaaaaaaaaaaa" ++ "bbbbbbbbbbbbbbbbbbbbbb" ++ "cccccccccccccccccccccc" ++ "dddddddddddddddddddddd"
```

```ucm
.> add
.> edit foo
.> undo
```

``` ucm
.> load roundtrip.u
```

## Emphasis in docs inserts the right number of underscores

Regression test for https://github.com/unisonweb/unison/issues/2408

```unison:hide roundtrip.u
myDoc = {{ **my text** __my text__ **MY_TEXT** ___MY__TEXT___ ~~MY~TEXT~~ **MY*TEXT** }}
```

```ucm
.> add
.> edit myDoc
.> undo
```

``` ucm
.> load roundtrip.u
```

## Parenthesized let-block with operator

Regression test for https://github.com/unisonweb/unison/issues/1778

```unison:hide roundtrip.u

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
.> load roundtrip.u
```

## Line breaks before 'let

Regression test for https://github.com/unisonweb/unison/issues/1536

```unison:hide roundtrip.u
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
.> load roundtrip.u
```

## Raw codeblocks add indentation

Regression test for https://github.com/unisonweb/unison/issues/2271

```ucm
.> load unison-src/transcripts-round-trip/docTest2.u
.> add
```

```unison:hide roundtrip.u
x = 2
```

```ucm
.> edit docTest2
```

```ucm
.> load roundtrip.u
.> add
```

## Unison Cloud roundtrip issues

Regression tests for  https://github.com/unisonweb/unison/issues/2650

```unison:hide roundtrip.u
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
.> load roundtrip.u
```

```unison:hide roundtrip.u
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
.> load roundtrip.u
```

```unison:hide roundtrip.u
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
.> load roundtrip.u
```

## Guard patterns on long lines

```unison:hide roundtrip.u
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
.> load roundtrip.u
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

```unison:hide roundtrip.u
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
.> load roundtrip.u
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

```unison:hide roundtrip.u
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
.> load roundtrip.u
```

# Lambda as the last argument where the bound var is not free in the body

If a lambda's argument is not free in the body, the term printer counts this as
a "delay" instead of a lambda. This test makes sure that detecting this
condition lines up with the printing, so we don't detect a delay but then
go ahead and print it as a normal lambda.

```unison:hide roundtrip.u
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
.> load roundtrip.u
```

# Comment out builtins in the edit command

Regression test for https://github.com/unisonweb/unison/pull/3548

```ucm
.> alias.term ##Nat.+ plus
.> edit plus
.> undo
```

```ucm
.> load roundtrip.u
```

# Indent long pattern lists to avoid virtual semicolon

Regression test for https://github.com/unisonweb/unison/issues/3627

```unison:hide roundtrip.u
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
.> load roundtrip.u
```

# Multi-line lambda let

Regression test for #3110 and #3801

```unison:hide roundtrip.u
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
.> load roundtrip.u
```

# Destructuring bind in delay or lambda

Regression test for https://github.com/unisonweb/unison/issues/3710

```unison:hide roundtrip.u
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
.> load roundtrip.u
```

# Avoid capture of local variables when selecting names for references

Regression test for https://github.com/unisonweb/unison/issues/525

```unison:hide roundtrip.u
-- Ex 1: 'quaffle' is a unique term suffix, but 'exampleTerm' binds 'quaffle'
-- as a local name, so the pretty-printer should use the longer name
Foo.bar.quaffle = 32

-- Notice this won't typecheck if we write 'quaffle' instead of 'Foo.bar.quaffle'
-- because 'quaffle' (the local variable) has type `Text`
exampleTerm : Text -> Nat
exampleTerm quaffle = Foo.bar.quaffle + 1

-- This demonstrates the same thing for types.
-- exampleType's signature locally binds the 'qualifiedName' type parameter,
-- so the pretty-printer should use the longer name 'Fully.qualifiedName' 
structural type Fully.qualifiedName = Dontcare () Nat

structural type Id a = Id a

exampleType : forall qualifiedName . Id qualifiedName -> Id Fully.qualifiedName
exampleType z = Id (Dontcare () 19)
```

```ucm:hide
.> add
```

We'd get a type error here if `exampleTerm` or `exampleType` didn't round-trip, but it typechecks okay! 🎉 

```ucm
.> edit exampleTerm exampleType
.> load roundtrip.u
```

```ucm:hide
.> undo
```

# Use clauses can't introduce shadowing 

```unison:hide roundtrip.u

example : Int -> Text -> Nat
example oo quaffle = 
  Foo.bar.quaffle + Foo.bar.quaffle + 1

Foo.bar.quaffle = 32

example2 : Int -> Nat
example2 oo =
  quaffle = "hi"
  Foo.bar.quaffle + Foo.bar.quaffle + Foo.bar.quaffle + 1
```

Notice there's a local name 'quaffle' of type `Text``, but the function refers to 'Foo.bar.quaffle' of type `Nat`.

```ucm
.> add
.> edit example example2
```

This just shows that we don't insert a `use Foo.bar quaffle`, even though it's referenced multiple times, since this would case shadowing.

```ucm
.> load roundtrip.u
.> undo
```

# Use clauses aren't pushed down too far

We push `use` clauses down to the nearest enclosing let or let rec block so they're close to where they're used:

```unison:hide roundtrip.u
Foo.bar.qux1 = 42
Foo'.bar.qux1 = "43" -- ensures qux1 is not a unique suffix

Foo.bar.qux2 = 44
Foo'.bar.qux2 = "45"

Foo.bar.qux3 = 46
Foo'.bar.qux3 = "47"

ex1 = 
  a = Foo.bar.qux3 + Foo.bar.qux3
  Foo.bar.qux1 + Foo.bar.qux1 + Foo.bar.qux2

ex2 = 
  a = 
    -- use Foo.bar qux3 will get pushed in here since it's already a multiline block
    z = 203993
    Foo.bar.qux3 + Foo.bar.qux3
  Foo.bar.qux1 + Foo.bar.qux1 + Foo.bar.qux2

ex3 = 
  a = do
    -- use clause gets pushed in here
    x = Foo.bar.qux3 + Foo.bar.qux3
    x + x
  ()

ex3a = 
  a = do Foo.bar.qux3 + Foo.bar.qux3 -- use clause will get pulled up to top level
  ()
```

```ucm:hide
.> add
```

```ucm
.> edit ex1 ex2 ex3 ex3a
.> load roundtrip.u
```

```ucm:hide
.> undo
```

# Use soft hangs after `with` and `=` and in last argument of function application

```unison:hide roundtrip.u
structural ability Abort where
  abort : x

ex1 = handle
  x = 1
  y = abort
  x + y
  with cases
    { a } -> a
    { Abort.abort -> _ } -> 0

ex1a = handle
  x = 1
  y = abort
  x + y
  with cases
    { a } -> "lskdfjlaksjdf al;ksdjf;lkj sa;sldkfja;sldfkj a;lsdkfj asd;lfkj "
    { Abort.abort -> _ } -> "lskdfjlaksjdf al;ksdjf;lkj sa;sldkfja;sldfkj a;lsdkfj asd;lfkj "

List.foreach x f = 0 

ex2 = List.foreach [0,1,2,3,4,5] cases
  0 -> 0
  1 -> 1
  n -> n + 100

catchAll x = 
  99

ex3 = do
  catchAll do
    x = 1
    y = 2
    x + y

ex4 = do match 0 with
  0 -> 0
  1 -> 1
  n -> n

ex5 = match Nat.increment 1 with
  2 -> "yay"
  n -> "oh no"

ex6 = List.foreach [1,2,3,4] cases
  0 -> 1
  n -> n + 1

forkAt loc c = 
  x = 99
  390439034 

ex7 somewhere = forkAt somewhere do
  x = 1
  y = 2 
  x + y

ex8 = List.foreach [0,1,2,3,4,5] cases
  0 -> 0
  1 -> 1
  n -> forkAt 0 (n + n + n + n + n + n + n + n + n + n + n)
```

```ucm:hide
.> add
```

```ucm
.> edit ex1 ex1a ex2 ex3 ex4 ex5 ex6 ex7 ex8
.> load roundtrip.u
```

```ucm:hide
.> undo
```

# Make sure use clauses don't show up before a soft hang 

Regression test for https://github.com/unisonweb/unison/issues/3883

```unison:hide roundtrip.u
unique type UUID = UUID Nat Nat

UUID.random : 'UUID
UUID.random = do UUID 0 0 

UUID.randomUUIDBytes : 'Bytes
UUID.randomUUIDBytes = do
  (UUID a b) = !UUID.random
  (encodeNat64be a) ++ (encodeNat64be b)
```

```ucm:hide
.> add
```

```ucm
.> edit UUID.randomUUIDBytes 
.> load roundtrip.u
```

```ucm:hide
.> undo
```

# Weirdness reported by Stew with super long lines

```unison:hide roundtrip.u
blah x = 
  u = 92393
  x

thunk x = do x

test1 = do 
  blah !(thunk "This has to laksdjf alsdkfj alskdjf asdf be a long enough string to force a line break")

test2 =
  ("adsf",
    '(Text.toUtf8
       "adsfsfdgsfdgsdfgsdfgsfdgsfdgsdgsgsgfsfgsgsfdgsgfsfdgsgfsfdgsdgsdfgsgf"))

test3 = do
  run : forall a . Nat -> a
  run x = bug x
  runrun = 42
  a = "asldkfj"
  b = "asdflkjasdf"
  do do run runrun do do runrun
```

```ucm:hide
.> add
```

```ucm
.> edit test1 test2 test3
.> load roundtrip.u
```

```ucm:hide
.> undo
```

# Raw string round trip

Regression test for https://github.com/unisonweb/unison/issues/3973

```unison:hide roundtrip.u
a = "a\nb"
b = """
  a
  b
  c -- note blank line

  """
c = """
ignored (wonky case)
Use an extra blank line if you'd like a trailing newline. Like so:

"""
d = """
  ignored (works great)
  Use an extra blank line if you'd like a trailing newline. Like so:

  """
```

```ucm:hide
.> add
```

```ucm
.> edit a b c d
.> load roundtrip.u
```

```ucm:hide
.> undo
```