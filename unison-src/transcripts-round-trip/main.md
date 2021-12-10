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
f : [a] -> a
f xs = match xs with
  x +: (x' +: rest) -> x

g : [a] -> a
g xs = match xs with
  (rest :+ x') :+ x -> x

h : [[a]] -> a
h xs = match xs with
  (rest :+ (rest' :+ x)) -> x
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
```

``` ucm
.> add
.> edit broken
.> undo
```

``` ucm
.> load scratch.u
```

