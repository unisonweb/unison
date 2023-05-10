This transcript verifies that the pretty-printer produces code that can be successfully parsed, for a variety of examples. Terms or types that fail to round-trip can be added here as regression tests. Add tests at the bottom of this

## How to use this transcript: checking round-trip for inline definitions

```unison
x = 1 + 1
```

```ucm
.> add

  ⍟ I've added these definitions:
  
    x : Nat

.> edit x

  ☝️
  
  I added these definitions to the top of
  /Users/cpenner/dev/unison/scratch.u
  
    x : Nat
    x =
      use Nat +
      1 + 1
  
  You can edit them there, then do `update` to replace the
  definitions currently in this namespace.

.> reflog

  Here is a log of the root namespace hashes, starting with the
  most recent, along with the command that got us there. Try:
  
    `fork 2 .old`             
    `fork #u0kujjj8n2 .old`   to make an old namespace
                              accessible again,
                              
    `reset-root #u0kujjj8n2`  to reset the root namespace and
                              its history to that of the
                              specified namespace.
  
       When         Root Hash     Action
  1.   now          #7po6t2j4ji   add
  2.   1 secs ago   #u0kujjj8n2   builtins.mergeio
  3.                #sg60bvjo91   history starts here
  
  Tip: Use `diff.namespace 1 7` to compare namespaces between
       two points in history.

.> reset-root 2

  Done.

```
Resetting the namespace after each example ensures they don't interact at all, which is probably what you want.

The `load` command which does parsing and typechecking of the `edit`'d definitions needs to be in a separate stanza from the `edit` command.

```ucm
.> load scratch.u

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      x : Nat

```
## How to use this transcript: checking round-trip for definitions from a file

Examples can also be loaded from `.u` files:

```ucm
.> load unison-src/transcripts-round-trip/ex2.u

  I found and typechecked these definitions in
  unison-src/transcripts-round-trip/ex2.u. If you do an `add` or
  `update`, here's how your codebase would change:
  
    ⍟ These new definitions are ok to `add`:
    
      b : Nat

.> add

  ⍟ I've added these definitions:
  
    b : Nat

```
When loading definitions from a file, an empty stanza like this will ensure that this empty file is where the definitions being `edit`'d will get dumped.

```unison
-- empty scratch file, `edit` will target this
```

Without the above stanza, the `edit` will send the definition to the most recently loaded file, which would be `ex2.u`, making the transcript not idempotent.

```ucm
.> edit b

  ☝️
  
  I added these definitions to the top of
  /Users/cpenner/dev/unison/scratch.u
  
    b : Nat
    b = 92384
  
  You can edit them there, then do `update` to replace the
  definitions currently in this namespace.

.> reflog

  Here is a log of the root namespace hashes, starting with the
  most recent, along with the command that got us there. Try:
  
    `fork 2 .old`             
    `fork #u0kujjj8n2 .old`   to make an old namespace
                              accessible again,
                              
    `reset-root #u0kujjj8n2`  to reset the root namespace and
                              its history to that of the
                              specified namespace.
  
       When         Root Hash     Action
  1.   now          #jm7d1ujf0n   add
  2.   now          #u0kujjj8n2   reset-root #u0kujjj8n2
  3.   now          #7po6t2j4ji   add
  4.   1 secs ago   #u0kujjj8n2   builtins.mergeio
  5.                #sg60bvjo91   history starts here
  
  Tip: Use `diff.namespace 1 7` to compare namespaces between
       two points in history.

.> reset-root 2

  Done.

```
```ucm
.> load scratch.u

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      b : Nat

```
No reason you can't load a bunch of definitions from a single `.u` file in one go, the only thing that's annoying is you'll have to `find` and then `edit 1-11` in the transcript to load all the definitions into the file.

## Destructuring binds

Regression test for https://github.com/unisonweb/unison/issues/2337

```unison
unique type Blah = Blah Boolean Boolean

f : Blah -> Boolean
f x = let
  (Blah.Blah a b) = x
  a
```

```ucm
.> add

  ⍟ I've added these definitions:
  
    unique type Blah
    f : Blah -> Boolean

.> edit Blah f

  ☝️
  
  I added these definitions to the top of
  /Users/cpenner/dev/unison/scratch.u
  
    unique type Blah
      = Blah Boolean Boolean
    
    f : Blah -> Boolean
    f = cases Blah a b -> a
  
  You can edit them there, then do `update` to replace the
  definitions currently in this namespace.

.> reflog

  Here is a log of the root namespace hashes, starting with the
  most recent, along with the command that got us there. Try:
  
    `fork 2 .old`             
    `fork #u0kujjj8n2 .old`   to make an old namespace
                              accessible again,
                              
    `reset-root #u0kujjj8n2`  to reset the root namespace and
                              its history to that of the
                              specified namespace.
  
       When         Root Hash     Action
  1.   now          #gi14tq4ehs   add
  2.   now          #u0kujjj8n2   reset-root #u0kujjj8n2
  3.   now          #jm7d1ujf0n   add
  4.   now          #u0kujjj8n2   reset-root #u0kujjj8n2
  5.   now          #7po6t2j4ji   add
  6.   1 secs ago   #u0kujjj8n2   builtins.mergeio
  7.                #sg60bvjo91   history starts here
  
  Tip: Use `diff.namespace 1 7` to compare namespaces between
       two points in history.

.> reset-root 2

  Done.

```
```ucm
.> load scratch.u

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      unique type Blah
      f : Blah -> Boolean

```
## Parens around infix patterns

Regression test for https://github.com/unisonweb/unison/issues/2224

```unison
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

  ⍟ I've added these definitions:
  
    f : [()] -> ()
    g : [()] -> ()
    h : [[()]] -> ()

.> edit f g

  ☝️
  
  I added these definitions to the top of
  /Users/cpenner/dev/unison/scratch.u
  
    f : [()] -> ()
    f = cases
      x +: (x' +: rest) -> x
      _                 -> ()
    
    g : [()] -> ()
    g = cases
      rest :+ x' :+ x -> ()
      _               -> ()
  
  You can edit them there, then do `update` to replace the
  definitions currently in this namespace.

.> reflog

  Here is a log of the root namespace hashes, starting with the
  most recent, along with the command that got us there. Try:
  
    `fork 2 .old`             
    `fork #u0kujjj8n2 .old`   to make an old namespace
                              accessible again,
                              
    `reset-root #u0kujjj8n2`  to reset the root namespace and
                              its history to that of the
                              specified namespace.
  
       When         Root Hash     Action
  1.   now          #g01qct62s7   add
  2.   now          #u0kujjj8n2   reset-root #u0kujjj8n2
  3.   now          #gi14tq4ehs   add
  4.   now          #u0kujjj8n2   reset-root #u0kujjj8n2
  5.   now          #jm7d1ujf0n   add
  6.   now          #u0kujjj8n2   reset-root #u0kujjj8n2
  7.   now          #7po6t2j4ji   add
  8.   1 secs ago   #u0kujjj8n2   builtins.mergeio
  9.                #sg60bvjo91   history starts here
  
  Tip: Use `diff.namespace 1 7` to compare namespaces between
       two points in history.

.> reset-root 2

  Done.

```
```ucm
.> load scratch.u

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      f : [()] -> ()
      g : [()] -> ()

```
## Type application inserts necessary parens

Regression test for https://github.com/unisonweb/unison/issues/2392

```unison
unique ability Zonk where zonk : Nat
unique type Foo x y =

foo : Nat -> Foo ('{Zonk} a) ('{Zonk} b) -> Nat
foo n _ = n
```

```ucm
.> add

  ⍟ I've added these definitions:
  
    unique type Foo x y
    unique ability Zonk
    foo : Nat -> Foo ('{Zonk} a) ('{Zonk} b) -> Nat

.> edit foo Zonk Foo

  ☝️
  
  I added these definitions to the top of
  /Users/cpenner/dev/unison/scratch.u
  
    unique type Foo x y
      = 
    
    unique ability Zonk where zonk : {Zonk} Nat
    
    foo : Nat -> Foo ('{Zonk} a) ('{Zonk} b) -> Nat
    foo n _ = n
  
  You can edit them there, then do `update` to replace the
  definitions currently in this namespace.

.> reflog

  Here is a log of the root namespace hashes, starting with the
  most recent, along with the command that got us there. Try:
  
    `fork 2 .old`             
    `fork #u0kujjj8n2 .old`   to make an old namespace
                              accessible again,
                              
    `reset-root #u0kujjj8n2`  to reset the root namespace and
                              its history to that of the
                              specified namespace.
  
        When         Root Hash     Action
  1.    now          #drsgmfubvu   add
  2.    now          #u0kujjj8n2   reset-root #u0kujjj8n2
  3.    now          #g01qct62s7   add
  4.    now          #u0kujjj8n2   reset-root #u0kujjj8n2
  5.    now          #gi14tq4ehs   add
  6.    now          #u0kujjj8n2   reset-root #u0kujjj8n2
  7.    now          #jm7d1ujf0n   add
  8.    now          #u0kujjj8n2   reset-root #u0kujjj8n2
  9.    now          #7po6t2j4ji   add
  10.   1 secs ago   #u0kujjj8n2   builtins.mergeio
  11.                #sg60bvjo91   history starts here
  
  Tip: Use `diff.namespace 1 7` to compare namespaces between
       two points in history.

.> reset-root 2

  Done.

```
```ucm
.> load scratch.u

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      unique type Foo x y
      unique ability Zonk
      foo : Nat -> Foo ('{Zonk} a) ('{Zonk} b) -> Nat

```
## Long lines with repeated operators

Regression test for https://github.com/unisonweb/unison/issues/1035

```unison
foo : Text
foo =
  "aaaaaaaaaaaaaaaaaaaaaa" ++ "bbbbbbbbbbbbbbbbbbbbbb" ++ "cccccccccccccccccccccc" ++ "dddddddddddddddddddddd"
```

```ucm
.> add

  ⍟ I've added these definitions:
  
    foo : Text

.> edit foo

  ☝️
  
  I added these definitions to the top of
  /Users/cpenner/dev/unison/scratch.u
  
    foo : Text
    foo =
      use Text ++
      "aaaaaaaaaaaaaaaaaaaaaa"
        ++ "bbbbbbbbbbbbbbbbbbbbbb"
        ++ "cccccccccccccccccccccc"
        ++ "dddddddddddddddddddddd"
  
  You can edit them there, then do `update` to replace the
  definitions currently in this namespace.

.> reflog

  Here is a log of the root namespace hashes, starting with the
  most recent, along with the command that got us there. Try:
  
    `fork 2 .old`             
    `fork #u0kujjj8n2 .old`   to make an old namespace
                              accessible again,
                              
    `reset-root #u0kujjj8n2`  to reset the root namespace and
                              its history to that of the
                              specified namespace.
  
        When         Root Hash     Action
  1.    now          #66k3d281ae   add
  2.    now          #u0kujjj8n2   reset-root #u0kujjj8n2
  3.    now          #drsgmfubvu   add
  4.    now          #u0kujjj8n2   reset-root #u0kujjj8n2
  5.    now          #g01qct62s7   add
  6.    now          #u0kujjj8n2   reset-root #u0kujjj8n2
  7.    now          #gi14tq4ehs   add
  8.    now          #u0kujjj8n2   reset-root #u0kujjj8n2
  9.    now          #jm7d1ujf0n   add
  10.   now          #u0kujjj8n2   reset-root #u0kujjj8n2
  11.   now          #7po6t2j4ji   add
  12.   1 secs ago   #u0kujjj8n2   builtins.mergeio
  13.                #sg60bvjo91   history starts here
  
  Tip: Use `diff.namespace 1 7` to compare namespaces between
       two points in history.

.> reset-root 2

  Done.

```
```ucm
.> load scratch.u

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo : Text

```
## Emphasis in docs inserts the right number of underscores

Regression test for https://github.com/unisonweb/unison/issues/2408

```unison
myDoc = {{ **my text** __my text__ **MY_TEXT** ___MY__TEXT___ ~~MY~TEXT~~ **MY*TEXT** }}
```

```ucm
.> add

  ⍟ I've added these definitions:
  
    myDoc : Doc2

.> edit myDoc

  ☝️
  
  I added these definitions to the top of
  /Users/cpenner/dev/unison/scratch.u
  
    myDoc : Doc2
    myDoc =
      {{
      **my text** __my text__ **MY_TEXT** ___MY__TEXT___
      ~~MY~TEXT~~ **MY*TEXT**
      }}
  
  You can edit them there, then do `update` to replace the
  definitions currently in this namespace.

.> undo

  Here are the changes I undid
  
  Added definitions:
  
    1. myDoc : Doc2

```
```ucm
.> load scratch.u

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      myDoc : Doc2

```
## Parenthesized let-block with operator

Regression test for https://github.com/unisonweb/unison/issues/1778

```unison
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

  ⍟ I've added these definitions:
  
    structural ability base.Abort
    Abort.toDefault!  : a -> '{g, Abort} a ->{g} a
    Abort.toOptional  : '{g, Abort} a -> '{g} Optional a
    Abort.toOptional! : '{g, Abort} a ->{g} Optional a
    handler           : a -> Request {Abort} a -> a
    x                 : 'Optional Nat
    |>                : a -> (a ->{e} b) ->{e} b

.> edit x base.Abort |> handler Abort.toOptional Abort.toOptional! Abort.toDefault!

  ☝️
  
  I added these definitions to the top of
  /Users/cpenner/dev/unison/scratch.u
  
    structural ability base.Abort where abort : {Abort} a
    
    Abort.toDefault! : a -> '{g, Abort} a ->{g} a
    Abort.toDefault! default thunk =
      h x = toDefault! (handler default x) thunk
      handle !thunk with h
    
    Abort.toOptional : '{g, Abort} a -> '{g} Optional a
    Abort.toOptional thunk = '(toOptional! thunk)
    
    Abort.toOptional! : '{g, Abort} a ->{g} Optional a
    Abort.toOptional! thunk = toDefault! None '(Some !thunk)
    
    handler : a -> Request {Abort} a -> a
    handler default = cases
      { a }        -> a
      {abort -> _} -> default
    
    x : 'Optional Nat
    x =
      (do
        abort
        0) |> toOptional
    
    (|>) : a -> (a ->{e} b) ->{e} b
    a |> f = f a
  
  You can edit them there, then do `update` to replace the
  definitions currently in this namespace.

.> undo

  Here are the changes I undid
  
  Added definitions:
  
    1. structural ability base.Abort
    2. base.Abort.abort  : {#b589mbg492} a
    3. handler           : a -> Request {#b589mbg492} a -> a
    4. Abort.toDefault!  : a -> '{g, #b589mbg492} a ->{g} a
    5. Abort.toOptional  : '{g, #b589mbg492} a
                         -> '{g} Optional a
    6. Abort.toOptional! : '{g, #b589mbg492} a ->{g} Optional a
    7. x                 : 'Optional Nat
    8. |>                : a -> (a ->{e} b) ->{e} b

```
```ucm
.> load scratch.u

  
    ❓
    
    I couldn't resolve any of these symbols:
    
        1 | structural ability base.Abort where abort : {Abort} a
    
    
    Symbol   Suggestions
             
    Abort    No matches
  

```



🛑

The transcript failed due to an error in the stanza above. The error is:


  
    ❓
    
    I couldn't resolve any of these symbols:
    
        1 | structural ability base.Abort where abort : {Abort} a
    
    
    Symbol   Suggestions
             
    Abort    No matches
  

