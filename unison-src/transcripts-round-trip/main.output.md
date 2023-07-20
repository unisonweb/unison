This transcript verifies that the pretty-printer produces code that can be successfully parsed, for a variety of examples. Terms or types that fail to round-trip can be added here as regression tests. Add tests at the bottom of this

## How to use this transcript: checking round-trip for inline definitions

```unison
---
title: roundtrip.u
---
x = 1 + 1

```


```ucm
.> add

  ⍟ I've added these definitions:
  
    x : Nat

.> edit x

  ☝️
  
  I added these definitions to the top of
  /Users/pchiusano/unison/roundtrip.u
  
    x : Nat
    x =
      use Nat +
      1 + 1
  
  You can edit them there, then do `update` to replace the
  definitions currently in this namespace.

.> undo

  Here are the changes I undid
  
  Added definitions:
  
    1. x : Nat

```
Resetting the namespace after each example ensures they don't interact at all, which is probably what you want.

The `load` command which does parsing and typechecking of the `edit`'d definitions needs to be in a separate stanza from the `edit` command.

```ucm
.> load roundtrip.u

  I found and typechecked these definitions in roundtrip.u. If
  you do an `add` or `update`, here's how your codebase would
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
---
title: roundtrip.u
---
-- empty scratch file, `edit` will target this

```


Without the above stanza, the `edit` will send the definition to the most recently loaded file, which would be `ex2.u`, making the transcript not idempotent.

```ucm
.> edit b

  ☝️
  
  I added these definitions to the top of
  /Users/pchiusano/unison/roundtrip.u
  
    b : Nat
    b = 92384
  
  You can edit them there, then do `update` to replace the
  definitions currently in this namespace.

.> undo

  Here are the changes I undid
  
  Added definitions:
  
    1. b : Nat

```
```ucm
.> load roundtrip.u

  I found and typechecked these definitions in roundtrip.u. If
  you do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      b : Nat

```
No reason you can't load a bunch of definitions from a single `.u` file in one go, the only thing that's annoying is you'll have to `find` and then `edit 1-11` in the transcript to load all the definitions into the file.

## Destructuring binds

Regression test for https://github.com/unisonweb/unison/issues/2337

```unison
---
title: roundtrip.u
---
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
  /Users/pchiusano/unison/roundtrip.u
  
    unique type Blah
      = Blah Boolean Boolean
    
    f : Blah -> Boolean
    f = cases Blah a b -> a
  
  You can edit them there, then do `update` to replace the
  definitions currently in this namespace.

.> undo

  Here are the changes I undid
  
  Added definitions:
  
    1. unique type Blah
    2. Blah.Blah : Boolean -> Boolean -> #c9ct8a6u1t
    3. f         : #c9ct8a6u1t -> Boolean

```
```ucm
.> load roundtrip.u

  I found and typechecked these definitions in roundtrip.u. If
  you do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      unique type Blah
      f : Blah -> Boolean

```
## Parens around infix patterns

Regression test for https://github.com/unisonweb/unison/issues/2224

```unison
---
title: roundtrip.u
---
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
  /Users/pchiusano/unison/roundtrip.u
  
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

.> undo

  Here are the changes I undid
  
  Added definitions:
  
    1. f : [()] -> ()
    2. g : [()] -> ()
    3. h : [[()]] -> ()

```
```ucm
.> load roundtrip.u

  I found and typechecked these definitions in roundtrip.u. If
  you do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      f : [()] -> ()
      g : [()] -> ()

```
## Type application inserts necessary parens

Regression test for https://github.com/unisonweb/unison/issues/2392

```unison
---
title: roundtrip.u
---
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
  /Users/pchiusano/unison/roundtrip.u
  
    unique type Foo x y
      = 
    
    unique ability Zonk where zonk : {Zonk} Nat
    
    foo : Nat -> Foo ('{Zonk} a) ('{Zonk} b) -> Nat
    foo n _ = n
  
  You can edit them there, then do `update` to replace the
  definitions currently in this namespace.

.> undo

  Here are the changes I undid
  
  Added definitions:
  
    1. unique type Foo x y
    2. unique ability Zonk
    3. Zonk.zonk : {#54l2535tfc} Nat
    4. foo       : Nat
                 -> #udgqg0p7ql
                   ('{#54l2535tfc} a) ('{#54l2535tfc} b)
                 -> Nat

```
```ucm
.> load roundtrip.u

  I found and typechecked these definitions in roundtrip.u. If
  you do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      unique type Foo x y
      unique ability Zonk
      foo : Nat -> Foo ('{Zonk} a) ('{Zonk} b) -> Nat

```
## Long lines with repeated operators

Regression test for https://github.com/unisonweb/unison/issues/1035

```unison
---
title: roundtrip.u
---
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
  /Users/pchiusano/unison/roundtrip.u
  
    foo : Text
    foo =
      use Text ++
      "aaaaaaaaaaaaaaaaaaaaaa"
        ++ "bbbbbbbbbbbbbbbbbbbbbb"
        ++ "cccccccccccccccccccccc"
        ++ "dddddddddddddddddddddd"
  
  You can edit them there, then do `update` to replace the
  definitions currently in this namespace.

.> undo

  Here are the changes I undid
  
  Added definitions:
  
    1. foo : Text

```
```ucm
.> load roundtrip.u

  I found and typechecked these definitions in roundtrip.u. If
  you do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo : Text

```
## Emphasis in docs inserts the right number of underscores

Regression test for https://github.com/unisonweb/unison/issues/2408

```unison
---
title: roundtrip.u
---
myDoc = {{ **my text** __my text__ **MY_TEXT** ___MY__TEXT___ ~~MY~TEXT~~ **MY*TEXT** }}

```


```ucm
.> add

  ⍟ I've added these definitions:
  
    myDoc : Doc2

.> edit myDoc

  ☝️
  
  I added these definitions to the top of
  /Users/pchiusano/unison/roundtrip.u
  
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
.> load roundtrip.u

  I found and typechecked these definitions in roundtrip.u. If
  you do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      myDoc : Doc2

```
## Parenthesized let-block with operator

Regression test for https://github.com/unisonweb/unison/issues/1778

```unison
---
title: roundtrip.u
---
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
  /Users/pchiusano/unison/roundtrip.u
  
    structural ability base.Abort where abort : {base.Abort} a
    
    Abort.toDefault! : a -> '{g, Abort} a ->{g} a
    Abort.toDefault! default thunk =
      h x = Abort.toDefault! (handler default x) thunk
      handle !thunk with h
    
    Abort.toOptional : '{g, Abort} a -> '{g} Optional a
    Abort.toOptional thunk = do toOptional! thunk
    
    Abort.toOptional! : '{g, Abort} a ->{g} Optional a
    Abort.toOptional! thunk = toDefault! None '(Some !thunk)
    
    handler : a -> Request {Abort} a -> a
    handler default = cases
      { a }          -> a
      { abort -> _ } -> default
    
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
.> load roundtrip.u

  I found and typechecked these definitions in roundtrip.u. If
  you do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      structural ability base.Abort
      Abort.toDefault!  : a -> '{g, Abort} a ->{g} a
      Abort.toOptional  : '{g, Abort} a -> '{g} Optional a
      Abort.toOptional! : '{g, Abort} a ->{g} Optional a
      handler           : a -> Request {Abort} a -> a
      x                 : 'Optional Nat
      |>                : a -> (a ->{e} b) ->{e} b

```
## Line breaks before 'let

Regression test for https://github.com/unisonweb/unison/issues/1536

```unison
---
title: roundtrip.u
---
r = 'let
 y = 0
 y

```


```ucm
.> add

  ⍟ I've added these definitions:
  
    r : 'Nat

.> edit r

  ☝️
  
  I added these definitions to the top of
  /Users/pchiusano/unison/roundtrip.u
  
    r : 'Nat
    r = do
      y = 0
      y
  
  You can edit them there, then do `update` to replace the
  definitions currently in this namespace.

.> undo

  Here are the changes I undid
  
  Added definitions:
  
    1. r : 'Nat

```
```ucm
.> load roundtrip.u

  I found and typechecked these definitions in roundtrip.u. If
  you do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      r : 'Nat

```
## Raw codeblocks add indentation

Regression test for https://github.com/unisonweb/unison/issues/2271

```ucm
.> load unison-src/transcripts-round-trip/docTest2.u

  I found and typechecked these definitions in
  unison-src/transcripts-round-trip/docTest2.u. If you do an
  `add` or `update`, here's how your codebase would change:
  
    ⍟ These new definitions are ok to `add`:
    
      docTest2 : Doc2

.> add

  ⍟ I've added these definitions:
  
    docTest2 : Doc2

```
```unison
---
title: roundtrip.u
---
x = 2

```


```ucm
.> edit docTest2

  ☝️
  
  I added these definitions to the top of
  /Users/pchiusano/unison/roundtrip.u
  
    docTest2 : Doc2
    docTest2 =
      {{ # Full doc body indented
      
        ``` raw
        myVal1 = 42 
        myVal2 = 43
        myVal4 = 44
        ```
        
        ``` raw
        indented1= "hi"
        indented2="this is two indents"
        ```
        
        I am two spaces over }}
  
  You can edit them there, then do `update` to replace the
  definitions currently in this namespace.

```
```ucm
.> load roundtrip.u

  I found and typechecked the definitions in roundtrip.u. This
  file has been previously added to the codebase.

.> add

  ⊡ Ignored previously added definitions: docTest2

```
## Unison Cloud roundtrip issues

Regression tests for  https://github.com/unisonweb/unison/issues/2650

```unison
---
title: roundtrip.u
---
broken =
    addNumbers: 'Nat
    addNumbers = 'let
      use Nat +
      y = 12
      13 + y
    !addNumbers

```


```ucm
.> add

  ⍟ I've added these definitions:
  
    broken : Nat

.> edit broken

  ☝️
  
  I added these definitions to the top of
  /Users/pchiusano/unison/roundtrip.u
  
    broken : Nat
    broken =
      addNumbers : 'Nat
      addNumbers = do
        use Nat +
        y = 12
        13 + y
      !addNumbers
  
  You can edit them there, then do `update` to replace the
  definitions currently in this namespace.

.> undo

  Here are the changes I undid
  
  Added definitions:
  
    1. broken : Nat

```
```ucm
.> load roundtrip.u

  I found and typechecked these definitions in roundtrip.u. If
  you do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      broken : Nat

```
```unison
---
title: roundtrip.u
---
tvarmodify tvar fun = ()

broken tvar =
  '(tvarmodify tvar (cases
     Some _ -> "oh boy isn't this a very very very very very very very long string?"
     None -> ""))

```


```ucm
.> add

  ⍟ I've added these definitions:
  
    broken     : tvar -> () -> ()
    tvarmodify : tvar -> fun -> ()

.> edit tvarmodify broken

  ☝️
  
  I added these definitions to the top of
  /Users/pchiusano/unison/roundtrip.u
  
    broken : tvar -> () -> ()
    broken tvar =
      do
        tvarmodify tvar cases
          Some _ ->
            "oh boy isn't this a very very very very very very very long string?"
          None -> ""
    
    tvarmodify : tvar -> fun -> ()
    tvarmodify tvar fun = ()
  
  You can edit them there, then do `update` to replace the
  definitions currently in this namespace.

.> undo

  Here are the changes I undid
  
  Added definitions:
  
    1. broken     : tvar -> () -> ()
    2. tvarmodify : tvar -> fun -> ()

```
```ucm
.> load roundtrip.u

  I found and typechecked these definitions in roundtrip.u. If
  you do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      broken     : tvar -> '()
      tvarmodify : tvar -> fun -> ()

```
```unison
---
title: roundtrip.u
---
broken = cases
  Some loooooooooooooooooooooooooooooooooooooooooooooooooooooooong | loooooooooooooooooooooooooooooooooooooooooooooooooooooooong == 1 -> ()
  _ -> ()

```


```ucm
.> add

  ⍟ I've added these definitions:
  
    broken : Optional Nat -> ()

.> edit broken

  ☝️
  
  I added these definitions to the top of
  /Users/pchiusano/unison/roundtrip.u
  
    broken : Optional Nat -> ()
    broken = cases
      Some
        loooooooooooooooooooooooooooooooooooooooooooooooooooooooong| loooooooooooooooooooooooooooooooooooooooooooooooooooooooong
        == 1  ->
        ()
      _ -> ()
  
  You can edit them there, then do `update` to replace the
  definitions currently in this namespace.

.> undo

  Here are the changes I undid
  
  Added definitions:
  
    1. broken : Optional Nat -> ()

```
```ucm
.> load roundtrip.u

  I found and typechecked these definitions in roundtrip.u. If
  you do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      broken : Optional Nat -> ()

```
## Guard patterns on long lines

```unison
---
title: roundtrip.u
---
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

  ⍟ I've added these definitions:
  
    structural type SomethingUnusuallyLong
    foo : 'Nat

.> edit SomethingUnusuallyLong foo

  ☝️
  
  I added these definitions to the top of
  /Users/pchiusano/unison/roundtrip.u
  
    structural type SomethingUnusuallyLong
      = SomethingUnusuallyLong Text Text Text
    
    foo : 'Nat
    foo =
      go x =
        do
          match (a -> a) x with
            SomethingUnusuallyLong
              lijaefliejalfijelfj aefilaeifhlei liaehjffeafijij
              | lijaefliejalfijelfj == aefilaeifhlei    -> 0
              | lijaefliejalfijelfj == liaehjffeafijij  -> 1
            _ -> 2
      go (SomethingUnusuallyLong "one" "two" "three")
  
  You can edit them there, then do `update` to replace the
  definitions currently in this namespace.

.> undo

  Here are the changes I undid
  
  Added definitions:
  
    1. structural type SomethingUnusuallyLong
    2. SomethingUnusuallyLong.SomethingUnusuallyLong : Text
                                                     -> Text
                                                     -> Text
                                                     -> #p9dp5r8ff6
    3. foo                                           : 'Nat

```
```ucm
.> load roundtrip.u

  I found and typechecked these definitions in roundtrip.u. If
  you do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      structural type SomethingUnusuallyLong
      foo : 'Nat

```
## Nested fences

```ucm
.> load unison-src/transcripts-round-trip/nested.u

  I found and typechecked these definitions in
  unison-src/transcripts-round-trip/nested.u. If you do an `add`
  or `update`, here's how your codebase would change:
  
    ⍟ These new definitions are ok to `add`:
    
      nested : Doc2

.> add

  ⍟ I've added these definitions:
  
    nested : Doc2

```
```ucm
.> edit nested

  ☝️
  
  I added these definitions to the top of
  /Users/pchiusano/unison/unison-src/transcripts-round-trip/nested.u
  
    nested : Doc2
    nested =
      {{ ```` raw
      ```unison
      r = "boopydoo"
      ```
      ```` }}
  
  You can edit them there, then do `update` to replace the
  definitions currently in this namespace.

.> undo

  Here are the changes I undid
  
  Added definitions:
  
    1. nested : Doc2

```
```ucm
.> load unison-src/transcripts-round-trip/nested.u

  I found and typechecked these definitions in
  unison-src/transcripts-round-trip/nested.u. If you do an `add`
  or `update`, here's how your codebase would change:
  
    ⍟ These new definitions are ok to `add`:
    
      nested : Doc2

```
## Multiline expressions in multiliine lists

```unison
---
title: roundtrip.u
---
foo a b c d e f g h i j = 42

use Nat +
x = [ 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
    , foo 12939233 2102020 329292 429292 522020 62929292 72020202 820202 920202 1020202 ]

```


```ucm
.> add

  ⍟ I've added these definitions:
  
    foo : a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> Nat
    x   : [Nat]

.> edit foo x

  ☝️
  
  I added these definitions to the top of
  /Users/pchiusano/unison/roundtrip.u
  
    foo : a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> Nat
    foo a b c d e f g h i j = 42
    
    x : [Nat]
    x =
      use Nat +
      [ 1
          + 1
          + 1
          + 1
          + 1
          + 1
          + 1
          + 1
          + 1
          + 1
          + 1
          + 1
          + 1
          + 1
          + 1
          + 1
          + 1
          + 1
          + 1
      , foo
          12939233
          2102020
          329292
          429292
          522020
          62929292
          72020202
          820202
          920202
          1020202
      ]
  
  You can edit them there, then do `update` to replace the
  definitions currently in this namespace.

.> undo

  Here are the changes I undid
  
  Added definitions:
  
    1. foo : a
           -> b
           -> c
           -> d
           -> e
           -> f
           -> g
           -> h
           -> i
           -> j
           -> Nat
    2. x   : [Nat]

```
```ucm
.> load roundtrip.u

  I found and typechecked these definitions in roundtrip.u. If
  you do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo : a
            -> b
            -> c
            -> d
            -> e
            -> f
            -> g
            -> h
            -> i
            -> j
            -> Nat
      x   : [Nat]

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

```unison
---
title: roundtrip.u
---
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

  ⍟ I've added these definitions:
  
    +    : Nat -> Nat -> Nat
    bar0 : x -> () -> Nat
    bar1 : x -> () -> Nat
    bar2 : x -> () -> Nat
    bar3 : x -> () -> b -> Nat
    foo  : a -> b -> Nat

.> edit foo bar0 bar1 bar2 bar3

  ☝️
  
  I added these definitions to the top of
  /Users/pchiusano/unison/roundtrip.u
  
    bar0 : x -> () -> Nat
    bar0 x = do
      a = 1
      b = 2
      foo a do
        c = 3
        a + b
    
    bar1 : x -> () -> Nat
    bar1 x =
      do
        a = 1
        b = 2
        foo
          (100
          + 200
          + 300
          + 400
          + 500
          + 600
          + 700
          + 800
          + 900
          + 1000
          + 1100
          + 1200
          + 1300
          + 1400
          + 1500)
          do
          c = 3
          a + b
    
    bar2 : x -> () -> Nat
    bar2 x = do
      a = 1
      b = 2
      1 + (foo a do
        c = 3
        a + b)
    
    bar3 : x -> () -> b -> Nat
    bar3 x = do
      a = 1
      b = 2
      c = foo do
        c = 3
        a + b
      c
    
    foo : a -> b -> Nat
    foo a b = 42
  
  You can edit them there, then do `update` to replace the
  definitions currently in this namespace.

.> undo

  Here are the changes I undid
  
  Added definitions:
  
    1. +    : Nat -> Nat -> Nat
    2. bar0 : x -> () -> Nat
    3. bar1 : x -> () -> Nat
    4. bar2 : x -> () -> Nat
    5. bar3 : x -> () -> b -> Nat
    6. foo  : a -> b -> Nat

```
```ucm
.> load roundtrip.u

  I found and typechecked these definitions in roundtrip.u. If
  you do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      bar0 : x -> 'Nat
      bar1 : x -> 'Nat
      bar2 : x -> 'Nat
      bar3 : x -> '(b -> Nat)
      foo  : a -> b -> Nat

```
# Lambda as the last argument where the bound var is not free in the body

If a lambda's argument is not free in the body, the term printer counts this as
a "delay" instead of a lambda. This test makes sure that detecting this
condition lines up with the printing, so we don't detect a delay but then
go ahead and print it as a normal lambda.

```unison
---
title: roundtrip.u
---
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

  ⍟ I've added these definitions:
  
    +                : Nat -> Nat -> Nat
    afun             : x -> (x ->{g} t) ->{g} t
    roundtripLastLam : Nat

.> edit roundtripLastLam afun

  ☝️
  
  I added these definitions to the top of
  /Users/pchiusano/unison/roundtrip.u
  
    afun : x -> (x ->{g} t) ->{g} t
    afun x f = f x
    
    roundtripLastLam : Nat
    roundtripLastLam = afun "foo" do
      _ = 1 + 1
      3
  
  You can edit them there, then do `update` to replace the
  definitions currently in this namespace.

.> undo

  Here are the changes I undid
  
  Added definitions:
  
    1. +                : Nat -> Nat -> Nat
    2. afun             : x -> (x ->{g} t) ->{g} t
    3. roundtripLastLam : Nat

```
```ucm
.> load roundtrip.u

  I found and typechecked these definitions in roundtrip.u. If
  you do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      afun             : x -> (x ->{g} t) ->{g} t
      roundtripLastLam : Nat

```
# Comment out builtins in the edit command

Regression test for https://github.com/unisonweb/unison/pull/3548

```ucm
.> alias.term ##Nat.+ plus

  Done.

.> edit plus

  ☝️
  
  I added these definitions to the top of
  /Users/pchiusano/unison/roundtrip.u
  
    -- builtin plus : builtin.Nat -> builtin.Nat -> builtin.Nat
  
  You can edit them there, then do `update` to replace the
  definitions currently in this namespace.

.> undo

  Here are the changes I undid
  
  Name changes:
  
    Original            Changes
    1. builtin.Nat.+    2. plus (added)

```
```ucm
.> load roundtrip.u

  I loaded roundtrip.u and didn't find anything.

```
# Indent long pattern lists to avoid virtual semicolon

Regression test for https://github.com/unisonweb/unison/issues/3627

```unison
---
title: roundtrip.u
---
(+) a b = ##Nat.+ a b

foo = cases
  aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,
   bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
    -> aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa + bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb

```


```ucm
.> add

  ⍟ I've added these definitions:
  
    +   : Nat -> Nat -> Nat
    foo : Nat -> Nat -> Nat

.> edit foo

  ☝️
  
  I added these definitions to the top of
  /Users/pchiusano/unison/roundtrip.u
  
    foo : Nat -> Nat -> Nat
    foo = cases
      aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,
        bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb ->
        aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
          + bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
  
  You can edit them there, then do `update` to replace the
  definitions currently in this namespace.

.> undo

  Here are the changes I undid
  
  Added definitions:
  
    1. +   : Nat -> Nat -> Nat
    2. foo : Nat -> Nat -> Nat

```
```ucm
.> load roundtrip.u

  I found and typechecked these definitions in roundtrip.u. If
  you do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo : Nat -> Nat -> Nat

```
# Multi-line lambda let

Regression test for #3110 and #3801

```unison
---
title: roundtrip.u
---
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

  ⍟ I've added these definitions:
  
    foreach : [a] -> (a ->{e} t) ->{e} ()
    ignore  : x -> ()
    test1   : ()
    test2   : ()
    test3   : ()

.> edit test1 test2 test3 foreach ignore

  ☝️
  
  I added these definitions to the top of
  /Users/pchiusano/unison/roundtrip.u
  
    foreach : [a] -> (a ->{e} t) ->{e} ()
    foreach x f =
      _ = List.map f x
      ()
    
    ignore : x -> ()
    ignore x = ()
    
    test1 : ()
    test1 =
      foreach
        [1, 2, 3] (x -> let
          y = Nat.increment x
          ())
    
    test2 : ()
    test2 = foreach [1, 2, 3] (x -> ignore (Nat.increment x))
    
    test3 : ()
    test3 = foreach [1, 2, 3] '(x -> do
        y = Nat.increment x
        ())
  
  You can edit them there, then do `update` to replace the
  definitions currently in this namespace.

.> undo

  Here are the changes I undid
  
  Added definitions:
  
    1. foreach : [a] -> (a ->{e} t) ->{e} ()
    2. ignore  : x -> ()
    3. test1   : ()
    4. test2   : ()
    5. test3   : ()

```
```ucm
.> load roundtrip.u

  I found and typechecked these definitions in roundtrip.u. If
  you do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foreach : [a] -> (a ->{e} t) ->{e} ()
      ignore  : x -> ()
      test1   : ()
      test2   : ()
      test3   : ()

```
# Destructuring bind in delay or lambda

Regression test for https://github.com/unisonweb/unison/issues/3710

```unison
---
title: roundtrip.u
---
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

  ⍟ I've added these definitions:
  
    d1 : '(Nat, Nat, Nat, Nat, Nat, Nat)
    d2 : (Nat, Nat, Nat, Nat, Nat, Nat)
    d3 : x -> (Nat, x, Nat, Nat, Nat, Nat)
    d4 : x -> () -> (Nat, x, Nat, Nat, Nat, Nat)
    d5 : Optional a -> a

.> edit d1 d2 d3 d4 d5

  ☝️
  
  I added these definitions to the top of
  /Users/pchiusano/unison/roundtrip.u
  
    d1 : '(Nat, Nat, Nat, Nat, Nat, Nat)
    d1 = do
      (a, b) = (1, 2)
      (c, d) = (3, 4)
      (e, f) = (5, 6)
      (a, b, c, d, e, f)
    
    d2 : (Nat, Nat, Nat, Nat, Nat, Nat)
    d2 =
      (a, b) = (1, 2)
      (c, d) = (3, 4)
      (e, f) = (5, 6)
      (a, b, c, d, e, f)
    
    d3 : x -> (Nat, x, Nat, Nat, Nat, Nat)
    d3 x =
      (a, b) = (1, x)
      (c, d) = (3, 4)
      (e, f) = (5, 6)
      (a, b, c, d, e, f)
    
    d4 : x -> () -> (Nat, x, Nat, Nat, Nat, Nat)
    d4 x = do
      (a, b) = (1, x)
      (c, d) = (3, 4)
      (e, f) = (5, 6)
      (a, b, c, d, e, f)
    
    d5 : Optional a -> a
    d5 = cases
      Some x -> x
      None   -> bug "oops"
  
  You can edit them there, then do `update` to replace the
  definitions currently in this namespace.

.> undo

  Here are the changes I undid
  
  Added definitions:
  
    1. d1 : '(Nat, Nat, Nat, Nat, Nat, Nat)
    2. d2 : (Nat, Nat, Nat, Nat, Nat, Nat)
    3. d3 : x -> (Nat, x, Nat, Nat, Nat, Nat)
    4. d4 : x -> () -> (Nat, x, Nat, Nat, Nat, Nat)
    5. d5 : Optional a -> a

```
```ucm
.> load roundtrip.u

  I found and typechecked these definitions in roundtrip.u. If
  you do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      d1 : '(Nat, Nat, Nat, Nat, Nat, Nat)
      d2 : (Nat, Nat, Nat, Nat, Nat, Nat)
      d3 : x -> (Nat, x, Nat, Nat, Nat, Nat)
      d4 : x -> '(Nat, x, Nat, Nat, Nat, Nat)
      d5 : Optional a -> a

```
# Avoid capture of local variables when selecting names for references

Regression test for https://github.com/unisonweb/unison/issues/525

```unison
---
title: roundtrip.u
---
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


We'd get a type error here if `exampleTerm` or `exampleType` didn't round-trip, but it typechecks okay! 🎉 

```ucm
.> edit exampleTerm exampleType

  ☝️
  
  I added these definitions to the top of
  /Users/pchiusano/unison/roundtrip.u
  
    exampleTerm : Text -> Nat
    exampleTerm quaffle =
      use Nat +
      Foo.bar.quaffle + 1
    
    exampleType : Id qualifiedName -> Id Fully.qualifiedName
    exampleType z = Id (Dontcare () 19)
  
  You can edit them there, then do `update` to replace the
  definitions currently in this namespace.

.> load roundtrip.u

  I found and typechecked the definitions in roundtrip.u. This
  file has been previously added to the codebase.

```
# Use clauses can't introduce shadowing 

```unison
---
title: roundtrip.u
---
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

  ⍟ I've added these definitions:
  
    Foo.bar.quaffle : Nat
    example         : Int -> Text -> Nat
    example2        : Int -> Nat

.> edit example example2

  ☝️
  
  I added these definitions to the top of
  /Users/pchiusano/unison/roundtrip.u
  
    example : Int -> Text -> Nat
    example oo quaffle =
      use Nat +
      Foo.bar.quaffle + Foo.bar.quaffle + 1
    
    example2 : Int -> Nat
    example2 oo =
      use Nat +
      quaffle = "hi"
      Foo.bar.quaffle + Foo.bar.quaffle + Foo.bar.quaffle + 1
  
  You can edit them there, then do `update` to replace the
  definitions currently in this namespace.

```
This just shows that we don't insert a `use Foo.bar quaffle`, even though it's referenced multiple times, since this would case shadowing.

```ucm
.> load roundtrip.u

  I found and typechecked the definitions in roundtrip.u. This
  file has been previously added to the codebase.

.> undo

  Here are the changes I undid
  
  Added definitions:
  
    1. example         : Int -> Text -> Nat
    2. example2        : Int -> Nat
    3. Foo.bar.quaffle : Nat

```
# Use clauses aren't pushed down too far

We push `use` clauses down to the nearest enclosing let or let rec block so they're close to where they're used:

```unison
---
title: roundtrip.u
---
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


```ucm
.> edit ex1 ex2 ex3 ex3a

  ☝️
  
  I added these definitions to the top of
  /Users/pchiusano/unison/roundtrip.u
  
    ex1 : Nat
    ex1 =
      use Foo.bar qux1 qux3
      use Nat +
      a = qux3 + qux3
      qux1 + qux1 + Foo.bar.qux2
    
    ex2 : Nat
    ex2 =
      use Foo.bar qux1
      use Nat +
      a =
        use Foo.bar qux3
        z = 203993
        qux3 + qux3
      qux1 + qux1 + Foo.bar.qux2
    
    ex3 : ()
    ex3 =
      a = do
        use Foo.bar qux3
        use Nat +
        x = qux3 + qux3
        x + x
      ()
    
    ex3a : ()
    ex3a =
      use Foo.bar qux3
      use Nat +
      a = do qux3 + qux3
      ()
  
  You can edit them there, then do `update` to replace the
  definitions currently in this namespace.

.> load roundtrip.u

  I found and typechecked the definitions in roundtrip.u. This
  file has been previously added to the codebase.

```
# Use soft hangs after `with` and `=` and in last argument of function application

```unison
---
title: roundtrip.u
---
structural ability Abort where
  abort : x

ex1 = handle
  x = 1
  y = abort
  x + y
  with cases
    { a } -> a
    { Abort.abort -> _ } -> 0

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


```ucm
.> edit ex1 ex2 ex3 ex4 ex5 ex6 ex7 ex8

  ☝️
  
  I added these definitions to the top of
  /Users/pchiusano/unison/roundtrip.u
  
    ex1 : Nat
    ex1 =
      use Nat +
      handle
        x = 1
        y = abort
        x + y
      with cases
        { a }          -> a
        { abort -> _ } -> 0
    
    ex2 : Nat
    ex2 = foreach [0, 1, 2, 3, 4, 5] cases
      0 -> 0
      1 -> 1
      n -> n Nat.+ 100
    
    ex3 : 'Nat
    ex3 = do
      use Nat +
      catchAll do
        x = 1
        y = 2
        x + y
    
    ex4 : 'Nat
    ex4 = do match 0 with
      0 -> 0
      1 -> 1
      n -> n
    
    ex5 : Text
    ex5 = match Nat.increment 1 with
      2 -> "yay"
      n -> "oh no"
    
    ex6 : Nat
    ex6 = foreach [1, 2, 3, 4] cases
      0 -> 1
      n -> n Nat.+ 1
    
    ex7 : somewhere -> Nat
    ex7 somewhere = forkAt somewhere do
      use Nat +
      x = 1
      y = 2
      x + y
    
    ex8 : Nat
    ex8 = 
      foreach [0, 1, 2, 3, 4, 5] cases
        0 -> 0
        1 -> 1
        n ->
          forkAt
            0
            (n
              Nat.+ n
              Nat.+ n
              Nat.+ n
              Nat.+ n
              Nat.+ n
              Nat.+ n
              Nat.+ n
              Nat.+ n
              Nat.+ n
              Nat.+ n)
  
  You can edit them there, then do `update` to replace the
  definitions currently in this namespace.

.> load roundtrip.u

  I found and typechecked these definitions in roundtrip.u. If
  you do an `add` or `update`, here's how your codebase would
  change:
  
    ⊡ Previously added definitions will be ignored: ex1 ex2 ex5
      ex6 ex8
    
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      ex3 : 'Nat
      ex4 : 'Nat
      ex7 : somewhere -> Nat

```
# Make sure use clauses don't show up before a soft hang 

Regression test for https://github.com/unisonweb/unison/issues/3883

```unison
---
title: roundtrip.u
---
unique type UUID = UUID Nat Nat

UUID.random : 'UUID
UUID.random = do UUID 0 0 

UUID.randomUUIDBytes : 'Bytes
UUID.randomUUIDBytes = do
  (UUID a b) = !UUID.random
  (encodeNat64be a) ++ (encodeNat64be b)

```


```ucm
.> edit UUID.randomUUIDBytes 

  ☝️
  
  I added these definitions to the top of
  /Users/pchiusano/unison/roundtrip.u
  
    UUID.randomUUIDBytes : 'Bytes
    UUID.randomUUIDBytes = do
      use Bytes ++
      (UUID a b) = !random
      encodeNat64be a ++ encodeNat64be b
  
  You can edit them there, then do `update` to replace the
  definitions currently in this namespace.

.> load roundtrip.u

  I found and typechecked the definitions in roundtrip.u. This
  file has been previously added to the codebase.

```
# Weirdness reported by Stew with super long lines

```unison
---
title: roundtrip.u
---
blah x = 
  u = 92393
  x
thunk x = do x
test = do 
  blah !(thunk "This has to laksdjf alsdkfj alskdjf asdf be a long enough string to force a line break")

test2 =
  ("adsf",
    '(Text.toUtf8
       "adsfsfdgsfdgsdfgsdfgsfdgsfdgsdgsgsgfsfgsgsfdgsgfsfdgsgfsfdgsdgsdfgsgf"))

```


```ucm
.> edit test test2

  ☝️
  
  I added these definitions to the top of
  /Users/pchiusano/unison/roundtrip.u
  
    test : 'Text
    test =
      do
        blah
          !(thunk
             "This has to laksdjf alsdkfj alskdjf asdf be a long enough string to force a line break")
    
    test2 : (Text, '{g} Bytes)
    test2 =
      ( "adsf"
      , '(toUtf8
            "adsfsfdgsfdgsdfgsdfgsfdgsfdgsdgsgsgfsfgsgsfdgsgfsfdgsgfsfdgsdgsdfgsgf")
      )
  
  You can edit them there, then do `update` to replace the
  definitions currently in this namespace.

.> load roundtrip.u

  I found and typechecked these definitions in roundtrip.u. If
  you do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      test  : 'Text
      test2 : (Text, '{g} Bytes)

```
