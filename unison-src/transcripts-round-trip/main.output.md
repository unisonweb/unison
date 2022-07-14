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
  /Users/pchiusano/unison/scratch.u
  
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
    `fork #3uk2laeo44 .old`   to make an old namespace
                              accessible again,
                              
    `reset-root #3uk2laeo44`  to reset the root namespace and
                              its history to that of the
                              specified namespace.
  
  1. #5d37lofc79 : add
  2. #3uk2laeo44 : builtins.mergeio
  3. #sg60bvjo91 : (initial reflogged namespace)

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
  /Users/pchiusano/unison/scratch.u
  
    b : Nat
    b = 92384
  
  You can edit them there, then do `update` to replace the
  definitions currently in this namespace.

.> reflog

  Here is a log of the root namespace hashes, starting with the
  most recent, along with the command that got us there. Try:
  
    `fork 2 .old`             
    `fork #3uk2laeo44 .old`   to make an old namespace
                              accessible again,
                              
    `reset-root #3uk2laeo44`  to reset the root namespace and
                              its history to that of the
                              specified namespace.
  
  1. #m90i9pca73 : add
  2. #3uk2laeo44 : reset-root #3uk2laeo44
  3. #5d37lofc79 : add
  4. #3uk2laeo44 : builtins.mergeio
  5. #sg60bvjo91 : (initial reflogged namespace)

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
  /Users/pchiusano/unison/scratch.u
  
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
    `fork #3uk2laeo44 .old`   to make an old namespace
                              accessible again,
                              
    `reset-root #3uk2laeo44`  to reset the root namespace and
                              its history to that of the
                              specified namespace.
  
  1. #ru0oo7dcnh : add
  2. #3uk2laeo44 : reset-root #3uk2laeo44
  3. #m90i9pca73 : add
  4. #3uk2laeo44 : reset-root #3uk2laeo44
  5. #5d37lofc79 : add
  6. #3uk2laeo44 : builtins.mergeio
  7. #sg60bvjo91 : (initial reflogged namespace)

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

  ⍟ I've added these definitions:
  
    f : [a] -> a
    g : [a] -> a
    h : [[a]] -> a

.> edit f g

  ☝️
  
  I added these definitions to the top of
  /Users/pchiusano/unison/scratch.u
  
    f : [a] -> a
    f = cases x +: (x' +: rest) -> x
    
    g : [a] -> a
    g = cases rest :+ x' :+ x -> x
  
  You can edit them there, then do `update` to replace the
  definitions currently in this namespace.

.> reflog

  Here is a log of the root namespace hashes, starting with the
  most recent, along with the command that got us there. Try:
  
    `fork 2 .old`             
    `fork #3uk2laeo44 .old`   to make an old namespace
                              accessible again,
                              
    `reset-root #3uk2laeo44`  to reset the root namespace and
                              its history to that of the
                              specified namespace.
  
  1. #c5qfjje3vn : add
  2. #3uk2laeo44 : reset-root #3uk2laeo44
  3. #ru0oo7dcnh : add
  4. #3uk2laeo44 : reset-root #3uk2laeo44
  5. #m90i9pca73 : add
  6. #3uk2laeo44 : reset-root #3uk2laeo44
  7. #5d37lofc79 : add
  8. #3uk2laeo44 : builtins.mergeio
  9. #sg60bvjo91 : (initial reflogged namespace)

.> reset-root 2

  Done.

```
```ucm
.> load scratch.u

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      f : [a] -> a
      g : [a] -> a

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
  /Users/pchiusano/unison/scratch.u
  
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
    `fork #3uk2laeo44 .old`   to make an old namespace
                              accessible again,
                              
    `reset-root #3uk2laeo44`  to reset the root namespace and
                              its history to that of the
                              specified namespace.
  
  1.  #90vn9fdk1t : add
  2.  #3uk2laeo44 : reset-root #3uk2laeo44
  3.  #c5qfjje3vn : add
  4.  #3uk2laeo44 : reset-root #3uk2laeo44
  5.  #ru0oo7dcnh : add
  6.  #3uk2laeo44 : reset-root #3uk2laeo44
  7.  #m90i9pca73 : add
  8.  #3uk2laeo44 : reset-root #3uk2laeo44
  9.  #5d37lofc79 : add
  10. #3uk2laeo44 : builtins.mergeio
  11. #sg60bvjo91 : (initial reflogged namespace)

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
  /Users/pchiusano/unison/scratch.u
  
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
    `fork #3uk2laeo44 .old`   to make an old namespace
                              accessible again,
                              
    `reset-root #3uk2laeo44`  to reset the root namespace and
                              its history to that of the
                              specified namespace.
  
  1.  #fqe7rpjits : add
  2.  #3uk2laeo44 : reset-root #3uk2laeo44
  3.  #90vn9fdk1t : add
  4.  #3uk2laeo44 : reset-root #3uk2laeo44
  5.  #c5qfjje3vn : add
  6.  #3uk2laeo44 : reset-root #3uk2laeo44
  7.  #ru0oo7dcnh : add
  8.  #3uk2laeo44 : reset-root #3uk2laeo44
  9.  #m90i9pca73 : add
  10. #3uk2laeo44 : reset-root #3uk2laeo44
  11. #5d37lofc79 : add
  12. #3uk2laeo44 : builtins.mergeio
  13. #sg60bvjo91 : (initial reflogged namespace)

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
  /Users/pchiusano/unison/scratch.u
  
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
  /Users/pchiusano/unison/scratch.u
  
    structural ability base.Abort where abort : {base.Abort} a
    
    Abort.toDefault! : a -> '{g, Abort} a ->{g} a
    Abort.toDefault! default thunk =
      h x = Abort.toDefault! (handler default x) thunk
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

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
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
  /Users/pchiusano/unison/scratch.u
  
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
.> load scratch.u

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
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
x = 2
```

```ucm
.> edit docTest2

  ☝️
  
  I added these definitions to the top of
  /Users/pchiusano/unison/scratch.u
  
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
.> load scratch.u

  I found and typechecked the definitions in scratch.u. This
  file has been previously added to the codebase.

.> add

  ⊡ Ignored previously added definitions: docTest2

```
## Unison Cloud roundtrip issues

Regression tests for  https://github.com/unisonweb/unison/issues/2650

```unison
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
  /Users/pchiusano/unison/scratch.u
  
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
.> load scratch.u

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      broken : Nat

```
```unison
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
  /Users/pchiusano/unison/scratch.u
  
    broken : tvar -> () -> ()
    broken tvar =
      '(tvarmodify
          tvar
          (cases
            Some _ ->
              "oh boy isn't this a very very very very very very very long string?"
            None   -> ""))
    
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
.> load scratch.u

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      broken     : tvar -> '()
      tvarmodify : tvar -> fun -> ()

```
```unison
broken = cases
  Some loooooooooooooooooooooooooooooooooooooooooooooooooooooooong | loooooooooooooooooooooooooooooooooooooooooooooooooooooooong == 1 -> ()
```

```ucm
.> add

  ⍟ I've added these definitions:
  
    broken : Optional Nat -> ()

.> edit broken

  ☝️
  
  I added these definitions to the top of
  /Users/pchiusano/unison/scratch.u
  
    broken : Optional Nat -> ()
    broken = cases
      Some
        loooooooooooooooooooooooooooooooooooooooooooooooooooooooong | loooooooooooooooooooooooooooooooooooooooooooooooooooooooong
        == 1 ->
        ()
  
  You can edit them there, then do `update` to replace the
  definitions currently in this namespace.

.> undo

  Here are the changes I undid
  
  Added definitions:
  
    1. broken : Optional Nat -> ()

```
```ucm
.> load scratch.u

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      broken : Optional Nat -> ()

```
## Guard patterns on long lines

```unison
structural type SomethingUnusuallyLong = SomethingUnusuallyLong Text Text Text

foo = let
  go x =
    'match (a -> a) x with
      SomethingUnusuallyLong lijaefliejalfijelfj aefilaeifhlei liaehjffeafijij |
        lijaefliejalfijelfj == aefilaeifhlei -> 0
      SomethingUnusuallyLong lijaefliejalfijelfj aefilaeifhlei liaehjffeafijij |
        lijaefliejalfijelfj == liaehjffeafijij -> 1
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
  /Users/pchiusano/unison/scratch.u
  
    structural type SomethingUnusuallyLong
      = SomethingUnusuallyLong Text Text Text
    
    foo : 'Nat
    foo =
      go x =
        '(match (a -> a) x with
            SomethingUnusuallyLong
              lijaefliejalfijelfj aefilaeifhlei liaehjffeafijij 
              | lijaefliejalfijelfj == aefilaeifhlei   -> 0
              | lijaefliejalfijelfj == liaehjffeafijij -> 1)
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
.> load scratch.u

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
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
  /Users/pchiusano/unison/scratch.u
  
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
          + 1,
        foo
          12939233
          2102020
          329292
          429292
          522020
          62929292
          72020202
          820202
          920202
          1020202 ]
  
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
.> load scratch.u

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
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
  /Users/pchiusano/unison/scratch.u
  
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
      c =
        foo do
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
.> load scratch.u

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      bar0 : x -> 'Nat
      bar1 : x -> 'Nat
      bar2 : x -> 'Nat
      bar3 : x -> '(b -> Nat)
      foo  : a -> b -> Nat

```
