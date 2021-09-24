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
  /Users/runar/work/unison/scratch.u
  
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
    `fork #bt17giel42 .old`   to make an old namespace
                              accessible again,
                              
    `reset-root #bt17giel42`  to reset the root namespace and
                              its history to that of the
                              specified namespace.
  
  1. #agadr8gg6g : add
  2. #bt17giel42 : builtins.mergeio
  3. #sjg2v58vn2 : (initial reflogged namespace)

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
  /Users/runar/work/unison/scratch.u
  
    b : Nat
    b = 92384
  
  You can edit them there, then do `update` to replace the
  definitions currently in this namespace.

.> reflog

  Here is a log of the root namespace hashes, starting with the
  most recent, along with the command that got us there. Try:
  
    `fork 2 .old`             
    `fork #bt17giel42 .old`   to make an old namespace
                              accessible again,
                              
    `reset-root #bt17giel42`  to reset the root namespace and
                              its history to that of the
                              specified namespace.
  
  1. #rhf1s808fb : add
  2. #bt17giel42 : reset-root #bt17giel42
  3. #agadr8gg6g : add
  4. #bt17giel42 : builtins.mergeio
  5. #sjg2v58vn2 : (initial reflogged namespace)

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
  /Users/runar/work/unison/scratch.u
  
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
    `fork #bt17giel42 .old`   to make an old namespace
                              accessible again,
                              
    `reset-root #bt17giel42`  to reset the root namespace and
                              its history to that of the
                              specified namespace.
  
  1. #gj5agagj7s : add
  2. #bt17giel42 : reset-root #bt17giel42
  3. #rhf1s808fb : add
  4. #bt17giel42 : reset-root #bt17giel42
  5. #agadr8gg6g : add
  6. #bt17giel42 : builtins.mergeio
  7. #sjg2v58vn2 : (initial reflogged namespace)

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
  /Users/runar/work/unison/scratch.u
  
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
    `fork #bt17giel42 .old`   to make an old namespace
                              accessible again,
                              
    `reset-root #bt17giel42`  to reset the root namespace and
                              its history to that of the
                              specified namespace.
  
  1. #3igmh2it4p : add
  2. #bt17giel42 : reset-root #bt17giel42
  3. #gj5agagj7s : add
  4. #bt17giel42 : reset-root #bt17giel42
  5. #rhf1s808fb : add
  6. #bt17giel42 : reset-root #bt17giel42
  7. #agadr8gg6g : add
  8. #bt17giel42 : builtins.mergeio
  9. #sjg2v58vn2 : (initial reflogged namespace)

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
  /Users/runar/work/unison/scratch.u
  
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
    `fork #bt17giel42 .old`   to make an old namespace
                              accessible again,
                              
    `reset-root #bt17giel42`  to reset the root namespace and
                              its history to that of the
                              specified namespace.
  
  1.  #jsnoueu9le : add
  2.  #bt17giel42 : reset-root #bt17giel42
  3.  #3igmh2it4p : add
  4.  #bt17giel42 : reset-root #bt17giel42
  5.  #gj5agagj7s : add
  6.  #bt17giel42 : reset-root #bt17giel42
  7.  #rhf1s808fb : add
  8.  #bt17giel42 : reset-root #bt17giel42
  9.  #agadr8gg6g : add
  10. #bt17giel42 : builtins.mergeio
  11. #sjg2v58vn2 : (initial reflogged namespace)

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
  /Users/runar/work/unison/scratch.u
  
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
    `fork #bt17giel42 .old`   to make an old namespace
                              accessible again,
                              
    `reset-root #bt17giel42`  to reset the root namespace and
                              its history to that of the
                              specified namespace.
  
  1.  #vbmanbqtlh : add
  2.  #bt17giel42 : reset-root #bt17giel42
  3.  #jsnoueu9le : add
  4.  #bt17giel42 : reset-root #bt17giel42
  5.  #3igmh2it4p : add
  6.  #bt17giel42 : reset-root #bt17giel42
  7.  #gj5agagj7s : add
  8.  #bt17giel42 : reset-root #bt17giel42
  9.  #rhf1s808fb : add
  10. #bt17giel42 : reset-root #bt17giel42
  11. #agadr8gg6g : add
  12. #bt17giel42 : builtins.mergeio
  13. #sjg2v58vn2 : (initial reflogged namespace)

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
  /Users/runar/work/unison/scratch.u
  
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
