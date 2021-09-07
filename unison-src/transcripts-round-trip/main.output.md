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
    `fork #pqvd5behc2 .old`   to make an old namespace
                              accessible again,
                              
    `reset-root #pqvd5behc2`  to reset the root namespace and
                              its history to that of the
                              specified namespace.
  
  1. #8rn1an5gj8 : add
  2. #pqvd5behc2 : builtins.mergeio
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
  /Users/pchiusano/unison/scratch.u
  
    b : Nat
    b = 92384
  
  You can edit them there, then do `update` to replace the
  definitions currently in this namespace.

.> reflog

  Here is a log of the root namespace hashes, starting with the
  most recent, along with the command that got us there. Try:
  
    `fork 2 .old`             
    `fork #pqvd5behc2 .old`   to make an old namespace
                              accessible again,
                              
    `reset-root #pqvd5behc2`  to reset the root namespace and
                              its history to that of the
                              specified namespace.
  
  1. #dbvse9969b : add
  2. #pqvd5behc2 : reset-root #pqvd5behc2
  3. #8rn1an5gj8 : add
  4. #pqvd5behc2 : builtins.mergeio
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
No reason you can't load a bunch of definitions from a single `.u` file in one go, the only thing that's annoying is you'll have to `find` and then `edit 1-11 in the transcript to load all the definitions into the file.

## Example 1

Add tests here
