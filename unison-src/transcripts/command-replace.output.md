# Replace with terms and types

Let's set up some definitions to start:

```unison
x = 1
y = 2

structural type X = One Nat
structural type Y = Two Nat Nat
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      structural type X
      structural type Y
      x : Nat
      y : Nat

```
```ucm
  ☝️  The namespace .scratch is empty.

.scratch> add

  ⍟ I've added these definitions:
  
    structural type X
    structural type Y
    x : Nat
    y : Nat

```
Test that replace works with terms
```ucm
.scratch> replace x y

  Done.

.scratch> view x

  x : Nat
  x = 2

```
Test that replace works with types
```ucm
.scratch> replace X Y

  Done.

.scratch> find

  1. structural type X
  2. x : Nat
  3. X.One : Nat -> Nat -> X
  4. structural type Y
  5. y : Nat
  6. Y.Two : Nat -> Nat -> X
  

.scratch> view.patch patch

  Edited Types: X#hhc6goudjq -> X
  
  Edited Terms: #rrsqv1ogaq -> x
  
  Tip: To remove entries from a patch, use
       delete.term-replacement or delete.type-replacement, as
       appropriate.

.scratch> view X

  structural type X = One Nat Nat

```
Try with a type/term mismatch
```ucm
.scratch> replace X x

  ⚠️
  
  I was expecting either two types or two terms but was given a type X and a term x.

```
```ucm
.scratch> replace y Y

  ⚠️
  
  I was expecting either two types or two terms but was given a type Y and a term y.

```
Try with missing references
```ucm
.scratch> replace X NOPE

  ⚠️
  
  The following names were not found in the codebase. Check your spelling.
    NOPE

```
```ucm
.scratch> replace y nope

  ⚠️
  
  The following names were not found in the codebase. Check your spelling.
    nope

```
```ucm
.scratch> replace nope X

  ⚠️
  
  The following names were not found in the codebase. Check your spelling.
    nope

```
```ucm
.scratch> replace nope y

  ⚠️
  
  The following names were not found in the codebase. Check your spelling.
    nope

```
```ucm
.scratch> replace nope nope

  ⚠️
  
  The following names were not found in the codebase. Check your spelling.
    nope
    nope

```
