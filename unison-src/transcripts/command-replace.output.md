# Replace with terms and types

Let's set up some definitions to start:

```unison
x = 1
y = 2

structural type X = One Nat
structural type Y = Two Nat Nat
```

```ucm

  Loading changes detected in scratch.u.

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
.> add

  ⍟ I've added these definitions:
  
    structural type X
    structural type Y
    x : Nat
    y : Nat

```
Test that replace works with terms
```ucm
.> replace x y

  Done.

.> view x

  x : Nat
  x = 2

```
Test that replace works with types
```ucm
.> replace X Y

  Done.

.> find

  1. structural type X
  2. x : Nat
  3. X.One : Nat -> Nat -> X
  4. structural type Y
  5. y : Nat
  6. Y.Two : Nat -> Nat -> X
  

.> view.patch patch

  Edited Types: 1. #68k40ra7l7 -> 3. X
  
  Edited Terms: 2. #gjmq673r1v -> 4. x
  
  Tip: To remove entries from a patch, use
       delete.term-replacement or delete.type-replacement, as
       appropriate.

.> view X

  structural type X = One Nat Nat

```
Try with a type/term mismatch
```ucm
.> replace X x

  ⚠️
  
  I was expecting either two types or two terms but was given a type X and a term x.

```
```ucm
.> replace y Y

  ⚠️
  
  I was expecting either two types or two terms but was given a type Y and a term y.

```
Try with missing references
```ucm
.> replace X NOPE

  ⚠️
  
  The following names were not found in the codebase. Check your spelling.
    NOPE

```
```ucm
.> replace y nope

  ⚠️
  
  The following names were not found in the codebase. Check your spelling.
    nope

```
```ucm
.> replace nope X

  ⚠️
  
  The following names were not found in the codebase. Check your spelling.
    nope

```
```ucm
.> replace nope y

  ⚠️
  
  The following names were not found in the codebase. Check your spelling.
    nope

```
```ucm
.> replace nope nope

  ⚠️
  
  The following names were not found in the codebase. Check your spelling.
    nope
    nope

```
