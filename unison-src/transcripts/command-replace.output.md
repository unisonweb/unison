# Replace with terms and types

Let's set up some definitions to start:

```unison
x = 1
y = 2

type X = One Nat
type Y = Two Nat Nat
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      type X
      type Y
      x : Nat
      y : Nat

```
```ucm
  ☝️  The namespace .scratch is empty.

.scratch> add

  ⍟ I've added these definitions:
  
    type X
    type Y
    x : Nat
    y : Nat

```
Test that replace works with terms
```ucm
.scratch> replace.any x y

  Done.

.scratch> view x

  x : Nat
  x = 2

```
Test that replace works with types
```ucm
.scratch> replace.any X Y

  Done.

.scratch> view X

  type X = Two Nat Nat

```
Try with a type/term mismatch
```ucm
.scratch> replace.any X x

  ⚠️
  
  The following names were not found in the codebase. Check your spelling.
    X

```
```ucm
.scratch> replace.any y Y 

  ⚠️
  
  The following names were not found in the codebase. Check your spelling.
    Y

```
Try with missing references
```ucm
.scratch> replace.any X NOPE

  ⚠️
  
  The following names were not found in the codebase. Check your spelling.
    NOPE

```
```ucm
.scratch> replace.any y nope

  ⚠️
  
  The following names were not found in the codebase. Check your spelling.
    nope

```
```ucm
.scratch> replace.any nope X

  ⚠️
  
  The following names were not found in the codebase. Check your spelling.
    nope

```
```ucm
.scratch> replace.any nope y

  ⚠️
  
  The following names were not found in the codebase. Check your spelling.
    nope

```
```ucm
.scratch> replace.any nope nope

  ⚠️
  
  The following names were not found in the codebase. Check your spelling.
    nope
    nope

```
