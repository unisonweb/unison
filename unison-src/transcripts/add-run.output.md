# add.run

## Basic usage

```unison
even : Nat -> Boolean
even x = if x == 0 then true else odd (drop x 1)

odd : Nat -> Boolean
odd x = if x == 0 then false else even (drop x 1)

is2even : 'Boolean
is2even = '(even 2)
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      even    : Nat -> Boolean
      is2even : 'Boolean
      odd     : Nat -> Boolean

```
it errors if there isn't a previous run

```ucm
.> add.run foo

  ⚠️
  
  There is no previous evaluation to save. Use `run` to evaluate
  something before attempting to save it.

```
```ucm
.> run is2even

  true

```
it errors if the desired result name conflicts with a name in the
unison file
```ucm
.> add.run is2even

  ⚠️
  
  Cannot save the last run result into `is2even` because that
  name conflicts with a name in the scratch file.

```
otherwise, the result is successfully persisted
```ucm
.> add.run foo.bar.baz

  ⍟ These new definitions are ok to `add`:
  
    foo.bar.baz : Boolean

```
```ucm
.> view foo.bar.baz

  foo.bar.baz : Boolean
  foo.bar.baz = true

```
## It resolves references within the unison file

```unison
z b = b Nat.+ 12
y a b = a Nat.+ b Nat.+ z 10




main : '{IO, Exception} (Nat -> Nat -> Nat)
main _ = y
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      main : '{IO, Exception} (Nat -> Nat -> Nat)
      y    : Nat -> Nat -> Nat
      z    : Nat -> Nat

```
```ucm
.> run main

  a b -> a Nat.+ b Nat.+ z 10

.> add.run result

  ⍟ These new definitions are ok to `add`:
  
    result : Nat -> Nat -> Nat
    z      : Nat -> Nat

```
## It resolves references within the codebase

```unison
inc : Nat -> Nat
inc x = x + 1
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      inc : Nat -> Nat

```
```ucm
.> add inc

  ⍟ I've added these definitions:
  
    inc : Nat -> Nat

```
```unison
main : '(Nat -> Nat)
main _ x = inc x
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      main : '(Nat -> Nat)

```
```ucm
.> run main

  inc

.> add.run natfoo

  ⍟ These new definitions are ok to `add`:
  
    natfoo : Nat -> Nat

.> view natfoo

  natfoo : Nat -> Nat
  natfoo = inc

```
## It captures scratch file dependencies at run time

```unison
x = 1
y = x + x
main = 'y
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      main : 'Nat
      x    : Nat
      y    : Nat

```
```ucm
.> run main

  2

```
```unison
x = 50
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      x : Nat

```
```ucm
.> add.run xres

  ⍟ These new definitions are ok to `add`:
  
    xres : Nat

.> view xres

  xres : Nat
  xres = 2

```
