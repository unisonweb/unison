# tests for the new merge command

## Basic fast-forward merge

```unison
foo : Nat
foo = 1
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo : Nat

```
```ucm
proj/main> add

  ⍟ I've added these definitions:
  
    foo : Nat

proj/main> branch topic

  Done. I've created the topic branch based off of main.
  
  Tip: Use `merge /topic /main` to merge your work back into the
       main branch.

```
```unison
bar : Nat
bar = foo + 1
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      bar : Nat

```
```ucm
proj/topic> add

  ⍟ I've added these definitions:
  
    bar : Nat

proj/main> merge2 /topic

  I merged proj/topic into proj/main.

proj/main> view bar

  bar : Nat
  bar =
    use Nat +
    foo + 1

```
## Basic conflict

```unison
foo : Nat
foo = 1
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo : Nat

```
```ucm
proj/topic> add

  ⍟ I've added these definitions:
  
    foo : Nat

```
```unison
foo : Nat
foo = 4
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo : Nat

```
```ucm
proj/main> add

  ⍟ I've added these definitions:
  
    foo : Nat

proj/main> merge2 /topic

  I couldn't automatically merge proj/topic into proj/main.
  However, I've added the definitions that need attention to the
  top of scratch.u.

```
```unison:added-by-ucm scratch.u
foo : Nat
foo = 4

foo : Nat
foo = 1
```

## Altered dependent

`foo : Nat` is in the ancestor of `main` and `topic`

```unison
foo : Nat
foo = 1
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo : Nat

```
```ucm
proj/main> add

  ⍟ I've added these definitions:
  
    foo : Nat

proj/main> branch topic

  Done. I've created the topic branch based off of main.
  
  Tip: Use `merge /topic /main` to merge your work back into the
       main branch.

```
`topic` adds a dependent of `foo`

```unison
bar : Nat
bar = foo + 1
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      bar : Nat

```
```ucm
proj/topic> add

  ⍟ I've added these definitions:
  
    bar : Nat

```
`main` changes the type of `foo`

```unison
foo : Int
foo = +1
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      foo : Int

```
```ucm
proj/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

```
attempt to merge `topic` into `main`

```ucm
proj/main> merge2 /topic

  I couldn't automatically merge proj/topic into proj/main.
  However, I've added the definitions that need attention to the
  top of scratch.u.

```
```unison:added-by-ucm scratch.u
bar : Nat
bar =
  use Nat +
  foo + 1
```

