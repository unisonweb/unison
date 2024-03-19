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

  I merged topic into main.

proj/main> view bar

  bar : Nat
  bar =
    use Nat +
    foo + 1

```
## Add/Add conflict

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

  I couldn't automatically merge topic into main. However, I've
  added the definitions that need attention to the top of
  scratch.u.

```
```unison:added-by-ucm scratch.u
foo : Nat
foo = 4

foo : Nat
foo = 1
```

## Update/Update conflict

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
foo : Nat
foo = 2
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      foo : Nat

```
```ucm
proj/topic> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

```
```unison
foo : Nat
foo = 3
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      foo : Nat

```
```ucm
proj/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

```
```ucm
proj/main> merge2 /topic

  I couldn't automatically merge topic into main. However, I've
  added the definitions that need attention to the top of
  scratch.u.

```
```unison:added-by-ucm scratch.u
foo : Nat
foo = 3

foo : Nat
foo = 2
```

## Update/Delete conflict

We don't consider these, so this transcript is capturing our
ignorance.

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

proj/topic> delete.term foo

  Done.

```
```unison
foo : Nat
foo = 2
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
proj/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

```
We silently ignore the delete

```ucm
proj/main> merge2 /topic

  I merged topic into main.

proj/main> view foo

  foo : Nat
  foo = 2

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

  I couldn't automatically merge topic into main. However, I've
  added the definitions that need attention to the top of
  scratch.u.

```
```unison:added-by-ucm scratch.u
bar : Nat
bar =
  use Nat +
  foo + 1
```

