```ucm
.> builtins.merge

  Done.

```
```unison
foo : Nat
foo = 5
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo : Nat

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    foo : Nat

```
```unison
foo : Int
foo = +5
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      foo : Int

```
```ucm
.> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

.> view foo

  foo : Int
  foo = +5

```
