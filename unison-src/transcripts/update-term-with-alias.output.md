```ucm
.> builtins.merge

  Done.

```
```unison
foo : Nat
foo = 5

bar : Nat
bar = 5
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      bar : Nat
      foo : Nat

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    bar : Nat
    foo : Nat

```
```unison
foo : Nat
foo = 6
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      foo : Nat
        (The old definition is also named bar. I'll update this
        name too.)

```
```ucm
.> update

  I propagated the update and am now saving the results.

  Done.

.> view foo bar

  bar : Nat
  bar = 5
  
  foo : Nat
  foo = 6

```
