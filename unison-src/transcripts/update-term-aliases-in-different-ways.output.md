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

bar : Nat
bar = 7
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      bar : Nat
        (The old definition is also named foo. I'll update this
        name too.)
      foo : Nat
        (The old definition is also named bar. I'll update this
        name too.)

```
```ucm
.> update

.> view foo bar

  bar : Nat
  bar = 7
  
  foo : Nat
  foo = 6

```
