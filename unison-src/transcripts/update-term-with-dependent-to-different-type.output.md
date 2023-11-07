```ucm
.> builtins.merge

  Done.

```
```unison
foo : Nat
foo = 5

bar : Nat
bar = foo + 10
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

  bar : Nat
  bar =
    use Nat +
    foo + 10
  
  foo : Int
  foo = +5

  Typechecking failed when propagating the update to all the dependents.

```
