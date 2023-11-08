```ucm
.> builtins.merge

  Done.

.> move.namespace builtin lib.builtin

  Done.

```
```unison
structural type Foo
  = Bar Nat
  | Baz Nat Nat

foo : Foo -> Nat
foo = cases
  Bar n -> n
  Baz n m -> n + m
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      structural type Foo
      foo : Foo -> Nat

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    structural type Foo
    foo : Foo -> Nat

```
```unison
structural type Foo
  = Bar Nat
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      structural type Foo

```
```ucm
.> update

  foo : Foo -> Nat
  foo = cases
    Bar n   -> n
    Baz n m -> n Nat.+ m
  
  structural type Foo = Bar Nat

  Typechecking failed when propagating the update to all the dependents.

```
