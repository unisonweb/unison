```ucm
.> builtins.merge

  Done.

.> move.namespace builtin lib.builtin

  Done.

```
```unison
structural type Foo = Bar Nat

incrFoo : Foo -> Foo
incrFoo = cases Bar n -> Bar (n+1)
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      structural type Foo
      incrFoo : Foo -> Foo

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    structural type Foo
    incrFoo : Foo -> Foo

```
```unison
structural type Foo = Bar Nat Nat
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

  incrFoo : Foo -> Foo
  incrFoo = cases Bar n -> Bar (n Nat.+ 1)
  
  structural type Foo = Bar Nat Nat

  Typechecking failed when propagating the update to all the dependents.

```
