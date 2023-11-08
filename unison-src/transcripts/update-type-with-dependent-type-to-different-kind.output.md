```ucm
.> builtins.merge

  Done.

.> move.namespace builtin lib.builtin

  Done.

```
```unison
structural type Foo = Bar Nat
structural type Baz = Qux Foo
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      structural type Baz
      structural type Foo

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    structural type Baz
    structural type Foo

```
```unison
structural type Foo a = Bar Nat a
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      structural type Foo a

```
```ucm
.> update

  structural type Baz = Qux Foo
  
  structural type Foo a = Bar Nat a

  Typechecking failed when propagating the update to all the dependents.

```
