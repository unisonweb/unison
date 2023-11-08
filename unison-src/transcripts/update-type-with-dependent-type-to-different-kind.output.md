```unison
unique type Foo = Bar Nat
unique type Baz = Qux Foo
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      unique type Baz
      unique type Foo

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    unique type Baz
    unique type Foo

```
```unison
unique type Foo a = Bar Nat a
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      unique type Foo a

```
```ucm
.> update

  unique type Baz = Qux Foo
  
  unique type Foo a = Bar Nat a

  Typechecking failed when propagating the update to all the dependents.

```
