```unison
unique type Foo = { bar : Nat }
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      unique type Foo
      Foo.bar        : Foo -> Nat
      Foo.bar.modify : (Nat ->{g} Nat) -> Foo ->{g} Foo
      Foo.bar.set    : Nat -> Foo -> Foo

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    unique type Foo
    Foo.bar        : Foo -> Nat
    Foo.bar.modify : (Nat ->{g} Nat) -> Foo ->{g} Foo
    Foo.bar.set    : Nat -> Foo -> Foo

```
Bug: this no-op update should (of course) succeed.

```ucm
.> update

  unique type Foo = { bar : Nat }
  
  Foo.bar.modify f = cases Foo bar -> Foo (f bar)
  
  Foo.bar.set bar1 = cases Foo _ -> Foo bar1
  
  Foo.bar = cases Foo bar -> bar

  Typechecking failed when propagating the update to all the dependents.

```
