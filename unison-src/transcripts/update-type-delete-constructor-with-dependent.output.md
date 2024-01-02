```unison
unique type Foo
  = Bar Nat
  | Baz Nat Nat

foo : Foo -> Nat
foo = cases
  Bar n -> n
  Baz n m -> n + m
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      unique type Foo
      foo : Foo -> Nat

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    unique type Foo
    foo : Foo -> Nat

```
```unison
unique type Foo
  = Bar Nat
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      unique type Foo

```
```ucm
.> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Now I'm making sure everything typechecks...

  foo : Foo -> Nat
  foo = cases
    Bar n   -> n
    Baz n m -> n Nat.+ m
  
  unique type Foo = Bar Nat

  Typechecking failed. I've updated your scratch file with the
  definitions that need fixing. Once the file is compiling, try
  `update` again.

```
