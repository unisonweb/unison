```unison
unique type Foo = Bar Nat
unique type Baz = Qux Foo
```

```ucm

  Loading changes detected in scratch.u.

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

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      unique type Foo a

```
```ucm
.> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Now I'm making sure everything typechecks...

  unique type Baz = Qux Foo
  
  unique type Foo a = Bar Nat a

  Typechecking failed. I've updated your scratch file with the
  definitions that need fixing. Once the file is compiling, try
  `update` again.

```
