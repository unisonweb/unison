```unison
unique type Foo = Bar Nat
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      unique type Foo

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    unique type Foo

.> delete.term Foo.Bar

  Done.

```
Now we've set up a situation where the original constructor missing.

```unison
unique type Foo = Bar Nat Nat
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      unique type Foo

```
```ucm
.> view Foo

  unique type Foo = #b509v3eg4k#0 Nat

.> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  I couldn't complete the update because I couldn't find 1
  constructor(s) for Foo where I expected to. I found: []
  
  You can use `view Foo` and
  `alias.term <hash> Foo.<ConstructorName>` to give names to
  each constructor, and then try again.

```
