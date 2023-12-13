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

.> move.term Foo.Bar Stray.Bar

  Done.

```
Now we've set up a situation where the constructor is not where it's supposed to be; it's somewhere else.

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
Note that the constructor name shown here (implied to be called `Foo.Stray.Bar`) doesn't really exist, it's just showing up due to a pretty-printer bug.

```ucm
.> view Foo

  unique type Foo = Stray.Bar Nat

.> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  I couldn't complete the update because one of your types, Foo,
  has unnamed constructors.
  
  I currently need each constructor to have a name somewhere
  under the type name.
  
  You can use `view Foo` and
  `alias.term <hash> Foo.<ConstructorName>` to give names to
  each constructor, and then try the update again.

```
