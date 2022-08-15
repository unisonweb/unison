# Tab Completion


Test that tab completion works as expected.


## Tab Complete Command Names

```ucm
.> debug.tab-complete vi

  Finished completions are prefixed with a *
   view
   view.patch

.> debug.tab-complete delete.

  Finished completions are prefixed with a *
   delete.link
   delete.namespace
   delete.namespace.force
   delete.patch
   delete.term
   delete.term-replacement
   delete.type
   delete.type-replacement

```
## Tab complete terms & types

```unison
subnamespace.someName = 1
subnamespace.someOtherName = 2
othernamespace.someName = 3

unique type subnamespace.AType = A | B
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      unique type subnamespace.AType
      othernamespace.someName    : ##Nat
      subnamespace.someName      : ##Nat
      subnamespace.someOtherName : ##Nat

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    unique type subnamespace.AType
    othernamespace.someName    : ##Nat
    subnamespace.someName      : ##Nat
    subnamespace.someOtherName : ##Nat

.> debug.tab-complete view sub

  Finished completions are prefixed with a *
   subnamespace.

.> debug.tab-complete view subnamespace

  Finished completions are prefixed with a *
   subnamespace.
   subnamespace.AType
   subnamespace.AType.
   subnamespace.someName
   subnamespace.someOtherName

.> debug.tab-complete view subnamespace.

  Finished completions are prefixed with a *
   subnamespace.AType
   subnamespace.AType.
   subnamespace.someName
   subnamespace.someOtherName

.> debug.tab-complete view subnamespace.some

  Finished completions are prefixed with a *
   subnamespace.someName
   subnamespace.someOtherName

.> debug.tab-complete view subnamespace.someOther

  Finished completions are prefixed with a *
   subnamespace.someOtherName

```
