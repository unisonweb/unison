# Tab Completion


Test that tab completion works as expected.


## Tab Complete Command Names

```ucm
.> debug.tab-complete vi

   view
   view.patch

.> debug.tab-complete delete.

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
-- Should tab complete namespaces since they may contain terms/types
.> debug.tab-complete view sub

   subnamespace.

-- Should complete things from child namespaces of the current query
.> debug.tab-complete view subnamespace

   subnamespace.
   subnamespace.AType
   subnamespace.AType.
   subnamespace.someName
   subnamespace.someOtherName

.> debug.tab-complete view subnamespace.

   subnamespace.AType
   subnamespace.AType.
   subnamespace.someName
   subnamespace.someOtherName

-- Should prefix-filter by query suffix
.> debug.tab-complete view subnamespace.some

   subnamespace.someName
   subnamespace.someOtherName

.> debug.tab-complete view subnamespace.someOther

   subnamespace.someOtherName

-- Should tab complete absolute names
.othernamespace> debug.tab-complete view .subnamespace.some

   .subnamespace.someName
   .subnamespace.someOtherName

```
## Tab complete namespaces

```ucm
-- Should tab complete namespaces
.> debug.tab-complete cd sub

   subnamespace

.> debug.tab-complete cd subnamespace

   subnamespace
   subnamespace.AType

.> debug.tab-complete cd subnamespace.

   subnamespace.AType

```
