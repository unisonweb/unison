# Tab Completion


Test that tab completion works as expected.


## Tab Complete Command Names

```ucm
.> debug.tab-complete vi

   view
   view.global
   view.patch

.> debug.tab-complete delete.

   delete.branch
   delete.link
   delete.namespace
   delete.namespace.force
   delete.patch
   delete.term
   delete.term-replacement
   delete.term.verbose
   delete.type
   delete.type-replacement
   delete.type.verbose
   delete.verbose

```
## Tab complete terms & types

```unison
subnamespace.someName = 1
subnamespace.someOtherName = 2
subnamespace2.thing = 3
othernamespace.someName = 4

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
      subnamespace2.thing        : ##Nat

```
```ucm
-- Should tab complete namespaces since they may contain terms/types
.> debug.tab-complete view sub

   subnamespace.
   subnamespace2.

-- Should not complete things from child namespaces of the current query if there are other completions at this level
.> debug.tab-complete view subnamespace

   subnamespace.
   subnamespace2.

-- Should complete things from child namespaces of the current query if it's dot-suffixed
.> debug.tab-complete view subnamespace.

  * subnamespace.AType
    subnamespace.AType.
  * subnamespace.someName
  * subnamespace.someOtherName

-- Should complete things from child namespaces of the current query if there are no more completions at this level.
.> debug.tab-complete view subnamespace2

    subnamespace2.
  * subnamespace2.thing

-- Should prefix-filter by query suffix
.> debug.tab-complete view subnamespace.some

  * subnamespace.someName
  * subnamespace.someOtherName

.> debug.tab-complete view subnamespace.someOther

  * subnamespace.someOtherName

-- Should tab complete absolute names
.othernamespace> debug.tab-complete view .subnamespace.some

  * .subnamespace.someName
  * .subnamespace.someOtherName

```
## Tab complete namespaces

```ucm
-- Should tab complete namespaces
.> debug.tab-complete cd sub

   subnamespace
   subnamespace2

.> debug.tab-complete cd subnamespace

   subnamespace
   subnamespace2

.> debug.tab-complete cd subnamespace.

   subnamespace.AType

.> debug.tab-complete io.test sub

   subnamespace.
   subnamespace2.

.> debug.tab-complete io.test subnamespace

   subnamespace.
   subnamespace2.

.> debug.tab-complete io.test subnamespace.

    subnamespace.AType.
  * subnamespace.someName
  * subnamespace.someOtherName

```
