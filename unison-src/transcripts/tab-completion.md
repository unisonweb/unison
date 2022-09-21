# Tab Completion


Test that tab completion works as expected.


## Tab Complete Command Names

```ucm
.> debug.tab-complete vi
.> debug.tab-complete delete.
```

## Tab complete terms & types

```unison
subnamespace.someName = 1
subnamespace.someOtherName = 2
subnamespace2.thing = 3
othernamespace.someName = 4

unique type subnamespace.AType = A | B
```

```ucm:hide
.> add
```

```ucm
-- Should tab complete namespaces since they may contain terms/types
.> debug.tab-complete view sub
-- Should not complete things from child namespaces of the current query if there are other completions at this level
.> debug.tab-complete view subnamespace
-- Should complete things from child namespaces of the current query if it's dot-suffixed
.> debug.tab-complete view subnamespace.
-- Should complete things from child namespaces of the current query if there are no more completions at this level.
.> debug.tab-complete view subnamespace2
-- Should prefix-filter by query suffix
.> debug.tab-complete view subnamespace.some
.> debug.tab-complete view subnamespace.someOther
-- Should tab complete absolute names
.othernamespace> debug.tab-complete view .subnamespace.some
```

## Tab complete namespaces

```ucm
-- Should tab complete namespaces
.> debug.tab-complete cd sub
.> debug.tab-complete cd subnamespace
.> debug.tab-complete cd subnamespace.
.> debug.tab-complete io.test sub
.> debug.tab-complete io.test subnamespace
.> debug.tab-complete io.test subnamespace.
```
