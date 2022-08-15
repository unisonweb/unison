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
othernamespace.someName = 3

unique type subnamespace.AType = A | B
```

```ucm:hide
.> add
```

```ucm
-- Should tab complete namespaces since they may contain terms/types
.> debug.tab-complete view sub
-- Should complete things from child namespaces of the current query
.> debug.tab-complete view subnamespace
.> debug.tab-complete view subnamespace.
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
```
