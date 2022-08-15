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

```ucm
.> add
.> debug.tab-complete view sub
.> debug.tab-complete view subnamespace
.> debug.tab-complete view subnamespace.
.> debug.tab-complete view subnamespace.some
.> debug.tab-complete view subnamespace.someOther
```
