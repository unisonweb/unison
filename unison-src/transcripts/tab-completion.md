# Tab Completion

Test that tab completion works as expected.

## Tab Complete Command Names

```ucm
scratch/main> debug.tab-complete vi
scratch/main> debug.tab-complete delete.
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
scratch/main> add
```

```ucm
-- Should tab complete namespaces since they may contain terms/types
scratch/main> debug.tab-complete view sub
-- Should not complete things from child namespaces of the current query if there are other completions at this level
scratch/main> debug.tab-complete view subnamespace
-- Should complete things from child namespaces of the current query if it's dot-suffixed
scratch/main> debug.tab-complete view subnamespace.
-- Should complete things from child namespaces of the current query if there are no more completions at this level.
scratch/main> debug.tab-complete view subnamespace2
-- Should prefix-filter by query suffix
scratch/main> debug.tab-complete view subnamespace.some
scratch/main> debug.tab-complete view subnamespace.someOther
```

```unison:hide
absolute.term = "absolute"
```

```ucm
scratch/main> add
-- Should tab complete absolute names
scratch/main> debug.tab-complete view .absolute.te
```

## Tab complete namespaces

```ucm
-- Should tab complete namespaces
scratch/main> debug.tab-complete find-in sub
scratch/main> debug.tab-complete find-in subnamespace
scratch/main> debug.tab-complete find-in subnamespace.
scratch/main> debug.tab-complete io.test sub
scratch/main> debug.tab-complete io.test subnamespace
scratch/main> debug.tab-complete io.test subnamespace.
```

Tab Complete Delete Subcommands

```unison
unique type Foo = A | B
add : a -> a
add b = b
```

```ucm
scratch/main> update.old
scratch/main> debug.tab-complete delete.type Foo
scratch/main> debug.tab-complete delete.term add
```

## Tab complete projects and branches

```ucm
myproject/main> branch mybranch
myproject/main> debug.tab-complete branch.delete /mybr
myproject/main> debug.tab-complete project.rename my
```

Commands which complete namespaces OR branches should list both

```unison
mybranchsubnamespace.term = 1
```


```ucm
myproject/main> add
myproject/main> debug.tab-complete merge mybr
```
