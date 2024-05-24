# Update on conflict

```ucm:hide
.> builtins.merge
.merged> builtins.merge
```

```unison
a.x = 1
b.x = 2
```

Cause a conflict:
```ucm
.> add
.merged> merge.old .a
.merged> merge.old .b
```

Updating conflicted definitions works fine.

```unison
x = 3
```

```ucm
.merged> update
```
