# Update on conflict

```ucm:hide
scratch/main> builtins.merge
scratch/main merged> builtins.merge
```

```unison
a.x = 1
b.x = 2
```

Cause a conflict:
```ucm
scratch/main> add
scratch/main merged> merge.old .a
scratch/main merged> merge.old .b
```

Updating conflicted definitions works fine.

```unison
x = 3
```

```ucm
scratch/main merged> update
```
