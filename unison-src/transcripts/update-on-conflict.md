# Update on conflict

```ucm:hide
.> builtins.merge
```

```unison
a.x = 1
b.x = 2
```

Cause a conflict:
```ucm
.> add
.merged> merge .a
.merged> merge .b
```

Updating conflicted definitions works fine, and the associated patch contains two entries.

```unison
x = 3
```

```ucm
.merged> update
.merged> view.patch
```
