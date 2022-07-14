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
.> cd .
```

Ideally we could just define the canonical `x` that we want, and update
to accept it, but we can't:

```unison
x = 1 + 2
```

Update fails on conflicted `x`:

```ucm:error
.merged> update
```
