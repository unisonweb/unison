# find.patch Test

```ucm:hide
.> builtins.merge
```

```unison test.u
hey = "yello"
```

```ucm
.> add
```

Update

```unison test.u
hey = "hello"
```

Update

```ucm
.> update.old
.> find.patch
.> view.patch 1
```
