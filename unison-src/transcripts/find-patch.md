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
.> update
.> find.patch
.> view.patch 1
```
