# find.patch Test

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
.> view.patch patch
.> view.patch 1
```
