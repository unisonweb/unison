```ucm:hide
.> builtins.mergeio
```

Demonstrating `create.author`:

```unison:hide
def1 = 1
def2 = 2
```

```ucm
.> add
.> create.author alicecoder "Alice McGee"
.> view 2
.> link metadata.authors.alicecoder def1 def2
```
