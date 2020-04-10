```ucm:hide
.> builtins.mergeio
```

Demonstrating `create.author`:

```unison:hide
def1 = 1
def2 = 2
```

```ucm
.foo> add
.foo> create.author alicecoder "Alice McGee"
.foo> view 3
.foo> link metadata.authors.alicecoder def1 def2
```
