### Transcript parser operations

```ucm:hide
.> builtins.merge
```

The transcript parser is meant to parse `ucm` and `unison` blocks.

```unison
x = 1
```

```ucm
.> add
```

```unison:hide:error:scratch.u
z
```

```ucm:error
.> delete foo
```

```ucm :error
.> delete lineToken.call
```

However handling of blocks of other languages should be supported.

```python
some python code
```

```c_cpp
some C++ code
```

```c9search
some cloud9 code
```
