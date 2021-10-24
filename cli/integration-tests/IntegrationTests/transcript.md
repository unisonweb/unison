# Integration test: transcript

```ucm:hide
.> builtins.mergeio
```

Test

```unison
use .builtin

coolFunction x = x * 2

coolFunction.doc = [: This is a cool function. :]
```

```ucm
.> add
.> link coolFunction.doc coolFunction
```
