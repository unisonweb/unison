```ucm
.> builtins.merge
```

```unison
test> pass = [Ok "Passed"]
```

```ucm
.> add
```

```unison
test> pass = [Ok "Passed"]
```

```ucm
.> add
.> test
```

```unison
> Scope.run do
    freeze! (Scope.arrayOf 0 0)

```
