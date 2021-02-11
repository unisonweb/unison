This transcript tests that UCM can always access the definition of 
`IsTest`/`isTest`, which is used internally.

```ucm
.> builtins.merge
```

```unison
test> pass = [Ok "Passed"]
```

```ucm
.> add
.> links pass
.> display 1
```

The definition and type of `isTest` should exist.
