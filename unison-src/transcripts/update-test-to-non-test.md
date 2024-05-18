```ucm
.> builtins.merge
```

```unison
test> foo = []
```

After adding the test `foo`, we expect `view` to render it like a test. (Bug: It doesn't.)

```ucm
.> add
.> view foo
```

```unison
foo = 1
```

After updating `foo` to not be a test, we expect `view` to not render it like a test.

```ucm
.> update
.> view foo
```
