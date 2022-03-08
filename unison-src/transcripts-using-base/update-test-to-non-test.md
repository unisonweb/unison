When updating a term from a test to a non-test, we don't delete its metadata that indicates it's a test.

```unison
test> foo = []
```

```ucm
.> add
```

```unison
foo = 1
```

```ucm
.> update
.> links foo
```
