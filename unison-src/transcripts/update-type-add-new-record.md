```ucm:hide
.lib> builtins.merge
```

```unison
unique type Foo = { bar : Nat }
```

This shouldn't be an error.

```ucm:error
.> update
.> view Foo
```
