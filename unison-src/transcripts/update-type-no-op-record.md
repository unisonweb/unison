```ucm:hide
.> builtins.merge
.> move.namespace builtin lib.builtin
```

```unison
unique type Foo = { bar : Nat }
```

```ucm
.> add
```

Bug: this no-op update should (of course) succeed.

```ucm:error
.> update
```
