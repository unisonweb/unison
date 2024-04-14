```ucm:hide
.> builtins.merge lib.builtin
```

```unison
unique type Foo = Bar Nat
```

```ucm
.> add
```

```unison
unique type Foo = Bar Nat Nat
```

```ucm
.> update
.> view Foo
.> find.verbose
```
