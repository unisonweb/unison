```ucm:hide
.> builtins.merge lib.builtin
```

```unison
unique type Foo = Bar Nat
unique type Baz = Qux Foo
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
.> view Baz
.> find.verbose
```
