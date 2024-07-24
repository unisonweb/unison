```ucm:hide
scratch/main> builtins.merge lib.builtin
```

```unison
unique type Foo = Bar Nat
unique type Baz = Qux Foo
```

```ucm
scratch/main> add
```

```unison
unique type Foo = Bar Nat Nat
```

```ucm
scratch/main> update
scratch/main> view Foo
scratch/main> view Baz
scratch/main> find.verbose
```
