```ucm:hide
scratch/main> builtins.merge lib.builtin
```

```unison
unique type Foo = Bar Nat
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
scratch/main> find.verbose
```
