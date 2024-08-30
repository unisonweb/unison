```ucm:hide
scratch/main> builtins.merge lib.builtin
```

```unison
unique type Foo = Bar Nat
```

```ucm
scratch/main> add
scratch/main> alias.term Foo.Bar Stray.BarAlias
```

```unison
unique type Foo = Bar Nat Nat
```

```ucm:error
scratch/main> update
```
