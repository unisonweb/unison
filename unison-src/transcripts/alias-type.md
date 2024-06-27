`alias.type` makes a new name for a type.

```ucm:hide
project/main> builtins.mergeio lib.builtins
```

```ucm
project/main> alias.type lib.builtins.Nat Foo
project/main> ls
```

It won't create a conflicted name, though.

```ucm:error
project/main> alias.type lib.builtins.Int Foo
```

```ucm
project/main> ls
```

You can use `debug.alias.type.force` for that.

```ucm
project/main> debug.alias.type.force lib.builtins.Int Foo
project/main> ls
```

