`alias.term` makes a new name for a term.

```ucm:hide
project/main> builtins.mergeio lib.builtins
```

```ucm
project/main> alias.term lib.builtins.bug foo
project/main> ls
```

It won't create a conflicted name, though.

```ucm:error
project/main> alias.term lib.builtins.todo foo
```

```ucm
project/main> ls
```

You can use `debug.alias.term.force` for that.

```ucm
project/main> debug.alias.term.force lib.builtins.todo foo
project/main> ls
```
