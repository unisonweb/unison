```ucm:hide
.> project.create-empty myproject
myproject/main> builtins.merge
myproject/main> move.namespace builtin lib.builtin
```

```unison
lib.old.foo = 141
lib.new.foo = 142
bar = 141
mything = lib.old.foo + 100
```

```ucm
myproject/main> update
myproject/main> upgrade old new
myproject/main> view mything
myproject/main> view bar
```
