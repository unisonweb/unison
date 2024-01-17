```ucm:hide
.> project.create-empty proj
proj/main> builtins.merge
proj/main> move.namespace builtin lib.builtin
```

```unison
lib.old.foo = 17
lib.new.foo = 18
thingy = lib.old.foo + 10
```

```ucm
proj/main> add
proj/main> upgrade old new
proj/main> ls lib
proj/main> view thingy
```
