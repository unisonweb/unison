```ucm:hide
.> project.create-empty proj
proj/main> builtins.merge lib.builtin
```

```unison
lib.old.foo = 17
lib.new.foo = +18
thingy = lib.old.foo + 10
```

```ucm
proj/main> add
```

```ucm:error
proj/main> upgrade old new
```
