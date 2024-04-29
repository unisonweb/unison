```ucm
.> project.create-empty update-in
update-in/main> builtins.merge lib.builtin
```

```unison
foo.x = "five"
foo.y = x ++ "ty"
```

```ucm
update-in/main> add
```

```unison
x = "six"
```

```ucm
update-in/main> update-in foo
```

```unison
> y
```
