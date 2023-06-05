```ucm:hide
.> builtins.merge
```

# reset loose code
```unison
a = 5
```

```ucm
.> add
.> history
.> reset 2
.> history
```

```unison
foo.a = 5
```

```ucm
.> add
.> ls foo
.> history
.> reset 1 foo
.> ls foo.foo
```

# reset branch

```ucm
.> project.create foo
foo/main> history
```

```unison
a = 5
```

```ucm
foo/main> add
foo/main> ls
foo/main> history
```

```unison
a = 3
```

```ucm
foo/main> update
foo/main> history
foo/main> reset 2
foo/main> history
```

# ambiguous reset

```unison
main.a = 3
```

```ucm:error
foo/main> add
foo/main> history
foo/main> reset 2 main
```
