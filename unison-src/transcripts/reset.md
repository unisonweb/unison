```ucm:hide
scratch/main> builtins.merge
```

# reset loose code
```unison
a = 5
```

```ucm
scratch/main> add
scratch/main> history
scratch/main> reset 2
scratch/main> history
```

```unison
foo.a = 5
```

```ucm
scratch/main> add
scratch/main> ls foo
scratch/main> history
scratch/main> reset 1 foo
scratch/main> ls foo.foo
```

# reset branch

```ucm
foo/main> history
```

```unison
a = 5
```

```ucm
foo/main> add
foo/main> branch topic
foo/main> history
```

```unison
a = 3
```

```ucm
foo/main> update
foo/main> reset /topic
foo/main> history
```

# ambiguous reset

## ambiguous target
```unison
main.a = 3
```

```ucm:error
foo/main> add
foo/main> history
foo/main> reset 2 main
```

## ambiguous hash

```unison
main.a = 3
```

```ucm:error
foo/main> switch /topic
foo/topic> add
foo/topic> reset main
```
