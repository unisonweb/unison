Alice tries to merge Bob's branch. In their LCA, `foo` and `bar` are aliases, but in Alice's branch, they are not. This
is a merge precondition violation.

```unison
foo = 10
bar = 10
```

```ucm
.> project.create-empty project
project/main> builtins.merge
project/main> add
project/main> branch alice
project/alice> delete.term foo
project/alice> delete.term bar
```

```unison
foo = 11
bar = 12
```

```ucm
project/alice> add
project/main> branch bob
```

```ucm:error
project/alice> merge2 bob
```

```ucm:error
project/bob> merge2 alice
```
