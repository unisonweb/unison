```ucm
.> builtins.merge
```

Create an LCA in which `foo` and `bar` are aliases.

```unison
foo = 10
bar = 10
```

```ucm
.> project.create-empty project
project/main> add
```

Have Alice update `foo` to one thing and `bar` to another.

```ucm
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
```

Have Bob do nothing at all (irrelevant to this transcript).

```ucm
project/main> branch bob
```

Try merging Bob into Alice and observe that Alice's branch violates the merge precondition that aliases must all be
updated together.

```ucm:error
project/alice> merge2 bob
```

Try merging Alice into Bob and observe the same.

```ucm:error
project/bob> merge2 alice
```
