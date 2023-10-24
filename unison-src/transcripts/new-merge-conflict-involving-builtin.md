```ucm
.> builtins.merge
```

```ucm
.> project.create-empty project
project/main> builtins.merge
```

Make an add-add conflict involving a builtin and observe that violates a merge precondition.

```unison
structural type Foo = MkFoo Nat Nat Nat
```

```ucm
project/main> branch alice
project/alice> alias.type builtin.Nat Foo
project/main> branch bob
project/bob> add
```

```ucm:error
project/alice> merge2 bob
```

```ucm:error
project/bob> merge2 alice
```
