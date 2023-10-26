Alice tries to merge Bob's branch. Alice has two names for one constructor. This is a merge precondition violation.

```ucm
.> project.create-empty project
project/main> builtins.merge
```

```unison
unique type Foo = MkFoo Nat
```

```ucm
project/main> branch alice
project/alice> add
project/alice> alias.term Foo.MkFoo Foo.internal.MkFoo2
project/main> branch bob
```

```ucm:error
project/alice> merge2 bob
```
