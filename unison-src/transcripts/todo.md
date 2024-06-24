# Conflicted names

The todo command shows conflicted names (not demonstrated here yet because it is not easy to create them for tests, yet).

# Direct dependencies without names

The `todo` command shows hashes of direct dependencies of local (outside `lib`) definitions that don't have names in
the current namespace.

```ucm:hide
project/main> builtins.mergeio lib.builtins
```

```unison
foo.bar = 15
baz = foo.bar + foo.bar
```

```ucm
project/main> add
project/main> delete.namespace.force foo
project/main> todo
```

```ucm:hide
project/main> delete.project project
```
