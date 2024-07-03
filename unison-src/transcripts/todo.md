# Nothing to do

When there's nothing to do, `todo` says this:

```ucm
scratch/main> todo
```

# Dependents of `todo`

The `todo` command shows local (outside `lib`) terms that directly call `todo`.

```ucm:hide
scratch/main> builtins.mergeio lib.builtins
```

```unison
foo : Nat
foo = todo "implement foo"

bar : Nat
bar = foo + foo
```

```ucm
scratch/main> add
scratch/main> todo
```

```ucm:hide
scratch/main> delete.project scratch
```

# Direct dependencies without names

The `todo` command shows hashes of direct dependencies of local (outside `lib`) definitions that don't have names in
the current namespace.

```ucm:hide
scratch/main> builtins.mergeio lib.builtins
```

```unison
foo.bar = 15
baz = foo.bar + foo.bar
```

```ucm
scratch/main> add
scratch/main> delete.namespace.force foo
scratch/main> todo
```

```ucm:hide
scratch/main> delete.project scratch
```

# Conflicted names

The `todo` command shows conflicted names.

```ucm:hide
scratch/main> builtins.mergeio lib.builtins
```

```unison
foo = 16
bar = 17
```

```ucm
scratch/main> add
scratch/main> debug.alias.term.force foo bar
scratch/main> todo
```

```ucm:hide
scratch/main> delete.project scratch
```

# Definitions in lib

The `todo` command complains about terms and types directly in `lib`.

```ucm:hide
scratch/main> builtins.mergeio lib.builtins
```

```unison
lib.foo = 16
```

```ucm
scratch/main> add
scratch/main> todo
```

```ucm:hide
scratch/main> delete.project scratch
```

# Constructor aliases

The `todo` command complains about constructor aliases.

```ucm:hide
scratch/main> builtins.mergeio lib.builtins
```

```unison
type Foo = One
```

```ucm
scratch/main> add
scratch/main> alias.term Foo.One Foo.Two
scratch/main> todo
```

```ucm:hide
scratch/main> delete.project scratch
```
