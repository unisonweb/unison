# Cross-file type alias resolution

Confirms that a `type alias` declared in one file can be referenced from a
later, separate scratch file.

```ucm :hide
> builtins.mergeio
```

First file: declare the alias and add it to the codebase.

```unison
type alias Endo a = a -> a
```

```ucm
> add
```

Second file: reference the alias from a fresh scratch file. Name
resolution turns `Endo` into the codebase alias's ref, and the typechecker
expands the body lazily during checking. The stored type signature for `g`
keeps `Endo Nat` intact.

```unison
g : Endo Nat
g x = x + 2
```

```ucm
> add
> view g
```

`g`'s stored type is `Endo Nat`, and `view g` renders it back with the
alias intact.
