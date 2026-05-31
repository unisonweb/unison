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

Second file: reference the alias from a fresh scratch file. The parser
should look up `Endo` in the namespace, fetch its body from the codebase,
and expand inline before hashing.

```unison
g : Endo Nat
g x = x + 2
```

```ucm
> add
> view g
```

Currently this fails: the alias is persisted but the parser's name
resolution doesn't yet look up alias refs from the codebase, so `Endo` in
the second file's signature reports as an unknown type.
