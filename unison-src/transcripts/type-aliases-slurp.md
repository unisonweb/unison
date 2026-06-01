# Aliases in slurp output

Confirms that the "Loading changes detected" output that UCM shows after
typechecking a file lists `type alias` declarations alongside data decls,
effects, and terms.

```ucm :hide
> builtins.mergeio
```

```unison
type alias Endo a = a -> a

f : Endo Nat
f x = x + 1
```

The alias appears as `+ type alias Endo a = a -> a` alongside the term.

```ucm
> add
```
