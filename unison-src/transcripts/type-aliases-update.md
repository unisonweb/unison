# Updating a type alias propagates to dependents

When a `type alias` body changes, every term that mentions the alias is
detected as a dependent and re-typechecked against the new alias — same
flow as a decl update.

```ucm :hide
> builtins.mergeio
```

## Compatible body change: dependents re-hashed cleanly

Define an alias and a term that uses it.

```unison
type alias Boxed a = Optional a

f : Boxed Nat -> Nat
f b = match b with
  Some n -> n
  None -> 0
```

```ucm
> add
```

Now change `Boxed`'s body to a different but compatible alias. `f`
typechecks against the new alias, gets a fresh hash, and its stored type
references the new `Boxed`.

```unison
type alias Boxed a = Optional a
```

```ucm
> update
> view f
```

## Incompatible body change: update is rejected

If the new alias body breaks dependents, the standard UCM update flow
catches it and creates a fix-up branch — exactly as it would for a decl.

```unison
type alias Endo a = a -> a

g : Endo Nat
g n = n + 1
```

```ucm
> add
```

Redefining `Endo` to take a second argument means `g`'s body no longer
fits:

```unison
type alias Endo a = a -> a -> a
```

```ucm :error
> update
```
