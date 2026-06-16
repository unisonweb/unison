# Meta.store writes a typechecked meta.Term back to the codebase

`Meta.store : meta.Term meta.TermF -> {IO} Either Text Link.Term`
typechecks a meta term against the live codebase, hashes the result,
persists the @(Reference.Id, Term, Type)@ triple via the runtime's
write callback, and returns a `Link.Term` to the freshly-stored hash.

```ucm :hide
scratch/main> builtins.mergeio
```

Decompile a user-defined term, then store the result. Then load it
back via `Meta.load` to confirm the term is actually persisted at the
hash `Meta.store` reported.

```unison
helper : Nat -> Nat
helper n = n + 1
```

```ucm
scratch/main> add
```

```unison
roundTrip : '{IO} Either Text Boolean
roundTrip _ =
  metaTerm = Meta.decompile helper
  match Meta.store metaTerm with
    Left e -> Left e
    Right newLink ->
      -- If store wrote the term, load should find it back at the
      -- new Link.Term.
      Right (match Meta.load newLink with
        None -> false
        Some _ -> true)
```

```ucm
scratch/main> run roundTrip
```
