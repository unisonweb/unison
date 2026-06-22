# Quote-built typeclass instance, stored as a given

A complete macro pipeline:

1. Define a typeclass.
2. Write a *macro* — a Unison function that, given a method
   implementation, builds the meta-term of an instance using
   `[| ... |]` quasiquote and `${ Meta.decompile m }` to lift the
   method.
3. `Meta.store` the AST. It returns a `Link.Term` whose payload is
   the canonical codebase hash, ready for ucm to consume.
4. `alias.term` it under a name, then `mark.given` it.
5. The elaborator now resolves `Show Nat` constraints against it.

```ucm :hide
scratch/main> builtins.mergeio
```

## The class

A minimal `Show a` typeclass — a record of one method, `a -> Text`.

```unison
unique type Show a = Show (a -> Text)
```

```ucm
scratch/main> add
```

## The macro

`deriveShow` takes any `a -> Text` function and builds the AST of
the corresponding `Show a` instance. `${ Meta.decompile m }` lifts
the runtime method into a `meta.Term meta.TermF`; quasiquote handles
the constructor invocation.

```unison
deriveShow : (a -> Text) ->{IO} meta.Term meta.TermF
deriveShow m =
  methodTerm = Meta.decompile m
  [| Show.Show ${methodTerm} |]
```

```ucm
scratch/main> add
```

## Generate and store

`Meta.store` writes the term to SQLite under its content hash, then
returns a `Link.Term` carrying that codebase hash.

```unison
storeShowNat : '{IO} Either Text Link.Term
storeShowNat _ =
  instance = !'(deriveShow Nat.toText)
  Meta.store instance
```

```ucm
scratch/main> add
scratch/main> run storeShowNat
```

## Alias and mark as given

The hash printed above is now in the codebase. Bring it under the
name `Show.nat` and mark it as a given.

```ucm
scratch/main> alias.term #am6f54si49 Show.nat
scratch/main> view Show.nat
scratch/main> mark.given Show.nat
scratch/main> givens
```

## Use the instance

The just-aliased `Show.nat` is now an ordinary codebase term that
also carries the `Builtin.Given` metadata tag. Functions can refer
to it by name, and it shows up in `find :given` queries.

```unison
greet : '{IO} Text
greet _ = match Show.nat with
  Show.Show f -> "the answer is " ++ f 42
```

```ucm
scratch/main> add
scratch/main> run greet
scratch/main> find :given
```
