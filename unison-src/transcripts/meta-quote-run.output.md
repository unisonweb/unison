# Run a quote directly, no codebase write

`Meta.store` persists a meta.Term back to the codebase, which is great
when you want a permanent name for the rewritten code — but it isn't
the only way to evaluate a quote. `Meta.typecheck` already does the
two things `Meta.eval` needs: it typechecks the meta.Term against the
codebase **and** runs `prepareEvaluation`, returning a `Link.Term`
that's already registered in the runtime cache. Piping that link
straight into `Meta.eval` evaluates the quote without any codebase
write.

``` ucm :hide
scratch/main> builtins.mergeio
```

A reusable wrapper: typecheck-then-eval, returning `Either` to surface
typecheck failures.

``` unison
Meta.run : meta.Term meta.TermF -> {IO} Either Text a
Meta.run mt = match Meta.typecheck mt with
  Left e -> Left e
  Right (_ty, link) -> Right (Meta.eval link)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + Meta.run : meta.Term TermF ->{IO} Either Text a

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

Re-run the staged-power example without `Meta.store`. Builds the
quoted AST of `\y -> y^7`, runs it on `2`, gets `128`.

``` unison
power : Nat -> meta.Term meta.TermF -> meta.Term meta.TermF
power n x =
  if n == 0 then
    [| 1 |]
  else if (Nat.mod n 2) == 0 then
    let
      half = power ((Nat./) n 2) x
      [| ${half} Nat.* ${half} |]
  else
    [| ${x} Nat.* ${power (Nat.drop n 1) x} |]

power7 : meta.Term meta.TermF
power7 = [| y -> ${power 7 [| y |]} |]

runDirect : '{IO} Either Text Nat
runDirect _ = match Meta.run power7 with
  Left e -> Left e
  Right f ->
    g : Nat -> Nat
    g = f
    Right (g 2)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + power     : Nat -> meta.Term TermF -> meta.Term TermF
  + power7    : meta.Term TermF
  + runDirect : '{IO} Either Text Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> run runDirect

  Right 128
```

That's a complete build-and-evaluate cycle with no `Meta.store` call
and no new hash committed to the codebase. The quote lives only as a
runtime value, useful when the result is throwaway (a JIT-style code
generator, a one-shot rewrite check, a REPL evaluator).

The codebase comparison:

| Step | Effect |
|------|--------|
| `Meta.typecheck mt` | typechecks against the codebase; **registers** the compiled term in the runtime's combinator cache; returns `(meta.Term TypeF, Link.Term)` |
| `Meta.store mt` | same as above **plus** computes the canonical hash and writes `(Reference.Id, Term, Type)` to the SQLite codebase |
| `Meta.eval link` | invokes the cached combinator; works on any `Link.Term` whose target is in the cache, no matter how it got there |

So `Meta.run = typecheck + eval` reuses what's already there.
