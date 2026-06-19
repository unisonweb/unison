# Match expressions inside quasiquotes

Up to now `[| ... |]` quotes accepted lambdas, applications, literals,
and references — enough to write a lot of useful macros, but not the
control-flow construct that drives most non-trivial Unison code. This
transcript exercises match expressions inside quotes, including:

* literal patterns, constructor patterns, wildcards
* `As` patterns (`m@(...)`)
* guards
* sequence patterns (`[]`, `+:`, `:+`)
* splicing into the scrutinee and into case bodies

Each example also exercises the printer round-trip, so the body of
each quoted term shows up as `[| ... |]` in `view` output instead of
the raw `meta.TermF.Match` constructor tree.

```ucm :hide
scratch/main> builtins.mergeio
```

## Basic match — constructor patterns

```unison
opt : meta.Term meta.TermF
opt = [| match Some 41 with
           None -> 0
           Some y -> y Nat.+ 1 |]
```

```ucm
scratch/main> add
scratch/main> view opt
```

The quoted term typechecks and stores as a real `Link.Term`:

```unison
runOpt : '{IO} Either Text Link.Term
runOpt _ = Meta.store opt
```

```ucm
scratch/main> add
scratch/main> run runOpt
```

## Cases shorthand — literal patterns and wildcards

```unison
classify : meta.Term meta.TermF
classify = [| n -> match n with
                     0 -> "zero"
                     1 -> "one"
                     _ -> "many" |]
```

```ucm
scratch/main> add
scratch/main> view classify
```

## Guards and `As` patterns

```unison
withGuard : meta.Term meta.TermF
withGuard = [| n -> match n with
                      m | m Nat.> 10 -> "big"
                      _ -> "small" |]

reuseScrut : meta.Term meta.TermF
reuseScrut = [| x -> match x with
                       m@(Some _) -> m
                       _ -> None |]
```

```ucm
scratch/main> add
scratch/main> view withGuard
scratch/main> view reuseScrut
```

## Sequence patterns

```unison
firstOrZero : meta.Term meta.TermF
firstOrZero = [| xs -> match xs with
                         []     -> 0
                         h +: _ -> h
                         _ :+ z -> z |]
```

```ucm
scratch/main> add
scratch/main> view firstOrZero
```

## Splicing into a match — `${ ... }`

The scrutinee and case bodies are expression positions, so `${ ... }`
slots in naturally. (Pattern positions don't have an expression
there to splice into.) A useful pattern: a `withDefault` macro that
takes a default value and an Optional-producing expression, and
expands to the match that unpacks it.

```unison
withDefault :
  meta.Term meta.TermF
  -> meta.Term meta.TermF
  -> meta.Term meta.TermF
withDefault defVal optExpr =
  [| match ${optExpr} with
       None -> ${defVal}
       Some y -> y |]

useDefault : meta.Term meta.TermF
useDefault = withDefault [| 999 |] [| Some 42 |]

storeDefault : '{IO} Either Text Link.Term
storeDefault _ = Meta.store useDefault
```

```ucm
scratch/main> add
scratch/main> run storeDefault
```

Aliasing the stored term and viewing it shows the fully expanded
match — the `[| match ${optExpr} with ... |]` template, with the
splice slots filled in, has produced an ordinary Unison program:

```ucm
scratch/main> alias.term #rqpca9c9tk derived
scratch/main> view derived
```
