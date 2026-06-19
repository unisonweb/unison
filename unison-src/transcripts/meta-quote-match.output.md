# Match expressions inside quasiquotes

Up to now `[| ... |]` quotes accepted lambdas, applications, literals,
and references — enough to write a lot of useful macros, but not the
control-flow construct that drives most non-trivial Unison code. This
transcript exercises match expressions inside quotes, including:

  - literal patterns, constructor patterns, wildcards
  - `As` patterns (`m@(...)`)
  - guards
  - sequence patterns (`[]`, `+:`, `:+`)
  - splicing into the scrutinee and into case bodies

Each example also exercises the printer round-trip, so the body of
each quoted term shows up as `[| ... |]` in `view` output instead of
the raw `meta.TermF.Match` constructor tree.

``` ucm :hide
scratch/main> builtins.mergeio
```

## Basic match — constructor patterns

``` unison
opt : meta.Term meta.TermF
opt = [| match Some 41 with
           None -> 0
           Some y -> y Nat.+ 1 |]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + opt : meta.Term TermF

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Done.

scratch/main> view opt

  opt : meta.Term TermF
  opt =
    use TermF App Lit
    use meta.Term Term
    [| match Some 41 with
      None   -> 0
      Some y -> y Nat.+ 1 |]
```

The quoted term typechecks and stores as a real `Link.Term`:

``` unison
runOpt : '{IO} Either Text Link.Term
runOpt _ = Meta.store opt
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + runOpt : '{IO} Either Text Link.Term

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Done.

scratch/main> run runOpt

  Right (termLink #panfc5kal7)
```

## Cases shorthand — literal patterns and wildcards

``` unison
classify : meta.Term meta.TermF
classify = [| n -> match n with
                     0 -> "zero"
                     1 -> "one"
                     _ -> "many" |]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + classify : meta.Term TermF

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Done.

scratch/main> view classify

  classify : meta.Term TermF
  classify =
    use TermF Lit
    use meta.Term Term
    [| cases
      0 -> "zero"
      1 -> "one"
      _ -> "many" |]
```

## Guards and `As` patterns

``` unison
withGuard : meta.Term meta.TermF
withGuard = [| n -> match n with
                      m | m Nat.> 10 -> "big"
                      _ -> "small" |]

reuseScrut : meta.Term meta.TermF
reuseScrut = [| x -> match x with
                       m@(Some _) -> m
                       _ -> None |]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + reuseScrut : meta.Term TermF
  + withGuard  : meta.Term TermF

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Done.

scratch/main> view withGuard

  withGuard : meta.Term TermF
  withGuard =
    use TermF App Lit
    use meta.Term Term
    [| cases
      m | m Nat.> 10 -> "big"
      _ -> "small" |]

scratch/main> view reuseScrut

  reuseScrut : meta.Term TermF
  reuseScrut =
    use meta.Term Term
    [| cases
      m@(Some _) -> m
      _          -> None |]
```

## Sequence patterns

``` unison
firstOrZero : meta.Term meta.TermF
firstOrZero = [| xs -> match xs with
                         []     -> 0
                         h +: _ -> h
                         _ :+ z -> z |]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + firstOrZero : meta.Term TermF

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Done.

scratch/main> view firstOrZero

  firstOrZero : meta.Term TermF
  firstOrZero =
    use meta.Term Term
    [| cases
      []     -> 0
      h +: _ -> h
      _ :+ z -> z |]
```

## Splicing into a match — `${ ... }`

The scrutinee and case bodies are expression positions, so `${ ... }`
slots in naturally. (Pattern positions don't have an expression
there to splice into.) A useful pattern: a `withDefault` macro that
takes a default value and an Optional-producing expression, and
expands to the match that unpacks it.

``` unison
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

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + storeDefault : '{IO} Either Text Link.Term
  + useDefault   : meta.Term TermF
  + withDefault  : meta.Term TermF
                   -> meta.Term TermF
                   -> meta.Term TermF

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Done.

scratch/main> run storeDefault

  Right (termLink #rqpca9c9tk)
```

Aliasing the stored term and viewing it shows the fully expanded
match — the `[| match ${optExpr} with ... |]` template, with the
splice slots filled in, has produced an ordinary Unison program:

``` ucm
scratch/main> alias.term #rqpca9c9tk derived

  Done.

scratch/main> view derived

  derived : Nat
  derived = match Some 42 with
    None   -> 999
    Some y -> y
```
