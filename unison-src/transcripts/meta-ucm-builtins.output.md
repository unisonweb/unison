# UCM-as-builtins — namespace mutation from inside `{IO}`

Five builtins let macros do everything UCM's `alias.term`/`alias.type`/
`delete.term`/`move.term`/`lookup` commands do, from inside an
ordinary `'{IO}` program. Plus `Meta.dependents` for analysis. No
round-trips through the UCM prompt; the macro is the whole story.

| Builtin           | Type                                       |
|-------------------|--------------------------------------------|
| `Meta.aliasTerm`  | `Link.Term -> Text -> {IO} ()`             |
| `Meta.aliasType`  | `Link.Type -> Text -> {IO} ()`             |
| `Meta.deleteTerm` | `Text -> {IO} ()`                          |
| `Meta.moveTerm`   | `Text -> Text -> {IO} ()`                  |
| `Meta.lookup`     | `Text -> {IO} Optional Link.Term`          |
| `Meta.dependents` | `Link.Term -> {IO} [Link.Term]`            |

Mutating calls queue actions during evaluation and are applied via
the standard branch-mutation machinery after the runtime returns,
so SQLite/LSP/check-and-set behave exactly as if the user had typed
the equivalent UCM command. Read-only calls (`lookup`,
`dependents`) hit the branch snapshot/codebase directly.

There is no `Meta.dependencies` because the existing
`Code.dependencies : Code -> [Link.Term]` already covers the
direction "what does this depend on" for any value you've staged.

``` ucm :hide
scratch/main> builtins.mergeio
```

## Setup — three terms with a dependency chain

``` unison
seed : Nat
seed = 42

doubled : Nat
doubled = seed Nat.+ seed

quadrupled : Nat
quadrupled = doubled Nat.+ doubled
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + doubled    : Nat
  + quadrupled : Nat
  + seed       : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

## `Meta.lookup` — resolve a name to a `Link.Term`

``` unison
findSeed : '{IO} Optional Link.Term
findSeed _ = Meta.lookup "seed"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + findSeed : '{IO} Optional Link.Term

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> run findSeed

  Some (termLink seed)
```

## `Meta.dependents` — who points at this?

`doubled` references `seed`; `quadrupled` references `doubled`.
`Meta.dependents (termLink seed)` should return `[termLink doubled]`.

``` unison
deps : '{IO} [Link.Term]
deps _ =
  match Meta.lookup "seed" with
    None -> []
    Some link -> Meta.dependents link
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + deps : '{IO} [Link.Term]

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> run deps

  [termLink doubled]
```

## `Meta.aliasTerm` — name a stored term in one step

The killer-demo workflow: build a `meta.Term`, store it, alias the
returned `Link.Term` to a name, all in one IO action.

``` unison
storeAt : Text -> meta.Term meta.TermF ->{IO} Either Text Link.Term
storeAt name tm =
  match Meta.store tm with
    Left e -> Left e
    Right link ->
      _ = Meta.aliasTerm link name
      Right link

makeGreeting : '{IO} Either Text Link.Term
makeGreeting _ = storeAt "greeting" [| "hello, world" |]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + makeGreeting : '{IO} Either Text Link.Term
  + storeAt      : Text
                   -> meta.Term TermF
                   ->{IO} Either Text Link.Term

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> run makeGreeting

  Right (termLink #su1f0sk79m)

scratch/main> view greeting

  greeting : Text
  greeting = "hello, world"
```

## `Meta.aliasType` — same idea for types

`Link.Type` references are produced by `typeLink`. Aliasing them
binds a new name to an existing type in the current namespace.

``` unison
unique type Pair a b = Pair a b
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type Pair a b

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
makePairAlias : '{IO} ()
makePairAlias _ = Meta.aliasType (typeLink Pair) "Couple"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + makePairAlias : '{IO} ()

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> run makePairAlias

  ()

scratch/main> view Couple

  type Couple a b = Pair a b
```

## `Meta.moveTerm` and `Meta.deleteTerm` — rename and remove

``` unison
shuffle : '{IO} ()
shuffle _ =
  _ = Meta.moveTerm "doubled" "twice"
  _ = Meta.deleteTerm "quadrupled"
  ()
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + shuffle : '{IO} ()

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> run shuffle

  ()

scratch/main> view twice

  twice : Nat
  twice =
    use Nat +
    seed + seed
```

After `shuffle`, `doubled` is gone (renamed to `twice`), and
`quadrupled` is gone (deleted). The codebase still contains the
hashes, of course — they aren't *removed*, just unbound from those
names.
