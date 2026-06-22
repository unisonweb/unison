# `let` and `let rec` inside quasiquotes

Adds three more constructs to `[| ... |]`:

* non-recursive `let` blocks — chained `meta.TermF.Let` nodes
* recursive `let` blocks — a `meta.ABT.Cycle` wrapping an `Abs` chain
  around `meta.TermF.LetRec`
* `if … then … else`, `&&`, `||`, and `handle … with …` — straight
  rewrites to `meta.TermF.If` / `meta.TermF.Handle`

`&&` / `||` desugar to `if c then y else False` / `if c then True else
y` because the `meta.TermF` data type only carries `If`, not separate
And/Or.

```ucm :hide
scratch/main> builtins.mergeio
```

## Non-recursive let

```unison
qLet : meta.Term meta.TermF
qLet = [| let
            x = 1
            y = 2
            x Nat.+ y |]

storeLet : '{IO} Either Text Link.Term
storeLet _ = Meta.store qLet
```

```ucm
scratch/main> add
scratch/main> view qLet
scratch/main> run storeLet
```

## Recursive let

`countdown` references itself in its own binding, so the deriver
emits a `Cycle` wrapping a single `Abs` over a `TermF.LetRec` with
the binding and the body inside.

```unison
qLetRec : meta.Term meta.TermF
qLetRec = [| let
               countdown n =
                 if n == 0 then 0
                 else n Nat.+ countdown (Nat.drop n 1)
               countdown 3 |]

storeLetRec : '{IO} Either Text Link.Term
storeLetRec _ = Meta.store qLetRec
```

```ucm
scratch/main> add
scratch/main> view qLetRec
scratch/main> run storeLetRec
```

Aliasing the stored term and `run`ning it through a wrapper confirms
the quoted code evaluates as expected — `countdown 3 = 3 + 2 + 1 + 0
= 6`:

```ucm
scratch/main> alias.term #g5nqq4oii7 countDown3
scratch/main> view countDown3
```

```unison
runIt : '{IO, Exception} Nat
runIt _ = countDown3
```

```ucm
scratch/main> add
scratch/main> run runIt
```

## `if`, `&&`, `||`, `handle`

```unison
qIf : meta.Term meta.TermF
qIf = [| n -> if n Nat.> 0 then "positive" else "zero" |]

qBool : meta.Term meta.TermF
qBool = [| x y -> x && (y || false) |]
```

```ucm
scratch/main> add
scratch/main> view qIf
scratch/main> view qBool
```

`qBool`'s round-trip shows the `&&` / `||` rewrite — the quoted
operators come back as the equivalent `if`-cascade, since
`meta.TermF` doesn't store `&&` / `||` as distinct constructors.
