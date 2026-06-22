# Unison metaprogramming — a tour

A single read-through of what this branch adds. Each section is
small and self-contained. For depth on any one feature, see the
focused transcripts (`meta-quote.md`, `meta-quote-match.md`,
`meta-quote-let.md`, `meta-derive-functor-nested.md`, …).

```ucm :hide
scratch/main> builtins.mergeio
```

## 1. Quotes — `[| e |]`

A quoted expression is a value of type `meta.Term meta.TermF` — a
first-class Unison AST node.

```unison
greeting : meta.Term meta.TermF
greeting = [| "hello, world" |]
```

```ucm
scratch/main> add
scratch/main> view greeting
```

## 2. Splices — `${ e }`

A splice plugs a `meta.Term meta.TermF` value into the surrounding
quote. The classic macro: `withDefault` builds a `match` that
unpacks an `Optional` with a fallback.

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
useDefault = withDefault [| 0 |] [| Some 42 |]
```

```ucm
scratch/main> add
scratch/main> view useDefault
```

## 3. Storing into the codebase — `Meta.store`

A `meta.Term` is just data until you ask the codebase to typecheck
and hash it. `Meta.store` returns the resulting `Link.Term`.

```unison
storeIt : '{IO} Either Text Link.Term
storeIt _ = Meta.store useDefault
```

```ucm
scratch/main> add
scratch/main> run storeIt
```

Alias the stored term and view it — the macro is fully expanded
into ordinary Unison:

```ucm
scratch/main> alias.term #vbh2muhdpi derived
scratch/main> view derived
```

## 4. Decompile — runtime values back to AST

`Meta.decompile` is the inverse direction. It pulls the runtime
representation of any value back into a `meta.Term`.

```unison
asTerm : '{IO} meta.Term meta.TermF
asTerm _ = Meta.decompile (Some (Some 7))
```

```ucm
scratch/main> add
scratch/main> run asTerm
```

## 5. Recursion via meta-let

Quotes carry `let` and `let rec`, so generated code can include
local recursion — which is the key ingredient for the recursive
deriving demo in `meta-derive-functor-nested.md`.

```unison
countdownAst : meta.Term meta.TermF
countdownAst =
  [| let
       countdown n =
         if n == 0 then 0
         else n Nat.+ countdown (Nat.drop n 1)
       countdown 5 |]

storeCountdown : '{IO} Either Text Link.Term
storeCountdown _ = Meta.store countdownAst
```

```ucm
scratch/main> add
scratch/main> view countdownAst
scratch/main> run storeCountdown
```

## 6. End-to-end: derive Functor for a recursive type

Pulls together every piece — quotes, splices, `Meta.dataDeclShape`,
`Meta.linkRef`, `Meta.store`, `mark.given`.

```unison
unique type Tree a = Leaf | Node a (Tree a) (Tree a)
```

```ucm
scratch/main> add
```

See `meta-derive-functor-nested.md` for the full deriver — it's
around 80 lines of Unison and produces a Functor instance like:

```
myMap f = cases
  Leaf -> Leaf
  Node x0 x1 x2 -> Node (f x0) (myMap f x1) (myMap f x2)
```

…wrapped in `Functor` and marked `given` so the implicit resolver
uses it for `fmapWith Functor.tree`.
