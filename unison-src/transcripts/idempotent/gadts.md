``` ucm :hide
> builtins.merge lib.builtin
```

# Generalized algebraic data types (GADTs)

A `type … where` declaration lets each constructor write out its own result
type, so the type can be indexed. This is the same shape as an `ability`
declaration, but for ordinary data.

``` unison
type Expr a where
  NatLit : Nat -> Expr Nat
  BoolLit : Boolean -> Expr Boolean
  Add : Expr Nat -> Expr Nat -> Expr Nat
  If : Expr Boolean -> Expr a -> Expr a -> Expr a
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type Expr a

  Run `update` to apply these changes to your codebase.
```

``` ucm :hide
> add
```

It round-trips through the codebase in `where` form (ordinary data types and
records still print with `=`):

``` ucm
> view Expr

  type Expr a where
    Add : Expr Nat -> Expr Nat -> Expr Nat
    BoolLit : Boolean -> Expr Boolean
    If : Expr Boolean -> Expr a -> Expr a -> Expr a
    NatLit : Nat -> Expr Nat
```

## Pattern matching refines the index

Matching a constructor refines the scrutinee's type index inside that branch:
in the `NatLit` branch the result type `a` is known to be `Nat`, in `BoolLit`
it is `Boolean`, and so on. This is what makes a type-preserving evaluator
type-check:

``` unison
eval : Expr a -> a
eval = cases
  NatLit n -> n
  BoolLit b -> b
  Add x y -> eval x + eval y
  If c t f -> if eval c then eval t else eval f
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + eval : Expr a -> a

  Run `update` to apply these changes to your codebase.
```

``` ucm :hide
> add
```

And it runs:

``` unison
example : Nat
example = eval (If (BoolLit true) (Add (NatLit 1) (NatLit 2)) (NatLit 0))
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + example : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

> display example

  3
```

## Indices can be structured

The index isn't limited to a single type variable: a constructor can build a
*compound* index, and matching it decomposes the structure (e.g. `a ~ (x, y)`).
Here `Pair` indexes a `Two` by a tuple, and the evaluator recovers each side:

``` unison
type Two a where
  One : Nat -> Two Nat
  Pair : Two x -> Two y -> Two (x, y)

evalTwo : Two a -> a
evalTwo = cases
  One n -> n
  Pair l r -> (evalTwo l, evalTwo r)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type Two a

  + evalTwo : Two a -> a

  Run `update` to apply these changes to your codebase.
```

``` ucm :hide
> add
```

And it runs:

``` unison
twoExample : (Nat, Nat)
twoExample = evalTwo (Pair (One 1) (One 2))
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + twoExample : (Nat, Nat)

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

> display twoExample

  (1, 2)
```

## Coverage is index-aware

When the scrutinee's index is known concretely, constructors that can't build
that index are impossible. Here the scrutinee is `Expr Nat`, so `BoolLit`
(which builds `Expr Boolean`) is excluded: these three cases are exhaustive
without it.

``` unison
isLit : Expr Nat -> Boolean
isLit = cases
  NatLit _ -> true
  Add _ _ -> false
  If _ _ _ -> false
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + isLit : Expr Nat -> Boolean

  Run `update` to apply these changes to your codebase.
```

``` ucm :hide
> add
```

``` ucm
> view isLit

  isLit : Expr Nat -> Boolean
  isLit = cases
    NatLit _ -> true
    Add _ _  -> false
    If _ _ _ -> false
```

## Refinement needs a principal scrutinee

Following Dunfield & Krishnaswami (2019), a GADT pattern match refines the type
index only when the scrutinee's type is *principal* — known, rather than guessed
from the shape of the patterns. A match that does **not** depend on the index
needs no annotation; its type is inferred even without a signature (here the
result is `Nat` whatever the index):

``` unison
sizeOf = cases
  NatLit _ -> 1
  BoolLit _ -> 1
  Add _ _ -> 1
  If _ _ _ -> 1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + sizeOf : Expr t -> Nat

  Run `update` to apply these changes to your codebase.
```

A match that **does** rely on the index, however, needs the scrutinee's type to
be determined first — without a signature the branches can't agree on a result
type, so refinement does not kick in and typechecking fails (the fix is to add a
signature, as `eval` above has):

``` unison :error
evalNoSig = cases
  NatLit n -> n
  BoolLit b -> b
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found a value  of type:  Boolean
  where I expected to find:  Nat

      3 |   BoolLit b -> b
```

## Impossible cases can be omitted

Coverage is derived from the same inconsistency (the DK indexed-types paper's
`⊥`): a case whose index equalities are contradictory can't occur, so it need
not be covered. Both arguments here share one index, so the mixed cases are
impossible — and `agree` is exhaustive without them:

``` unison
type Tagged a where
  IsNat : Tagged Nat
  IsBool : Tagged Boolean
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type Tagged a

  Run `update` to apply these changes to your codebase.
```

``` ucm :hide
> add
```

``` unison
agree : Tagged a -> Tagged a -> Boolean
agree = cases
  IsNat, IsNat -> true
  IsBool, IsBool -> true
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + agree : Tagged a -> Tagged a -> Boolean

  Run `update` to apply these changes to your codebase.
```

## Equality witnesses

The canonical GADT: a value of `Equ a b` proves `a` and `b` are the same type.
Matching the sole constructor `Refl` (whose declared result `Equ a a` forces both
indices equal) refines `a ~ b` for the branch — an equation between two scrutinee
*variables*, not just a variable and a concrete type. So a safe coercion type-checks:

``` unison
type Equ a b where
  Refl : Equ a a

coerce : Equ a b -> a -> b
coerce = cases
  Refl -> (x -> x)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type Equ a b

  + coerce : Equ a b -> a -> b

  Run `update` to apply these changes to your codebase.
```

The branch body is type-checked under the refinement, so an ill-typed body is
still rejected (here `a` is required, but `"text"` is `Text`):

``` unison :error
bad : Equ a b -> a -> b
bad = cases
  Refl -> (_ -> "text")
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.


    ❓
    
    I couldn't resolve any of these names:
    
        3 |   Refl -> (_ -> "text")
    
    
    Name   Type   Suggestions
                  
    Refl   term   No matches
```
