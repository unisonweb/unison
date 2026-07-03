# Scenario: a namespace merge preserves `given` and `class` marks

Given-ness and class-ness are namespace metadata (the `##Builtin.Given`
sentinel on a term and the `##Builtin.Class` sentinel on a type, in
their `MdValues`). A three-way merge rebuilds the merged namespace from
unconflicted definitions, which carry no metadata — so without explicit
handling every `given` / `class` mark would be silently dropped by a
merge. This transcript confirms that marks on definitions that are
unconflicted across the merge survive into the merged branch.

``` ucm :hide
scratch/main> builtins.merge
```

On `main`, declare a class and a `given` (auto-marked by the `given`
keyword), then confirm it is listed by `givens`.

``` unison :hide
class Foo a = { foo : a -> Nat }

given Foo.nat : Foo Nat = Foo (n -> n)
```

``` ucm
scratch/main> add

  Done.

scratch/main> givens

  Definitions marked as givens in the current namespace:

    Foo.nat
```

Branch `alice` and `bob` from `main` (both inherit `Foo.nat` and its
mark), then have each add an unrelated definition so the branches
diverge and a real three-way merge (not a fast-forward) is required.

``` ucm :hide
scratch/main> branch alice
```

``` unison :hide
a : Nat
a = 1
```

``` ucm :hide
scratch/alice> add

scratch/main> branch bob
```

``` unison :hide
b : Nat
b = 2
```

``` ucm :hide
scratch/bob> add
```

Merge `bob` into `alice`. `Foo.nat` is unconflicted (identical on both
sides), so it flows through the unconflicted-defn rebuild — and its
`given` mark must be re-stamped onto the merged namespace.

``` ucm :hide
scratch/alice> merge /bob
```

`givens` on the merged branch still lists `Foo.nat`, `view Foo.nat`
still renders it with the `given` keyword, and `view Foo` still renders
the class with the `class` keyword and record-field syntax — so both
the `given` and the `class` mark survived the merge.

``` ucm
scratch/alice> givens

  Definitions marked as givens in the current namespace:

    Foo.nat

scratch/alice> view Foo.nat

  given Foo.nat : Foo Nat = Foo (n -> n)

scratch/alice> view Foo

  class Foo a = { foo : a -> Nat }
```
