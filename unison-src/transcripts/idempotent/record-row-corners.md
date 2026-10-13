# Record row corner cases

Record rows — `{ a: Nat | r }` for a named remainder, `{ a: Nat | ... }` for an
anonymous one — and the operations that mention them: projections, `@.set`, and
how subtyping behaves when a row shows up on one side, the other, or both.

``` ucm
scratch/main> builtins.merge lib.builtins

  Done.
```

## Projections that infer their argument's row

A projection on an argument whose type isn't known yet learns that field and
leaves the *rest* of the row as a variable, so later projections can add more
fields to the same argument. The inferred types say `| r` rather than `| ...`
for exactly that reason.

``` unison
getName r = r@name

> getName {name: "Bob", age: 99}

two r = (r@a, r@b)

> two {a: 1, b: "hi"}

three r = (r@a, r@b, r@c)

> three {a: 1, b: "hi", c: true}

addFields r = r@field1 Nat.+ r@field2

> addFields {field1: 1, field2: 2}

swap r = {a: r@y, b: r@x}

> swap {x: 1, y: "hi"}

deep r = r@inner@x

> deep {inner: {x: 1, y: 2}, other: true}

bump r = r@age.set (r@age + 1)

> bump {age: 30, name: "Bob"}
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + addFields : {field1: Nat, field2: Nat | x} -> Nat
  + bump      : {age: Nat | x} -> {age: Nat | x}
  + deep      : {inner: {x: t | x} | x1} -> t
  + getName   : {name: t | x} -> t
  + swap      : {x: t, y: t1 | x} -> {a: t1, b: t}
  + three     : {a: t2, b: t1, c: t | x} -> (t2, t1, t)
  + two       : {a: t1, b: t | x} -> (t1, t)

  Run `update` to apply these changes to your codebase.

    3 | > getName {name: "Bob", age: 99}
          ⧩
          "Bob"

    7 | > two {a: 1, b: "hi"}
          ⧩
          (1, "hi")

    11 | > three {a: 1, b: "hi", c: true}
           ⧩
           (1, "hi", true)

    15 | > addFields {field1: 1, field2: 2}
           ⧩
           3

    19 | > swap {x: 1, y: "hi"}
           ⧩
           {a: "hi", b: 1}

    23 | > deep {inner: {x: 1, y: 2}, other: true}
           ⧩
           1

    27 | > bump {age: 30, name: "Bob"}
           ⧩
           {age: 31, name: "Bob"}
```

## Declared rows

The same functions with the row written out. All three spellings — named,
anonymous, and closed — accept a record with exactly the fields mentioned, and
the two open ones also accept extra fields.

``` unison
pairOpen : { field1: Nat, field2: Nat | r } -> (Nat, Nat)
pairOpen r = (r@field1, r@field2)

> pairOpen {field1: 1, field2: 2}
> pairOpen {field1: 1, field2: 2, field3: 3}

pairWild : { field1: Nat, field2: Nat | ... } -> (Nat, Nat)
pairWild r = (r@field1, r@field2)

> pairWild {field1: 1, field2: 2}
> pairWild {field1: 1, field2: 2, field3: 3}

pairExact : { field1: Nat, field2: Nat } -> (Nat, Nat)
pairExact r = (r@field1, r@field2)

> pairExact {field1: 1, field2: 2}
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + pairExact : {field1: Nat, field2: Nat} -> (Nat, Nat)
  + pairOpen  : {field1: Nat, field2: Nat | r} -> (Nat, Nat)
  + pairWild  : {field1: Nat, field2: Nat | ...} -> (Nat, Nat)

  Run `update` to apply these changes to your codebase.

    4 | > pairOpen {field1: 1, field2: 2}
          ⧩
          (1, 2)

    5 | > pairOpen {field1: 1, field2: 2, field3: 3}
          ⧩
          (1, 2)

    10 | > pairWild {field1: 1, field2: 2}
           ⧩
           (1, 2)

    11 | > pairWild {field1: 1, field2: 2, field3: 3}
           ⧩
           (1, 2)

    16 | > pairExact {field1: 1, field2: 2}
           ⧩
           (1, 2)
```

## Subtyping with rows on one side or both

`rowId` passes its row straight through, so both the argument and the result
keep whatever extra fields the caller had. `getX` accepts a wildcard row, and
`mk` produces a named one: feeding one to the other checks the row on both
sides at once.

``` unison
rowId : { x: Nat | r } -> { x: Nat | r }
rowId r = r

> rowId { x: 1, y: "hi" }
> rowId { x: 1 }

getX : { x: Nat | ... } -> Nat
getX r = r@x

mk : { x: Nat | r } -> { x: Nat | r }
mk r = r

> getX (mk { x: 1, y: 2 })
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + getX  : {x: Nat | ...} -> Nat
  + mk    : {x: Nat | r} -> {x: Nat | r}
  + rowId : {x: Nat | r} -> {x: Nat | r}

  Run `update` to apply these changes to your codebase.

    4 | > rowId { x: 1, y: "hi" }
          ⧩
          {x: 1, y: "hi"}

    5 | > rowId { x: 1 }
          ⧩
          {x: 1}

    13 | > getX (mk { x: 1, y: 2 })
           ⧩
           1
```

The row has to survive containers and pattern matching too, and a wildcard row
is still allowed on a record nested inside an argument:

``` unison
dup : { x: Nat | r } -> [{ x: Nat | r }]
dup r = [r, r]

> dup { x: 1, y: 2 }

firstOf : { x: Nat | r } -> Nat
firstOf = cases
  { x: n } -> n

> firstOf { x: 1, y: 2 }

innerX : { inner: { x: Nat | ... } } -> Nat
innerX r = r@inner@x

> innerX { inner: { x: 1, y: 2 } }

bumpNamed : { age: Nat | r } -> { age: Nat | r }
bumpNamed r = r@age.set 31

> bumpNamed {age: 30, name: "Bob"}
> bumpNamed {age: 30}
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + bumpNamed : {age: Nat | r} -> {age: Nat | r}
  + dup       : {x: Nat | r} -> [{x: Nat | r}]
  + firstOf   : {x: Nat | r} -> Nat
  + innerX    : {inner: {x: Nat | ...}} -> Nat

  Run `update` to apply these changes to your codebase.

    4 | > dup { x: 1, y: 2 }
          ⧩
          [{x: 1, y: 2}, {x: 1, y: 2}]

    10 | > firstOf { x: 1, y: 2 }
           ⧩
           1

    15 | > innerX { inner: { x: 1, y: 2 } }
           ⧩
           1

    20 | > bumpNamed {age: 30, name: "Bob"}
           ⧩
           {age: 31, name: "Bob"}

    21 | > bumpNamed {age: 30}
           ⧩
           {age: 31}
```

## Rows that can't grow

An anonymous row can be *required* of a caller but never extended, and a row
introduced by a `forall` is rigid, so a definition body can't demand fields its
signature didn't promise. Both are errors even though the argument's row is
still a variable at the point of the projection.

``` unison :error
demandExtra : { x: Nat | r } -> Nat
demandExtra v = v@y
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  This record has no field called y here:

      2 | demandExtra v = v@y


  It has type:
    {x: Nat | r5}
```

``` unison :error
demandExtraWild : { x: Nat | ... } -> Nat
demandExtraWild v = v@y
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  This record has no field called y here:

      2 | demandExtraWild v = v@y


  It has type:
    {x: Nat | ...}
```

A closed record has no row to grow, and `...` isn't allowed in a result at all.

``` unison :error
noSuchField : { x: Nat } -> Nat
noSuchField = cases
  { x: n, nope: m } -> n
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  This record has no field called nope here:

      3 |   { x: n, nope: m } -> n


  It has type:
    {x: Nat}
```

``` unison :error
badRow : { x: Nat | ... } -> { x: Nat | ... }
badRow r = r
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I can't use `...` here.

  `...` is an anonymous record row, which says "accept whatever
  other fields the caller has". That only makes sense for an
  argument: in a result it would let the caller choose the extra
  fields, so the row has to be named instead, e.g. `{ x: Nat | r
  }`.

      1 | badRow : { x: Nat | ... } -> { x: Nat | ... }

  type:
    {x: Nat | ...} ->{𝕖3} {x: Nat | ...}
```

``` unison :error
topLevelWild : { x: Nat | ... }
topLevelWild = { x: 1 }
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I can't use `...` here.

  `...` is an anonymous record row, which says "accept whatever
  other fields the caller has". That only makes sense for an
  argument: in a result it would let the caller choose the extra
  fields, so the row has to be named instead, e.g. `{ x: Nat | r
  }`.

      1 | topLevelWild : { x: Nat | ... }

  type:
    {x: Nat | ...}
```
