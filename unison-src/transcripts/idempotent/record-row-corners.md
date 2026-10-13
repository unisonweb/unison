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
fields to the same argument. A row that ends up used only once is then closed
again where the type is reported -- see "Rows dropped from an inferred type" below
\-- so the signature reads `{ field1: Nat, field2: Nat } -> Nat` rather than
`{ field1: Nat, field2: Nat | r } -> Nat`.

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

  + addFields : {field1: Nat, field2: Nat} -> Nat
  + bump      : {age: Nat | x} -> {age: Nat | x}
  + deep      : {inner: {x: t}} -> t
  + getName   : {name: t} -> t
  + swap      : {x: t, y: t1} -> {a: t1, b: t}
  + three     : {a: t2, b: t1, c: t} -> (t2, t1, t)
  + two       : {a: t1, b: t} -> (t1, t)

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

## Closed records in a parameter position

A record type in a parameter position means "at least these fields": the caller
may pass a record with more, so a closed row there reads as if it were `...`. The
definition body still only knows what was written — reading a field the parameter
didn't mention is still an error — which is the point: callers may add fields,
bodies may not depend on them.

``` unison
getX' : { x: Nat } -> Nat
getX' r = r@x

> getX' { x: 1, y: true }

pairExact : { field1: Nat, field2: Nat } -> (Nat, Nat)
pairExact r = (r@field1, r@field2)

> pairExact {field1: 1, field2: 2, field3: 3}

nestedExact : { inner: { x: Nat } } -> Nat
nestedExact r = r@inner@x

> nestedExact { inner: { x: 1, y: 2 }, extra: true }

hofExact : ({ x: Nat } -> Nat) -> Nat
hofExact g = g { x: 1, y: 2 }

> hofExact (r -> r@x)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + getX'       : {x: Nat} -> Nat
  + hofExact    : ({x: Nat} ->{g} Nat) ->{g} Nat
  + nestedExact : {inner: {x: Nat}} -> Nat
  + pairExact   : {field1: Nat, field2: Nat} -> (Nat, Nat)

  Run `update` to apply these changes to your codebase.

    4 | > getX' { x: 1, y: true }
          ⧩
          1

    9 | > pairExact {field1: 1, field2: 2, field3: 3}
          ⧩
          (1, 2)

    14 | > nestedExact { inner: { x: 1, y: 2 }, extra: true }
           ⧩
           1

    19 | > hofExact (r -> r@x)
           ⧩
           1
```

The variance of a data type is handled properly also. `Box` is covariant in `a`, so a
`Box {x : Nat} -> r` function will accept a `Box {x: Nat, y: Nat}`.

But `Fn` is contravariant in `a`, so `Fn (@x)` will check against `Fn {x : Nat, y: Nat}`.

``` unison
type Box a = Box a
type Fn a = Fn (a -> Nat)

boxCheck : Box {x: Nat} -> Box {x: Nat}
boxCheck b1 = b1

> boxCheck (Box {x: 0, y: 1})

fnCheck : Fn {x : Nat, y: Nat} -> Nat 
fnCheck fn1 = 42

> fnCheck (Fn (@x))
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type Box a
  + type Fn a

  + boxCheck : Box {x: Nat} -> Box {x: Nat}
  + fnCheck  : Fn {x: Nat, y: Nat} -> Nat

  Run `update` to apply these changes to your codebase.

    7 | > boxCheck (Box {x: 0, y: 1})
          ⧩
          Box {x: 0, y: 1}

    12 | > fnCheck (Fn (@x))
           ⧩
           42
```

A constructor's field is a parameter position as well, so the same reading
applies to it: the declared field type is what the field *at least* holds, extra
fields are kept, and a field the type requires is still required.

``` unison
type Duo = Duo { a: Nat, b: Nat }

> Duo { a: 1, b: 2, c: 3 }
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type Duo

  Run `update` to apply these changes to your codebase.

    3 | > Duo { a: 1, b: 2, c: 3 }
          ⧩
          Duo {a: 1, b: 2, c: 3}
```

``` unison :error
type Duo2 = Duo2 { a: Nat, b: Nat }

badDuo = Duo2 { a: 1 }
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I expected this record:

      3 | badDuo = Duo2 { a: 1 }


  to have the field
    b: Nat

  so that it would match the type:
    
    {a: Nat, b: Nat | ...}
    

  from here:

      1 | type Duo2 = Duo2 { a: Nat, b: Nat }
```

## Rows dropped from an inferred type

A row variable that occurs once, as a record's row in a parameter position, says
nothing: a parameter's record type already means "at least these fields". So it is
closed when the type is reported, which reads better. A row that occurs twice is
kept, because it ties its two positions together.

``` unison
dropRow r = r@field1 Nat.+ r@field2

keepRow r = r@age.set (r@age + 1)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + dropRow : {field1: Nat, field2: Nat} -> Nat
  + keepRow : {age: Nat | x} -> {age: Nat | x}

  Run `update` to apply these changes to your codebase.
```
