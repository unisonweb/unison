# Record row corner cases

Record rows — `{ a: Nat | r }` for a named remainder — and the operations that
mention them: projections, `@.set`, and how width subtyping behaves when a row
shows up on one side, the other, or both.

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

pairWild : { field1: Nat, field2: Nat } -> (Nat, Nat)
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
  + pairOpen  : {field1: Nat, field2: Nat} -> (Nat, Nat)
  + pairWild  : {field1: Nat, field2: Nat} -> (Nat, Nat)

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

getX : { x: Nat } -> Nat
getX r = r@x

mk : { x: Nat | r } -> { x: Nat | r }
mk r = r

> getX (mk { x: 1, y: 2 })
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + getX  : {x: Nat} -> Nat
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

innerX : { inner: { x: Nat } } -> Nat
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
  + firstOf   : {x: Nat} -> Nat
  + innerX    : {inner: {x: Nat}} -> Nat

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

A closed record written in a signature is opened to a row the body can't grow:
it may stay unsolved (and generalize) or be closed by how the body uses it, but
reading a field the signature didn't promise is an error. A row introduced by a
`forall` is rigid the same way. The exception is a shape that also appears in
positive position: it is never opened, so it stays exact -- see `closedResult`
below.

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
demandExtraWild : { x: Nat } -> Nat
demandExtraWild v = v@y
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  This record has no field called y here:

      2 | demandExtraWild v = v@y


  It has type:
    {x: Nat | _rest4}
```

A closed record has no row to grow:

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
    {x: Nat | _rest4}
```

A shape that appears in both positive and negative position stays exact: no
row is ever opened for it, since extra fields would leak into a result the
signature doesn't mention them in. The definition below is just the identity
on exact records, and applying it to an exact record is fine:

``` unison
closedResult : { x: Nat } -> { x: Nat }
closedResult r = r

> closedResult { x: 1 }
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + closedResult : {x: Nat} -> {x: Nat}

  Run `update` to apply these changes to your codebase.

    4 | > closedResult { x: 1 }
          ⧩
          {x: 1}
```

But a record with extra fields is rejected, even though the parameter position
alone would accept it:

``` unison :error
closedResultFails : { x: Nat } -> { x: Nat }
closedResultFails r = r

> closedResultFails { x: 1, y: 2 }
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I didn't expect this record:

      4 | > closedResultFails { x: 1, y: 2 }


  to have the field
    y: Nat

  because it should have the type:
    
    {x: Nat}
    

  derived from here:

      1 | closedResultFails : { x: Nat } -> { x: Nat }
```

``` unison :error
closedTopLevel : { x: Nat }
closedTopLevel = { x: 1, extra: 2 }
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I didn't expect this record:

      2 | closedTopLevel = { x: 1, extra: 2 }


  to have the field
    extra: Nat

  because it should have the type:
    
    {x: Nat}
    

  derived from here:

      1 | closedTopLevel : { x: Nat }
```

## Closed records in a parameter position

A record type in a parameter position means "at least these fields": the caller
may pass a record with more fields. The
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
hofExact g = g { x: 1 }

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

A function passed in for a function-typed parameter only accepts exactly what
that parameter says: the inner `{ x: Nat }` is in positive position, so it is
never opened, and handing it a wider record is an error:

``` unison :error
hofWide : ({ x: Nat } -> Nat) -> Nat
hofWide g = g { x: 1, y: 2 }
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I didn't expect this record:

      2 | hofWide g = g { x: 1, y: 2 }


  to have the field
    y: Nat

  because it should have the type:
    
    {x: Nat}
    

  derived from here:

      1 | hofWide : ({ x: Nat } -> Nat) -> Nat
```

Records with the same shape in one signature share a single row, so a function
taking two `{ x: Nat }` arguments takes two records with the *same* extra
fields:

``` unison
both : { x: Nat } -> { x: Nat } -> Nat
both a b = a@x

> both { x: 1, y: 2 } { x: 3, y: 4 }
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + both : {x: Nat} -> {x: Nat} -> Nat

  Run `update` to apply these changes to your codebase.

    4 | > both { x: 1, y: 2 } { x: 3, y: 4 }
          ⧩
          1
```

``` unison :error
bothMismatch : { x: Nat } -> { x: Nat } -> Nat
bothMismatch a b = a@x

> bothMismatch { x: 1, y: 2 } { x: 3 }
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I expected this record:

      4 | > bothMismatch { x: 1, y: 2 } { x: 3 }


  to have the field
    y: Nat

  so that it would match the type:
    
    {x: Nat, y: Nat}
    

  from here:

      4 | > bothMismatch { x: 1, y: 2 } { x: 3 }
```

The variance of a data type is handled properly also. `Box` is covariant in `a`, so a
`Box {x: Nat}` in a parameter position accepts a `Box {x: Nat, y: Nat}` -- the
extra field flows through the type variable.

``` unison
type Box a = Box a

boxUse : Box {x: Nat} -> Nat
boxUse = cases
  Box r -> r@x

> boxUse (Box {x: 0, y: 1})
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type Box a

  + boxUse : Box {x: Nat} -> Nat

  Run `update` to apply these changes to your codebase.

    7 | > boxUse (Box {x: 0, y: 1})
          ⧩
          0
```

A constructor's field type is exact: the declared field type is what the field
holds, no more and no less.

``` unison :error
type Duo = Duo { a: Nat, b: Nat }

> Duo { a: 1, b: 2, c: 3 }
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I didn't expect this record:

      3 | > Duo { a: 1, b: 2, c: 3 }


  to have the field
    c: Nat

  because it should have the type:
    
    {a: Nat, b: Nat}
    

  derived from here:

      1 | type Duo = Duo { a: Nat, b: Nat }
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
    
    {a: Nat, b: Nat}
    

  from here:

      1 | type Duo2 = Duo2 { a: Nat, b: Nat }
```

## Rows dropped from an inferred type

A row variable that occurs only as a record's row in parameter positions says
nothing: a parameter's record type already means "at least these fields". So it is
dropped when the type is reported, which reads better, though it is kept in the
stored type so each use site still gets a fresh row to solve. A row that occurs
anywhere else is kept, because it ties its positions together.

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
