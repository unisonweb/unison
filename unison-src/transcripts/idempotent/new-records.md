# Structural records

``` ucm
scratch/main> builtins.merge lib.builtins

  Done.
```

We should be able to write simple functions which construct record types, and can evaluate them.

``` unison
mkRec : a -> b -> c -> { x: a, y: b, z: c }
mkRec a b c = { x: a, y: b, z: c }
> mkRec 1 2 3

addUpRec : { x: Nat, y: Nat, z: Nat | ... } -> Nat
addUpRec = cases
  { x:x, y:y, z:z } -> x Nat.+ y Nat.+ z

> addUpRec (mkRec 1 2 3)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + addUpRec : {x: Nat, y: Nat, z: Nat | ...} -> Nat
  + mkRec    : a -> b -> c -> {x: a, y: b, z: c}

  Run `update` to apply these changes to your codebase.

    3 | > mkRec 1 2 3
          ⧩
          {x: 1, y: 2, z: 3}

    9 | > addUpRec (mkRec 1 2 3)
          ⧩
          6
```

And can add them to the codebase;

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> ls

  1. addUpRec ({x: Nat, y: Nat, z: Nat | ...} -> Nat)
  2. lib.     (747 terms, 116 types)
  3. mkRec    (a -> b -> c -> {x: a, y: b, z: c})
```

We should be able to create wrapper types which encapsulate records, and manipulate them.

``` unison
type Point = Point { x: Nat, y: Nat }

mkPoint : Nat -> Nat -> Point
mkPoint x y = Point { x: x, y: y }

unpackPoint : Point -> (Nat, Nat)
unpackPoint = cases
  Point { x:x, y:y } -> (x, y)

-- We can do partial record projections and only bind the fields we care about.
getX : Point -> Nat
getX = cases
    Point { x:x } -> x

getY : Point -> Nat
getY = cases
    Point { y:y } -> y

p : Point
p = mkPoint 3 4

> unpackPoint p
> getX p
> getY p
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type Point

  + getX        : Point -> Nat
  + getY        : Point -> Nat
  + mkPoint     : Nat -> Nat -> Point
  + p           : Point
  + unpackPoint : Point -> (Nat, Nat)

  Run `update` to apply these changes to your codebase.

    22 | > unpackPoint p
           ⧩
           (3, 4)

    23 | > getX p
           ⧩
           3

    24 | > getY p
           ⧩
           4
```

We should be able to add them to the codebase;

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> ls

  1.  Point       (type)
  2.  Point.      (1 term)
  3.  addUpRec    ({x: Nat, y: Nat, z: Nat | ...} -> Nat)
  4.  getX        (Point -> Nat)
  5.  getY        (Point -> Nat)
  6.  lib.        (747 terms, 116 types)
  7.  mkPoint     (Nat -> Nat -> Point)
  8.  mkRec       (a -> b -> c -> {x: a, y: b, z: c})
  9.  p           (Point)
  10. unpackPoint (Point -> (Nat, Nat))

scratch/main> view Point

  type Point = Point {x: Nat, y: Nat}
```

We should get a nice error if we are missing a field from an expected type.

``` unison :error
getName = cases
  { name:name, age:_ } -> name

-- Missing the 'name' field
> getName { age: 30 }
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I expected this record:

      5 | > getName { age: 30 }


  to have the field
    name: 𝕩16

  so that it would match the type:
    
    {age: 𝕩15, name: 𝕩16 | ...}
    

  from here:

      2 |   { name:name, age:_ } -> name
```

We should get a nice error if we have additional unexpected fields.

``` unison :error
type Person = Person { name: Text, age: Nat }

-- 'address' is an unexpected field
createPerson = Person { name: "Alice", age: 30, address: "123 Main St" }
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I didn't expect this record:

      4 | createPerson = Person { name: "Alice", age: 30, address: "123 Main St" }


  to have the field
    address: Text

  because it should have the type:
    
    {age: Nat, name: Text}
    

  derived from here:

      1 | type Person = Person { name: Text, age: Nat }
```

Record field projections should infer the most general record type:

``` unison
getAddress = cases
    { address: address } -> address
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + getAddress : {address: t | ...} -> t

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> ls

  1.  Point       (type)
  2.  Point.      (1 term)
  3.  addUpRec    ({x: Nat, y: Nat, z: Nat | ...} -> Nat)
  4.  getAddress  ({address: t | ...} -> t)
  5.  getX        (Point -> Nat)
  6.  getY        (Point -> Nat)
  7.  lib.        (747 terms, 116 types)
  8.  mkPoint     (Nat -> Nat -> Point)
  9.  mkRec       (a -> b -> c -> {x: a, y: b, z: c})
  10. p           (Point)
  11. unpackPoint (Point -> (Nat, Nat))
```

### Nested records

Records nest, both as literals and as patterns, to arbitrary depth.

``` unison
nested : { a: { b: Nat } } -> Nat
nested = cases
  { a: { b: b } } -> b

> nested { a: { b: 7 } }

-- The same thing with no annotation, so every field type is inferred.
nestedInferred = cases
  { a: { b: { c: c } } } -> c

> nestedInferred { a: { b: { c: 11 } } }
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + nested         : {a: {b: Nat}} -> Nat
  + nestedInferred : {a: {b: {c: t | ...} | ...} | ...} -> t

  Run `update` to apply these changes to your codebase.

    5 | > nested { a: { b: 7 } }
          ⧩
          7

    11 | > nestedInferred { a: { b: { c: 11 } } }
           ⧩
           11
```

Record types unify with each other inside other structures.

``` unison
jons =
  [ { name: "Jon Arbuckle", age: 35 }
  , { name: "Jon Snow", age: 25 }
  ]

> jons
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + jons : [{age: Nat, name: Text}]

  Run `update` to apply these changes to your codebase.

    6 | > jons
          ⧩
          [ {age: 35, name: "Jon Arbuckle"}
          , {age: 25, name: "Jon Snow"}
          ]
```

### More type errors

A field whose type doesn't match across two records that have to unify.

``` unison :error
mismatchedFieldTypes =
  [ { name: "Jon Arbuckle", age: 35 }
  , { name: "Jon Snow", age: "25" }
  ]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  All the elements of a list need to have the same type.

  Here, one   is:  {age: Nat, name: Text}
  and another is:  {age: Text, name: Text}


      2 |   [ { name: "Jon Arbuckle", age: 35 }
      3 |   , { name: "Jon Snow", age: "25" }
```

A record pattern can only match a record. If we already know the scrutinee
isn't one, say so rather than reporting a generic mismatch.

``` unison :error
notARecord : Nat -> Nat
notARecord = cases
  { a: a } -> a
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  This is a record pattern, but the value it's matching isn't a
  record:

      3 |   { a: a } -> a


  It has type:
    Nat
```

Matching on a field the record doesn't have points at the pattern.

``` unison :error
noSuchField : { age: Nat } -> Nat
noSuchField = cases
  { name: n, age: _ } -> 1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  This pattern matches on a field called name , but the record
  it's matching doesn't have that field:

      3 |   { name: n, age: _ } -> 1


  The record being matched has type:
    {age: Nat}
```

### Pattern match coverage

Pattern match coverage should warn on multiple record matches since they're irrefutable.

``` unison :error
getAgeRedundant : { age: Nat, address: Text | ... } -> Nat
getAgeRedundant = cases
  { age:age } -> age
  { address:_ } -> 99
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  This case would be ignored because it's already covered by the preceding case(s):
        4 |   { address:_ } -> 99
    
```

Pattern match coverage should warn if there are NO cases, at least one is required.

``` unison :error
missingRight : Either Nat { age: Nat } -> Nat
missingRight = cases
  Left n -> n
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  Pattern match doesn't cover all possible cases:
        2 | missingRight = cases
        3 |   Left n -> n
    

  Patterns not matched:
   * Right _
```

``` unison :error
missingAllCases : { age: Nat } -> Nat
missingAllCases = cases
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  Pattern match doesn't cover all possible cases:
        2 | missingAllCases = cases
    

  Patterns not matched:
   * _
```

Void inside a record shouldn't require any cases (But currently it does)

``` unison :error
type Void =

getVoid : Either { x: Void } Nat -> Nat
getVoid = cases
  Right n -> n
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  Pattern match doesn't cover all possible cases:
        4 | getVoid = cases
        5 |   Right n -> n
    

  Patterns not matched:
   * Left _
```

### Universals

``` unison
> {a: 1} Universal.== {a: 1}
> {a: 1} Universal.== {a: 2}
> Any {a: 1} Universal.== Any {a: 1}
> Any {a: 1} Universal.== Any {a: 1, b: 2}
> Any {a: 1} Universal.== Any {a: 2}
> {a: 1} Universal.> {a: 1}
> {a: 2} Universal.> {a: 1}
> {a: 1} Universal.> {a: 2}
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.

    1 | > {a: 1} Universal.== {a: 1}
          ⧩
          true

    2 | > {a: 1} Universal.== {a: 2}
          ⧩
          false

    3 | > Any {a: 1} Universal.== Any {a: 1}
          ⧩
          true

    4 | > Any {a: 1} Universal.== Any {a: 1, b: 2}
          ⧩
          false

    5 | > Any {a: 1} Universal.== Any {a: 2}
          ⧩
          false

    6 | > {a: 1} Universal.> {a: 1}
          ⧩
          false

    7 | > {a: 2} Universal.> {a: 1}
          ⧩
          true

    8 | > {a: 1} Universal.> {a: 2}
          ⧩
          false
```
