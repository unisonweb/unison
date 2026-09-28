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

  + addUpRec : {x: Nat, y: Nat, z: Nat | ... } -> Nat
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

  1. addUpRec ({x: Nat, y: Nat, z: Nat | ... } -> Nat)
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
  3.  addUpRec    ({x: Nat, y: Nat, z: Nat | ... } -> Nat)
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

  I didn't expect this record:

      2 |   { name:name, age:_ } -> name


  to have the field
    name: 𝕩16

  because it should have the type:
    
    {age: Nat}
    

  derived from here:

      5 | > getName { age: 30 }
```

We should get a nice error if we have additional unexpected fields.

``` unison :error
type Person = Person { name: Text, age: Nat }

-- 'address' is an unexpected field
createPerson = Person { name: "Alice", age: 30, address: "123 Main St" }
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I expected this record:

      4 | createPerson = Person { name: "Alice", age: 30, address: "123 Main St" }


  to have the field
    address: Text

  so that it would match the type:
    
    {age: Nat, name: Text}
    

  from here:

      4 | createPerson = Person { name: "Alice", age: 30, address: "123 Main St" }
```

Record field projections should infer the most general record type:

``` unison
getAddress = cases
    { address: address } -> address
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + getAddress : {address: t | ... } -> t

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
  3.  addUpRec    ({x: Nat, y: Nat, z: Nat | ... } -> Nat)
  4.  getAddress  ({address: t | ... } -> t)
  5.  getX        (Point -> Nat)
  6.  getY        (Point -> Nat)
  7.  lib.        (747 terms, 116 types)
  8.  mkPoint     (Nat -> Nat -> Point)
  9.  mkRec       (a -> b -> c -> {x: a, y: b, z: c})
  10. p           (Point)
  11. unpackPoint (Point -> (Nat, Nat))
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
