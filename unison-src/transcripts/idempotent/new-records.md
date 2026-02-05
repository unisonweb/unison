# Structural records

``` ucm
scratch/main> builtins.merge lib.builtins

  Done.
```

We should be able to write simple functions which construct record types, and can evaluate them.

``` unison
mkRec a b c = { x: a, y: b, z: c }
> mkRec 1 2 3

unpackRec = cases
  { x:x, y:y, z:z } -> x Nat.+ y Nat.+ z

> unpackRec (mkRec 1 2 3)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + mkRec     : a -> b -> c -> {x: a, y: b, z: c}
  + unpackRec : {x: Nat, y: Nat, z: Nat} -> Nat

  Run `update` to apply these changes to your codebase.

    2 | > mkRec 1 2 3
          ⧩
          {x: 1, y: 2, z: 3}

    7 | > unpackRec (mkRec 1 2 3)
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

  1. lib.      (746 terms, 116 types)
  2. mkRec     (a -> b -> c -> {x: a, y: b, z: c})
  3. unpackRec ({x: Nat, y: Nat, z: Nat} -> Nat)
```

We should be able to create wrapper types which encapsulate records, and manipulate them.

``` unison
type Point = Point { x: Nat, y: Nat }

mkPoint x y = Point { x: x, y: y }

unpackPoint = cases
  Point { x:x, y:y } -> x + y

getX = cases
    Point { x:x, y:y } -> x
getY = cases
    Point { x:x, y:y } -> y

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
  + unpackPoint : Point -> Nat

  Run `update` to apply these changes to your codebase.

    15 | > unpackPoint p
           ⧩
           7

    16 | > getX p
           ⧩
           3

    17 | > getY p
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
  3.  getX        (Point -> Nat)
  4.  getY        (Point -> Nat)
  5.  lib.        (746 terms, 116 types)
  6.  mkPoint     (Nat -> Nat -> Point)
  7.  mkRec       (a -> b -> c -> {x: a, y: b, z: c})
  8.  p           (Point)
  9.  unpackPoint (Point -> Nat)
  10. unpackRec   ({x: Nat, y: Nat, z: Nat} -> Nat)

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
