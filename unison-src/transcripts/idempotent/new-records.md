# Structural records

```ucm
scratch/main> builtins.merge lib.builtins
```

We should be able to write simple functions which construct record types, and can evaluate them.

```unison
mkRec a b c = { x: a, y: b, z: c }
> mkRec 1 2 3

unpackRec = cases
  { x, y, z } -> x + y + z

> unpackRec (mkRec 1 2 3)
```

And can add them to the codebase;

```ucm
scratch/main> update
```

We should be able to create wrapper types which encapsulate records, and manipulate them.

```unison
type Point = Point { x: Nat, y: Nat }

mkPoint x y = Point { x: x, y: y }

unpackPoint = cases
  Point { x, y } -> x + y

getX = cases
    Point { x, y } -> x
getY = cases
    Point { x, y } -> y

p = mkPoint 3 4

> unpackPoint p
> getX p
> getY p
```

We should be able to add them to the codebase;

```ucm
scratch/main> update
```

We should get a nice error if we are missing a field from an expected type.

```unison:error
getName = cases
  { name, age } -> name

-- Missing the 'name' field
> getName { age: 30 }
```

We should get a nice error if we have additional unexpected fields.

```unison:error
type Person = Person { name: Text, age: Nat }

-- 'address' is an unexpected field
createPerson = Person { name: "Alice", age: 30, address: "123 Main St" }
```
