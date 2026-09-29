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
  2. lib.     (775 terms, 120 types)
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
  6.  lib.        (775 terms, 120 types)
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
  7.  lib.        (775 terms, 120 types)
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

  This isn't a record, so there are no fields to read from it:

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

  This record has no field called name here:

      3 |   { name: n, age: _ } -> 1


  It has type:
    {age: Nat}
```

### Field names

A field can only be named once. The field map would otherwise be built with
`Map.fromList`, which silently keeps the last binding.

``` unison :error
dupLit = { a: 1, a: 2 }
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found the field a twice in the same record:

      1 | dupLit = { a: 1, a: 2 }


  Each field can only be named once.
```

``` unison :error
dupType : { a: Nat, a: Text } -> Nat
dupType = cases _ -> 1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found the field a twice in the same record:

      1 | dupType : { a: Nat, a: Text } -> Nat


  Each field can only be named once.
```

``` unison :error
dupPat = cases
  { a: x, a: y } -> x
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found the field a twice in the same record:

      2 |   { a: x, a: y } -> x


  Each field can only be named once.
```

The empty record is the record with no fields. It round-trips like any other.

``` unison
empty : { }
empty = { }

> empty
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + empty : {}

  Run `update` to apply these changes to your codebase.

    4 | > empty
          ⧩
          {}
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
   * Right {age: _}
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
   * {age: _}
```

A record with an uninhabited field is itself uninhabited, so it needs no case.

``` unison
type Void =

getVoid : Either { x: Void } Nat -> Nat
getVoid = cases
  Right n -> n
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type Void

  + getVoid : Either {x: Void} Nat -> Nat

  Run `update` to apply these changes to your codebase.
```

### Refutable patterns inside records

A record match is irrefutable, but its field patterns need not be. A literal in
a field:

``` unison
lit : { x: Nat | ... } -> Text
lit = cases
  { x: 0 } -> "zero"
  _ -> "other"

> lit { x: 0 }
> lit { x: 5 }
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + lit : {x: Nat | ...} -> Text

  Run `update` to apply these changes to your codebase.

    6 | > lit { x: 0 }
          ⧩
          "zero"

    7 | > lit { x: 5 }
          ⧩
          "other"
```

A constructor in a field, covering every case:

``` unison
con : { x: Optional Nat | ... } -> Nat
con = cases
  { x: Some n } -> n
  { x: None } -> 0

> con { x: Some 5 }
> con { x: None }
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + con : {x: Optional Nat | ...} -> Nat

  Run `update` to apply these changes to your codebase.

    6 | > con { x: Some 5 }
          ⧩
          5

    7 | > con { x: None }
          ⧩
          0
```

...and not covering every case:

``` unison :error
partialField : { x: Optional Nat | ... } -> Nat
partialField = cases
  { x: Some n } -> n
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  Pattern match doesn't cover all possible cases:
        2 | partialField = cases
        3 |   { x: Some n } -> n
    

  Patterns not matched:
   * {x: None}
```

Suggestions reach into nested records too:

``` unison :error
nestedSuggest : { a: { b: Optional Nat } } -> Nat
nestedSuggest = cases
  { a: { b: Some n } } -> n
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  Pattern match doesn't cover all possible cases:
        2 | nestedSuggest = cases
        3 |   { a: { b: Some n } } -> n
    

  Patterns not matched:
   * {a: {b: None}}
```

Two cases may match entirely different subsets of the record's fields. Each row
is compiled against the union of the fields matched anywhere in the match, so
the bindings stay aligned.

``` unison
subsets : { x: Nat, y: Text | ... } -> Text
subsets = cases
  { x: 0 } -> "zero"
  { y: y } -> y

> subsets { x: 0, y: "no" }
> subsets { x: 1, y: "one" }
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + subsets : {x: Nat, y: Text | ...} -> Text

  Run `update` to apply these changes to your codebase.

    6 | > subsets { x: 0, y: "no" }
          ⧩
          "zero"

    7 | > subsets { x: 1, y: "one" }
          ⧩
          "one"
```

A refutable first case leaves a later one reachable, so this is *not* redundant
\-- compare the `getAgeRedundant` case above.

``` unison
notRedundant : { age: Nat, address: Text | ... } -> Nat
notRedundant = cases
  { age: 0 } -> 1
  { address: _ } -> 99

> notRedundant { age: 0, address: "x" }
> notRedundant { age: 7, address: "x" }
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + notRedundant : {address: Text, age: Nat | ...} -> Nat

  Run `update` to apply these changes to your codebase.

    6 | > notRedundant { age: 0, address: "x" }
          ⧩
          1

    7 | > notRedundant { age: 7, address: "x" }
          ⧩
          99
```

### Values and code

A record value can be reflected into a `Value`, serialized, deserialized and
reified back. Fields are written in ascending name order regardless of the
order they were given in, so this uses deliberately unsorted source order with
distinguishable values.

``` unison
unpack : { a: Nat, z: Nat } -> (Nat, Nat)
unpack = cases { a: a, z: z } -> (a, z)

valueRoundTrip : '{IO, Exception} (Nat, Nat)
valueRoundTrip = do
  bytes = Value.serialize (Value.value {z: 7, a: 3})
  v = match Value.deserialize bytes with
    Right x -> x
    Left e -> bug e
  r : { a: Nat, z: Nat }
  r = match Value.load v with
    Right x -> x
    Left deps -> bug deps
  unpack r
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + unpack         : {a: Nat, z: Nat} -> (Nat, Nat)
  + valueRoundTrip : '{IO, Exception} (Nat, Nat)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> run valueRoundTrip

  (3, 7)
```

Code that builds and matches on records survives `validateLinks`,
serialization, and the code cache.

``` unison
mkRec : Nat -> { a: Nat, z: Text }
mkRec n = { a: n, z: "hi" }

readA : { a: Nat | ... } -> Nat
readA = cases { a: a } -> a

isRight : Either a b -> Boolean
isRight = cases
  Right _ -> true
  Left _ -> false
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + isRight : Either a b -> Boolean
  + readA   : {a: Nat | ...} -> Nat
  ~ mkRec : Nat -> {a: Nat, z: Text}

  + (added), ~ (modified)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
codeRoundTrip : '{IO, Exception} (Boolean, Nat)
codeRoundTrip = do
  tl = termLink mkRec
  code = match Code.lookup tl with
    Some c -> c
    None -> bug "no code"
  ok = Code.validateLinks [(tl, code)]
  bytes = Code.serialize code
  code2 = match Code.deserialize bytes with
    Right c -> c
    Left e -> bug e
  _ = Code.cache_ [(tl, code2)]
  (isRight ok, readA (mkRec 5))
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + codeRoundTrip : '{IO, Exception} (Boolean, Nat)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> run codeRoundTrip

  (true, 5)
```

A runtime error carrying a record shows real field names, since the names
travel with the value rather than being looked up by interned id.

``` unison :error
boom = bug { name: "Alice", age: 30 }

> boom
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + boom : b

  Run `update` to apply these changes to your codebase.

  💔💥

  I've encountered a call to builtin.bug with the following
  value:

    {age: 30, name: "Alice"}

  Stack trace:
    #135ikr7m3u
    #s2jmnl2nc9
```

### Reading fields

A record pattern in a destructuring bind reads fields without a `match`, and
works partially and at depth.

``` unison
sum3 : { x: Nat, y: Nat, z: Nat | ... } -> Nat
sum3 r =
  { x: x, y: y, z: z } = r
  x Nat.+ y Nat.+ z

getInner : { a: { b: Nat } | ... } -> Nat
getInner r =
  { a: { b: b } } = r
  b

> sum3 { x: 1, y: 2, z: 3, extra: "ignored" }
> getInner { a: { b: 42 }, c: 1 }
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + getInner : {a: {b: Nat} | ...} -> Nat
  + sum3     : {x: Nat, y: Nat, z: Nat | ...} -> Nat
      (also named addUpRec)

  Run `update` to apply these changes to your codebase.

    11 | > sum3 { x: 1, y: 2, z: 3, extra: "ignored" }
           ⧩
           6

    12 | > getInner { a: { b: 42 }, c: 1 }
           ⧩
           42
```

`r@x` reads the `x` field inline. It binds tighter than application, so
`f r@x` is `f (r@x)`, and it chains.

``` unison
rec : { a: Nat, b: Text }
rec = { a: 7, b: "hi" }

nested : { a: { b: { c: Nat } } }
nested = { a: { b: { c: 42 } } }

> rec@a
> rec@b
> Nat.increment rec@a
> nested@a@b@c
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + nested : {a: {b: {c: Nat}}}
  + rec    : {a: Nat, b: Text}

  Run `update` to apply these changes to your codebase.

    7 | > rec@a
          ⧩
          7

    8 | > rec@b
          ⧩
          "hi"

    9 | > Nat.increment rec@a
          ⧩
          8

    10 | > nested@a@b@c
           ⧩
           42
```

Unlike a qualified name, the left side can be any expression, not just an
identifier.

``` unison
mkRec2 : Nat -> { v: Nat }
mkRec2 n = { v: n }

> (mkRec2 5)@v
> ({ v: 9 })@v
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + mkRec2 : Nat -> {v: Nat}

  Run `update` to apply these changes to your codebase.

    4 | > (mkRec2 5)@v
          ⧩
          5

    5 | > ({ v: 9 })@v
          ⧩
          9
```

The `@` has to be adjacent to both sides, which is what keeps it from being
confused with the `@` of an as-pattern.

``` unison :error
spaced : { a: Nat } -> Nat
spaced r = r @ a
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I got confused here:

      2 | spaced r = r @ a


  I was surprised to find a '@' here.
  I was expecting one of these instead:

  * and
  * bang
  * do
  * false
  * force
  * handle
  * if
  * infixApp
  * let
  * newline or semicolon
  * or
  * quote
  * termLink
  * true
  * tuple
  * typeLink
```

``` unison
asPat : Optional Nat -> Nat
asPat = cases
  x@(Some n) -> n
  None -> 0

> asPat (Some 5)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + asPat : Optional Nat -> Nat

  Run `update` to apply these changes to your codebase.

    6 | > asPat (Some 5)
          ⧩
          5
```

Reading a field the record doesn't have, or reading from something that isn't a
record:

``` unison :error
noSuchField2 : { a: Nat } -> Nat
noSuchField2 r = r@nope
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  This record has no field called nope here:

      2 | noSuchField2 r = r@nope


  It has type:
    {a: Nat}
```

``` unison :error
notARecord2 : Nat -> Nat
notARecord2 n = n@a
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  This isn't a record, so there are no fields to read from it:

      2 | notARecord2 n = n@a


  It has type:
    Nat
```

A projection prints back as a projection.

``` unison
projA : { a: Nat | ... } -> Nat
projA r = Nat.increment r@a

projDeep : { a: { b: Nat } | ... } -> Nat
projDeep r = r@a@b
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + projA    : {a: Nat | ...} -> Nat
  + projDeep : {a: {b: Nat} | ...} -> Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> view projA projDeep

  projA : {a: Nat | ...} -> Nat
  projA r = Nat.increment r@a

  projDeep : {a: {b: Nat} | ...} -> Nat
  projDeep r = r@a@b
```

### Universals

Records are ordered by field name, most significant field first -- not by the
order field names happened to be interned, which would make the result depend
on unrelated compilation history. Two records of *different* shapes can be
compared once wrapped in `Any`, which erases their types; those are ordered by
comparing the sorted field-name lists.

``` unison
> Universal.compare {a: 2, b: 1} {a: 1, b: 2}
> Universal.compare (Any {a: 1}) (Any {b: 1})
> Universal.compare (Any {b: 1}) (Any {a: 1})
> Universal.compare (Any {a: 1}) (Any {a: 1, b: 2})
> Universal.compare (Any {a: 1, b: 2}) (Any {a: 1})
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.

    1 | > Universal.compare {a: 2, b: 1} {a: 1, b: 2}
          ⧩
          +1

    2 | > Universal.compare (Any {a: 1}) (Any {b: 1})
          ⧩
          -1

    3 | > Universal.compare (Any {b: 1}) (Any {a: 1})
          ⧩
          +1

    4 | > Universal.compare (Any {a: 1}) (Any {a: 1, b: 2})
          ⧩
          -1

    5 | > Universal.compare (Any {a: 1, b: 2}) (Any {a: 1})
          ⧩
          +1
```

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

### Rewrite rules

A `@rewrite case` rule can match on a record pattern. Here the field value is
a literal:

``` unison
litRule = @rewrite
  case {x: 0} ==> {x: 1}

targetLit : {x: Nat} -> Nat
targetLit = cases
  {x: 0} -> 100
  {x: n} -> n
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + litRule   : Rewrites
                  (Tuple (RewriteCase {x: Nat} {x: Nat}) ())
  + targetLit : {x: Nat} -> Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> rewrite litRule

  ☝️

  I found and replaced matches in these definitions: targetLit

  The rewritten file has been added to the top of scratch.u
```

``` unison :added-by-ucm scratch.u
-- | Rewrote using: 
-- | Modified definition(s): targetLit

litRule = @rewrite case {x: 0} ==> {x: 1}

targetLit : {x: Nat} -> Nat
targetLit = cases
  {x: 1} -> 100
  {x: n} -> n
```

``` ucm :hide
scratch/main> load

scratch/main> add
```

A record pattern binds its variables in ascending field-name order, so a rule
that swaps two field subpatterns has to move the bindings along with them. The
rewritten body still refers to `p` and `q` by name, and they follow the fields
they were swapped onto:

``` unison
swapRule a b = @rewrite
  case {x: a, y: b} ==> {x: b, y: a}

targetSwap : {x: Nat, y: Nat} -> Nat
targetSwap = cases
  {x: p, y: q} -> p Nat.+ (q Nat.* 2)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + swapRule   : a
                 -> b
                 -> Rewrites
                   (Tuple
                     (RewriteCase {x: a, y: b} {x: b, y: a}) ())
  + targetSwap : {x: Nat, y: Nat} -> Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> rewrite swapRule

  ☝️

  I found and replaced matches in these definitions: targetSwap

  The rewritten file has been added to the top of scratch.u
```

``` unison :added-by-ucm scratch.u
-- | Rewrote using: 
-- | Modified definition(s): targetSwap

swapRule a b = @rewrite case {x: a, y: b} ==> {x: b, y: a}

targetSwap : {x: Nat, y: Nat} -> Nat
targetSwap = cases {x: q, y: p} -> p Nat.+ q Nat.* 2
```

``` ucm :hide
scratch/main> load

scratch/main> add
```

Record patterns nest:

``` unison
nestRule = @rewrite
  case {x: {y: 0}} ==> {x: {y: 1}}

targetNest : {x: {y: Nat}} -> Nat
targetNest = cases
  {x: {y: 0}} -> 100
  {x: {y: n}} -> n
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + nestRule   : Rewrites
                   (Tuple
                     (RewriteCase {x: {y: Nat}} {x: {y: Nat}})
                     ())
  + targetNest : {x: {y: Nat}} -> Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> rewrite nestRule

  ☝️

  I found and replaced matches in these definitions: targetNest

  The rewritten file has been added to the top of scratch.u
```

``` unison :added-by-ucm scratch.u
-- | Rewrote using: 
-- | Modified definition(s): targetNest

nestRule = @rewrite case {x: {y: 0}} ==> {x: {y: 1}}

targetNest : {x: {y: Nat}} -> Nat
targetNest = cases
  {x: {y: 1}} -> 100
  {x: {y: n}} -> n
```

``` ucm :hide
scratch/main> load

scratch/main> add
```

Finally, a rule that has nothing to do with records still has to pass over any
record patterns in the definitions it rewrites:

``` unison
structural type Flag = On | Off

flagRule = @rewrite
  case On ==> Off

targetFlag : {x: Flag} -> Nat
targetFlag = cases
  {x: f} -> match f with
    On -> 1
    _ -> 0
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + structural type Flag

  + flagRule   : Rewrites (Tuple (RewriteCase Flag Flag) ())
  + targetFlag : {x: Flag} -> Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> rewrite flagRule

  ☝️

  I found and replaced matches in these definitions: targetFlag

  The rewritten file has been added to the top of scratch.u
```

``` unison :added-by-ucm scratch.u
-- | Rewrote using: 
-- | Modified definition(s): targetFlag

structural type Flag = On | Off

flagRule = @rewrite case Flag.On ==> Flag.Off

targetFlag : {x: Flag} -> Nat
targetFlag = cases
  {x: f} ->
    match f with
      Flag.Off -> 1
      _        -> 0
```

``` ucm :hide
scratch/main> load
```

A leading underscore makes the field's subpattern a wildcard, the same as
anywhere else in a rule:

``` unison
wildRule _w = @rewrite
  case {w: _w} ==> {w: 0}

targetWild : {w: Nat} -> Nat
targetWild = cases
  {w: _} -> 7
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + targetWild : {w: Nat} -> Nat
  + wildRule   : ∀ _w.
                   _w
                   -> Rewrites
                     (Tuple (RewriteCase {w: _w} {w: Nat}) ())

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> rewrite wildRule

  ☝️

  I found and replaced matches in these definitions: targetWild

  The rewritten file has been added to the top of scratch.u
```

``` unison :added-by-ucm scratch.u
-- | Rewrote using: 
-- | Modified definition(s): targetWild

wildRule _w = @rewrite case {w: _w} ==> {w: 0}

targetWild : {w: Nat} -> Nat
targetWild = cases {w: 0} -> 7
```
