# Structural records

Syntax at a glance:

| syntax | meaning |
| --- | --- |
| `{ x: 1, y: 2 }` | record literal; fields are stored in name order, not source order |
| `{ x: Nat, y: Text }` | record type |
| `type Point = Point { x: Nat, y: Nat }` | a constructor with a record payload, built by `Point { x: 1, y: 2 }` |
| `{ x: Nat | r }` | names the trailing row, so a function can talk about the other fields |
| `{ x: Nat | { y: Text | r } }` | a row can itself be a record; partial nesting is flattened |
| `r@x` | read a field; binds tighter than application (`f r@x` is `f (r@x)`) and chains (`r@a@b`) |
| `r@ x`, `r @ x` | the same getter — the `@` may be spaced on either side |
| `(@x)` | the getter as a function |
| `r@x.set v` | set a field (`r@ x.set v` and `r @ x.set v` too) |
| `(@x.set)` | the setter as a function |
| `r@{x: @x + 1, y: @y - 1}` | record-set block: set several fields at once; each `@x` is the field's value *before* it is set |
| `r@field1@field2@{x: @x + 1}` | modify a nested sub-record |
| `{ x: x, y: y }` | a pattern; fields may be listed in any order and omitted ones are ignored |
| `{ x: { y: y } }` | nested patterns, to any depth |
| `{ x: 0 }` | a *refutable* field pattern, e.g. to match a literal |
| `{ x: x } = r` | destructuring bind — no `match` needed |
| `{ }` | the empty record |
| `x@(Some n)` | unrelated: a pattern as-binding |
| `@` in a doc special form | unrelated: a distinct token from the getter `@` |

``` ucm
scratch/main> builtins.merge lib.builtins

  Done.
```

A record literal builds a value with named fields; fields are stored sorted by name rather than in
source order. `r@f` is a getter and `r@f.set v` is a setter; `(@f)` and `(@f.set)` are the same two
as functions.

``` unison
mkRec : a -> b -> c -> { x: a, y: b, z: c }
mkRec a b c = { x: a, y: b, z: c }

addUpRec : { x: Nat, y: Nat, z: Nat } -> Nat
addUpRec = cases
  { x: x, y: y, z: z } -> x Nat.+ y Nat.+ z

List.map : (a ->{e} b) -> [a] ->{e} [b]
List.map f =
  use List :+
  go acc = cases
    a +: as -> go (acc :+ f a) as
    []      -> acc
  go []

List.zipWith : (a -> b ->{e} c) -> [a] -> [b] ->{e} [c]
List.zipWith f a b =
  use List :+
  go acc as bs = match (as, bs) with
    (ah +: at, bh +: bt) -> go (acc :+ f ah bh) at bt
    _                    -> acc
  go [] a b

> mkRec 1 2 3
> let r = { name : "Bob", age : 99 }
      r@age
> addUpRec (mkRec 1 2 3)
> List.map (@name) [{name: "Alice", age : 40}, {name: "Bob", age : 33}]
> List.zipWith (@age.set) [{name: "Alice", age : 40}, {name: "Bob", age : 33}] [22, 23]
> List.map (r -> r@age + Text.size r@name) [{name: "Alice", age : 40}, {name: "Bob", age : 33}]

-- `@f.set` sets a field
> { age: 29, name: "Bob" } @ age.set 30
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + addUpRec     : {x: Nat, y: Nat, z: Nat} -> Nat
  + List.map     : (a ->{e} b) -> [a] ->{e} [b]
  + List.zipWith : (a -> b ->{e} c) -> [a] -> [b] ->{e} [c]
  + mkRec        : a -> b -> c -> {x: a, y: b, z: c}

  Run `update` to apply these changes to your codebase.

    24 | > mkRec 1 2 3
           ⧩
           {x: 1, y: 2, z: 3}

    25 | > let r = { name : "Bob", age : 99 }
           ⧩
           99

    27 | > addUpRec (mkRec 1 2 3)
           ⧩
           6

    28 | > List.map (@name) [{name: "Alice", age : 40}, {name: "Bob", age : 33}]
           ⧩
           ["Alice", "Bob"]

    29 | > List.zipWith (@age.set) [{name: "Alice", age : 40}, {name: "Bob", age : 33}] [22, 23]
           ⧩
           [{age: 22, name: "Alice"}, {age: 23, name: "Bob"}]

    30 | > List.map (r -> r@age + Text.size r@name) [{name: "Alice", age : 40}, {name: "Bob", age : 33}]
           ⧩
           [45, 36]

    33 | > { age: 29, name: "Bob" } @ age.set 30
           ⧩
           {age: 30, name: "Bob"}
```

## Getters and setters

`@foo.set` sets a field instead of reading it. Its `@` may be spaced on either
side — `r@foo.set v`, `r@ foo.set v`, `r @ foo.set v`. A space *only* before the `@`
is not a setter: `@foo` reads a field of the record a block is setting, and those reads
only exist inside a block's braces, so there is no term to read and it is a parse
error.

``` unison
setFoo : { foo: a | rest } -> b -> { foo: b | rest }
setFoo r v = r @ foo.set v

> setFoo { foo: "hi", age: 29 } []
> { foo: "hi", age: 29 } @ foo.set [1, 2, 3]
> { foo: 1, bar: true } @ foo.set "hello"
> { foo: "hi", age: 29 } @ foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + setFoo : {foo: a | rest} -> b -> {foo: b | rest}

  Run `update` to apply these changes to your codebase.

    4 | > setFoo { foo: "hi", age: 29 } []
          ⧩
          {age: 29, foo: []}

    5 | > { foo: "hi", age: 29 } @ foo.set [1, 2, 3]
          ⧩
          {age: 29, foo: [1, 2, 3]}

    6 | > { foo: 1, bar: true } @ foo.set "hello"
          ⧩
          {bar: true, foo: "hello"}

    7 | > { foo: "hi", age: 29 } @ foo
          ⧩
          "hi"
```

The field has to be there already, and the target has to be a record:

``` unison :error
noField : { a: Nat } -> Nat
noField r = r@nope.set 0
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I expected this record:

      1 | noField : { a: Nat } -> Nat


  to have the field
    nope: 𝕤7

  so that it would match the type:
    
    {nope: 𝕤7 | _row8}
    

  from here:

      2 | noField r = r@nope.set 0
```

``` unison :error
setOnNat : Nat -> Nat
setOnNat n = n@foo.set 0
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found a value  of type:  Nat
  where I expected to find:  {foo: 𝕤 | _row}

      2 | setOnNat n = n@foo.set 0

    from right here:

      1 | setOnNat : Nat -> Nat
```

Outside a block there is no `@foo` term to read, so a space before the `@` makes this a
parse error rather than a type error:

``` unison :error
spacedSet r v = r @foo.set v
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I got confused here:

      1 | spacedSet r v = r @foo.set v


  I was surprised to find a '@' here.
  I was expecting one of these instead:

  * and
  * infixApp
  * newline or semicolon
  * or
```

Both operators also have a prefix form, `(@foo)` and `(@foo.set)`, so they can be defined
once and passed around like any other function:

``` unison
getFoo : { foo: a } -> a
getFoo = (@foo)

setFoo2 : { foo: a | rest } -> b -> { foo: b | rest }
setFoo2 = (@foo.set)

> getFoo { foo: 7, bar: true }
> setFoo2 { foo: "hi", age: 29 } []
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + getFoo  : {foo: a} -> a
  + setFoo2 : {foo: a | rest} -> b -> {foo: b | rest}

  Run `update` to apply these changes to your codebase.

    7 | > getFoo { foo: 7, bar: true }
          ⧩
          7

    8 | > setFoo2 { foo: "hi", age: 29 } []
          ⧩
          {age: 29, foo: []}
```

A record can be the payload of a constructor, and a record pattern can bind only the fields it
names. A constructor's record parameter may also be given extra fields, since the caller may
supply more than the type mentions:

``` unison
type Point = Point { x: Nat, y: Nat }

mkPoint : Nat -> Nat -> Point
mkPoint x y = Point { x: x, y: y }

unpackPoint : Point -> (Nat, Nat)
unpackPoint = cases
  Point { x: x, y: y } -> (x, y)

-- partial record patterns bind just the fields they mention
getX : Point -> Nat
getX = cases
  Point { x: x } -> x

getY : Point -> Nat
getY = cases
  Point { y: y } -> y

p : Point
p = mkPoint 3 4

type Person = Person { name: Text, age: Nat }
createPerson = Person { name: "Alice", age: 30 }

-- a partial pattern infers the most general record type
getAddress = cases
  { address: address } -> address

> unpackPoint p
> getX p
> getY p
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type Person
  + type Point

  + createPerson : Person
  + getAddress   : {address: t} -> t
  + getX         : Point -> Nat
  + getY         : Point -> Nat
  + mkPoint      : Nat -> Nat -> Point
  + p            : Point
  + unpackPoint  : Point -> (Nat, Nat)

  Run `update` to apply these changes to your codebase.

    29 | > unpackPoint p
           ⧩
           (3, 4)

    30 | > getX p
           ⧩
           3

    31 | > getY p
           ⧩
           4
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> ls

  1.  Person       (type)
  2.  Person.      (1 term)
  3.  Point        (type)
  4.  Point.       (1 term)
  5.  createPerson (Person)
  6.  getAddress   ({address: t} -> t)
  7.  getX         (Point -> Nat)
  8.  getY         (Point -> Nat)
  9.  lib.         (778 terms, 120 types)
  10. mkPoint      (Nat -> Nat -> Point)
  11. p            (Point)
  12. unpackPoint  (Point -> (Nat, Nat))

scratch/main> view Point

  type Point = Point {x: Nat, y: Nat}
```

A constructor's field type is exact, so an extra field is an error:

``` unison :error
createPersonExtra = Person { name: "Alice", age: 30, address: "123 Main St" }
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I didn't expect this record:

      1 | createPersonExtra = Person { name: "Alice", age: 30, address: "123 Main St" }


  to have the field
    address: Text

  because it should have the type:
    
    {age: Nat, name: Text}
    

  derived from here:

```

A record literal that's missing a field the expected type has is an error:

``` unison :error
getName = cases
  { name: name, age: _ } -> name

-- Missing the 'name' field
> getName { age: 30 }
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I expected this record:

      5 | > getName { age: 30 }


  to have the field
    name: 𝕩18

  so that it would match the type:
    
    {age: 𝕩17, name: 𝕩18 | 𝕤19}
    

  from here:

      2 |   { name: name, age: _ } -> name
```

## Nested records

Records nest, both as literals and as patterns, to arbitrary depth, and record types unify with
each other inside other structures:

``` unison
nested : { a: { b: Nat } } -> Nat
nested = cases
  { a: { b: b } } -> b

-- with no annotation every field type is inferred
nestedInferred = cases
  { a: { b: { c: c } } } -> c

jons =
  [ { name: "Jon Arbuckle", age: 35 }
  , { name: "Jon Snow", age: 25 }
  ]

> nested { a: { b: 7 } }
> nestedInferred { a: { b: { c: 11 } } }
> jons
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + jons           : [{age: Nat, name: Text}]
  + nested         : {a: {b: Nat}} -> Nat
  + nestedInferred : {a: {b: {c: t}}} -> t

  Run `update` to apply these changes to your codebase.

    14 | > nested { a: { b: 7 } }
           ⧩
           7

    15 | > nestedInferred { a: { b: { c: 11 } } }
           ⧩
           11

    16 | > jons
           ⧩
           [ {age: 35, name: "Jon Arbuckle"}
           , {age: 25, name: "Jon Snow"}
           ]
```

## Pattern coverage

A record match is irrefutable, so a second record pattern is redundant and reported:

``` unison :error
getAgeRedundant : { age: Nat, address: Text } -> Nat
getAgeRedundant = cases
  { age: age } -> age
  { address: _ } -> 99
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  This case would be ignored because it's already covered by the preceding case(s):
        4 |   { address: _ } -> 99
    
```

A match must cover every record shape, and an uninhabited field needs no case:

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

A field pattern need not be irrefutable, so a literal or constructor in a field can leave a later
case reachable, and different cases may match different subsets of the fields -- each case is
compiled against the union of the fields matched anywhere in the match:

``` unison
lit : { x: Nat } -> Text
lit = cases
  { x: 0 } -> "zero"
  _ -> "other"

con : { x: Optional Nat } -> Nat
con = cases
  { x: Some n } -> n
  { x: None } -> 0

subsets : { x: Nat, y: Text } -> Text
subsets = cases
  { x: 0 } -> "zero"
  { y: y } -> y

-- the first case is refutable, so the second is reachable -- compare getAgeRedundant above
notRedundant : { age: Nat, address: Text } -> Nat
notRedundant = cases
  { age: 0 } -> 1
  { address: _ } -> 99

> lit { x: 0 }
> lit { x: 5 }
> con { x: Some 5 }
> con { x: None }
> subsets { x: 0, y: "no" }
> subsets { x: 1, y: "one" }
> notRedundant { age: 0, address: "x" }
> notRedundant { age: 7, address: "x" }
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + con          : {x: Optional Nat} -> Nat
  + lit          : {x: Nat} -> Text
  + notRedundant : {address: Text, age: Nat} -> Nat
  + subsets      : {x: Nat, y: Text} -> Text

  Run `update` to apply these changes to your codebase.

    22 | > lit { x: 0 }
           ⧩
           "zero"

    23 | > lit { x: 5 }
           ⧩
           "other"

    24 | > con { x: Some 5 }
           ⧩
           5

    25 | > con { x: None }
           ⧩
           0

    26 | > subsets { x: 0, y: "no" }
           ⧩
           "zero"

    27 | > subsets { x: 1, y: "one" }
           ⧩
           "one"

    28 | > notRedundant { age: 0, address: "x" }
           ⧩
           1

    29 | > notRedundant { age: 7, address: "x" }
           ⧩
           99
```

A field pattern that doesn't cover every case is reported with a suggestion:

``` unison :error
partialField : { x: Optional Nat } -> Nat
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

## More type errors

A field whose type doesn't match across two records that have to unify:

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

A record pattern can only match a record, and a pattern can only mention fields the record has:

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
    {age: Nat | _rest4}
```

## Field names

A field can only be named once -- the field map would otherwise be built with `Map.fromList`, which
silently keeps the last binding -- and the empty record is the record with no fields:

``` unison :error
dupLit = { a: 1, a: 2 }
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found the field a twice in the same record:

      1 | dupLit = { a: 1, a: 2 }


  Each field can only be named once.
```

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

## Reading fields

A record pattern in a destructuring bind reads fields without a `match`, and `r@x` reads one
inline. A getter binds tighter than application, so `f r@x` is `f (r@x)`, and its left side
can be any expression, not just a name:

``` unison
sum3 : { x: Nat, y: Nat, z: Nat } -> Nat
sum3 r =
  { x: x, y: y, z: z } = r
  x Nat.+ y Nat.+ z

getInner : { a: { b: Nat } } -> Nat
getInner r =
  { a: { b: b } } = r
  b

rec : { a: Nat, b: Text }
rec = { a: 7, b: "hi" }

recNested : { a: { b: { c: Nat } } }
recNested = { a: { b: { c: 42 } } }

mkRec2 : Nat -> { v: Nat }
mkRec2 n = { v: n }

> sum3 { x: 1, y: 2, z: 3, extra: "ignored" }
> getInner { a: { b: 42 }, c: 1 }
> rec@a
> rec@b
> Nat.increment rec@a
> recNested@a@b@c
> (mkRec2 5)@v
> ({ v: 9 })@v
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + getInner  : {a: {b: Nat}} -> Nat
  + mkRec2    : Nat -> {v: Nat}
  + rec       : {a: Nat, b: Text}
  + recNested : {a: {b: {c: Nat}}}
  + sum3      : {x: Nat, y: Nat, z: Nat} -> Nat

  Run `update` to apply these changes to your codebase.

    20 | > sum3 { x: 1, y: 2, z: 3, extra: "ignored" }
           ⧩
           6

    21 | > getInner { a: { b: 42 }, c: 1 }
           ⧩
           42

    22 | > rec@a
           ⧩
           7

    23 | > rec@b
           ⧩
           "hi"

    24 | > Nat.increment rec@a
           ⧩
           8

    25 | > recNested@a@b@c
           ⧩
           42

    26 | > (mkRec2 5)@v
           ⧩
           5

    27 | > ({ v: 9 })@v
           ⧩
           9
```

The `@` may be spaced on either side, so `r@a`, `r@ a` and `r @ a` are all the same getter.
A space before the `@` and none after it -- `r @a` -- is not a getter: it is `r` applied to
`@a`, a read of one of the fields a record-set block is setting. (`@` also introduces a doc special form,
but that's a distinct token.)

``` unison
spaced : { a: Nat } -> Nat
spaced r = r @ a

asPat : Optional Nat -> Nat
asPat = cases
  x@(Some n) -> n
  None -> 0

> spaced { a: 7 }
> asPat (Some 5)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + asPat  : Optional Nat -> Nat
  + spaced : {a: Nat} -> Nat

  Run `update` to apply these changes to your codebase.

    9 | > spaced { a: 7 }
          ⧩
          7

    10 | > asPat (Some 5)
           ⧩
           5
```

Reading a field the record doesn't have, or reading from something that isn't a record:

``` unison :error
noSuchField2 : { a: Nat } -> Nat
noSuchField2 r = r@nope
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  This record has no field called nope here:

      2 | noSuchField2 r = r@nope


  It has type:
    {a: Nat | _rest4}
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

A getter prints back as a getter:

``` unison
projA : { a: Nat } -> Nat
projA r = Nat.increment r@a

projDeep : { a: { b: Nat } } -> Nat
projDeep r = r@a@b
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + projA    : {a: Nat} -> Nat
  + projDeep : {a: {b: Nat}} -> Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> view projA projDeep

  projA : {a: Nat} -> Nat
  projA r = Nat.increment r@a

  projDeep : {a: {b: Nat}} -> Nat
  projDeep r = r@a@b
```

## Values and code

A record value can be reflected into a `Value`, serialized, deserialized and reified back. Code
that builds and matches on records then survives `validateLinks`, serialization and the code cache
— referring to a definition by name needs it to be in the codebase first, so this updates in
between:

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

mkRecV : Nat -> { a: Nat, z: Text }
mkRecV n = { a: n, z: "hi" }

readA : { a: Nat } -> Nat
readA = cases { a: a } -> a

isRight : Either a b -> Boolean
isRight = cases
  Right _ -> true
  Left _ -> false
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + isRight        : Either a b -> Boolean
  + mkRecV         : Nat -> {a: Nat, z: Text}
  + readA          : {a: Nat} -> Nat
  + unpack         : {a: Nat, z: Nat} -> (Nat, Nat)
  + valueRoundTrip : '{IO, Exception} (Nat, Nat)

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
  tl = termLink mkRecV
  code = match Code.lookup tl with
    Some c -> c
    None -> bug "no code"
  ok = Code.validateLinks [(tl, code)]
  bytes = Code.serialize code
  code2 = match Code.deserialize bytes with
    Right c -> c
    Left e -> bug e
  _ = Code.cache_ [(tl, code2)]
  (isRight ok, readA (mkRecV 5))
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

scratch/main> run valueRoundTrip

  (3, 7)

scratch/main> run codeRoundTrip

  (true, 5)
```

A runtime error carrying a record shows real field names, since the names travel with the value
rather than being looked up by interned id:

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

## Universals

Records are ordered by field name, most significant field first -- not by the order the names
happened to be interned, which would make the result depend on unrelated compilation history. Two
records of *different* shapes can be compared once wrapped in `Any`, which erases their types;
those are ordered by comparing the sorted field-name lists:

``` unison
> Universal.compare {a: 2, b: 1} {a: 1, b: 2}
> Universal.compare (Any {a: 1}) (Any {b: 1})
> Universal.compare (Any {b: 1}) (Any {a: 1})
> Universal.compare (Any {a: 1}) (Any {a: 1, b: 2})
> Universal.compare (Any {a: 1, b: 2}) (Any {a: 1})
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

    6 | > {a: 1} Universal.== {a: 1}
          ⧩
          true

    7 | > {a: 1} Universal.== {a: 2}
          ⧩
          false

    8 | > Any {a: 1} Universal.== Any {a: 1}
          ⧩
          true

    9 | > Any {a: 1} Universal.== Any {a: 1, b: 2}
          ⧩
          false

    10 | > Any {a: 1} Universal.== Any {a: 2}
           ⧩
           false

    11 | > {a: 1} Universal.> {a: 1}
           ⧩
           false

    12 | > {a: 2} Universal.> {a: 1}
           ⧩
           true

    13 | > {a: 1} Universal.> {a: 2}
           ⧩
           false
```

Comparing records of different shapes directly is a type error -- the two sides
can't share a type. Wrap them in `Any` to compare across shapes, as above:

``` unison :error
> {a: 1, b: 2} Universal.== {a: 1}
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I expected this record:

      1 | > {a: 1, b: 2} Universal.== {a: 1}


  to have the field
    b: Nat

  so that it would match the type:
    
    {a: Nat, b: Nat}
    

  from here:

      1 | > {a: 1, b: 2} Universal.== {a: 1}
```

## Rewrite rules

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

## Rows

A record type's trailing row can be named, `{ x: Nat | r }`, which lets a function talk about a record's
*other* fields. The row is solved at the call site, so `rest` is `{y: Text}` in the first call
below and empty in the second:

``` unison
rowId : { x: Nat | r } -> { x: Nat | r }
rowId r = r

firstOf : { x: Nat | rest } -> Nat
firstOf r = r@x

> firstOf { x: 1, y: "hi" }
> rowId { x: 1, y: "hi" }
> rowId { x: 1 }
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + firstOf : {x: Nat} -> Nat
  + rowId   : {x: Nat | r} -> {x: Nat | r}

  Run `update` to apply these changes to your codebase.

    7 | > firstOf { x: 1, y: "hi" }
          ⧩
          1

    8 | > rowId { x: 1, y: "hi" }
          ⧩
          {x: 1, y: "hi"}

    9 | > rowId { x: 1 }
          ⧩
          {x: 1}
```

A row can itself be a record, and partial nesting is flattened, so `{ a: Nat | { b: Text | r } }` is
the same type as `{ a: Nat, b: Text | r }`. A closed record means "at least these fields", so
`sumX` accepts a record with extra fields:

``` unison
flat : { a: Nat | { b: Text | rest } } -> { a: Nat, b: Text | rest }
flat r = r

sumX : { x: Nat } -> Nat
sumX r = r@x

> flat { a: 1, b: "hi" }
> sumX { x: 1, y: true }
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + flat : {a: Nat, b: Text | rest} -> {a: Nat, b: Text | rest}
  + sumX : {x: Nat} -> Nat

  Run `update` to apply these changes to your codebase.

    7 | > flat { a: 1, b: "hi" }
          ⧩
          {a: 1, b: "hi"}

    8 | > sumX { x: 1, y: true }
          ⧩
          1
```

A closed record in a result position is exact, so passing a record straight through
requires naming the row:

``` unison
passthrough : { x: Nat | r } -> { x: Nat | r }
passthrough r = r

> passthrough { x: 1, y: "hi" }
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + passthrough : {x: Nat | r} -> {x: Nat | r}

  Run `update` to apply these changes to your codebase.

    4 | > passthrough { x: 1, y: "hi" }
          ⧩
          {x: 1, y: "hi"}
```

## Record-set blocks

A record-set block sets several fields at once. Each value is written against the fields'
*current* values, so they all see the values from before the block rather than one field at a time:

``` unison
setBoth r = r@{f1: @f1 + 1, f2: @f1 + @f2}   -- @f1 is still 1 here, so f2 becomes 3

> setBoth {f1 : 1, f2 : 2}
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + setBoth : {f1: Nat, f2: Nat | x} -> {f1: Nat, f2: Nat | x}

  Run `update` to apply these changes to your codebase.

    3 | > setBoth {f1 : 1, f2 : 2}
          ⧩
          {f1: 2, f2: 3}
```

The name before each colon is the field being set, and `@field` reads a field's current
value — including a field the block does not set. A bare name means whatever it means
outside the block, so reading a field always goes through `@`:

``` unison
seed = 99

fromOuter r = r@{seed: seed + 1}       -- the definition above
fromField r = r@{seed: @seed + 1}      -- the record's own field

addOther r = r@{f1: @f2 + 1}           -- writes f1, reads the f2 it doesn't write
applyTo r = r@{f1: Nat.increment @f2}  -- a space before the `@` applies a function to a read

> fromOuter {seed : 1}
> fromField {seed : 1}
> addOther {f1 : 1, f2 : 10}
> applyTo {f1 : 0, f2 : 41}
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + addOther  : {f1: x1, f2: Nat | x} -> {f1: Nat, f2: Nat | x}
  + applyTo   : {f1: x1, f2: Nat | x} -> {f1: Nat, f2: Nat | x}
  + fromField : {seed: Nat | x} -> {seed: Nat | x}
  + fromOuter : {seed: x1 | x} -> {seed: Nat | x}
  + seed      : Nat

  Run `update` to apply these changes to your codebase.

    9 | > fromOuter {seed : 1}
          ⧩
          {seed: 100}

    10 | > fromField {seed : 1}
           ⧩
           {seed: 2}

    11 | > addOther {f1 : 1, f2 : 10}
           ⧩
           {f1: 11, f2: 10}

    12 | > applyTo {f1 : 0, f2 : 41}
           ⧩
           {f1: 42, f2: 41}
```

A path before the braces sets a field of a nested record, and because a block is a setter it can
change a field's type. The rows a block does not mention stay open, as they do for `@field.set`:

``` unison
setNested r = r@employee@{name: @name ++ "!", salary: @salary + 100}
deep r = r@a@b@{x: @x + 1}
retype r = r@{x: Nat.toText @x}

> setNested {employee : {name : "A", salary : 1}}
> deep {a : {b : {x : 1}}}
> retype {x : 5}
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + deep      : {a: {b: {x: Nat | x} | x1} | x2}
                -> {a: {b: {x: Nat | x} | x1} | x2}
  + retype    : {x: Nat | x} -> {x: Text | x}
  + setNested : {employee: {name: Text, salary: Nat | x} | x1}
                -> { employee: {name: Text, salary: Nat | x} | x1}

  Run `update` to apply these changes to your codebase.

    5 | > setNested {employee : {name : "A", salary : 1}}
          ⧩
          {employee: {name: "A!", salary: 101}}

    6 | > deep {a : {b : {x : 1}}}
          ⧩
          {a: {b: {x: 2}}}

    7 | > retype {x : 5}
          ⧩
          {x: "5"}
```

A block can only set fields the target's type has:

``` unison :error
noSuchField3 : { f1 : Nat } -> { f1 : Nat }
noSuchField3 r = r@{ nope: 1 }
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I expected this record:

      1 | noSuchField3 : { f1 : Nat } -> { f1 : Nat }


  to have the field
    nope: 𝕤6

  so that it would match the type:
    
    {nope: 𝕤6 | _row7}
    

  from here:

      2 | noSuchField3 r = r@{ nope: 1 }
```
