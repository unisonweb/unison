# Tests for `moveTo` and `rename`

These commands provide convenient ways to reorganize code:

  - `moveTo` moves items INTO a destination namespace, preserving final name segments
  - `rename` changes only the final segment of a name

``` ucm :hide
scratch/main> builtins.merge
```

## Setup

Create some terms, types, and namespaces to work with:

``` unison
mylib.foo = 1
mylib.bar = 2
mylib.baz.qux = 3
unique type mylib.MyType = A | B
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type mylib.MyType

  + mylib.bar     : Nat
  + mylib.baz.qux : Nat
  + mylib.foo     : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

## `rename` - Change final segment only

The `rename` command changes only the final segment of a name.

### Rename a term

``` ucm
scratch/main> rename mylib.foo myFoo

  Renamed:

    mylib.foo -> mylib.myFoo

scratch/main> ls mylib

  1. MyType  (type)
  2. MyType. (2 terms)
  3. bar     (Nat)
  4. baz.    (1 term)
  5. myFoo   (Nat)
```

### Rename a type

``` ucm
scratch/main> rename mylib.MyType RenamedType

  Renamed:

    mylib.MyType -> mylib.RenamedType

scratch/main> ls mylib

  1. RenamedType  (type)
  2. RenamedType. (2 terms)
  3. bar          (Nat)
  4. baz.         (1 term)
  5. myFoo        (Nat)
```

### Rename a namespace

``` ucm
scratch/main> rename mylib.baz stuff

  Renamed:

    mylib.baz -> mylib.stuff

scratch/main> ls mylib

  1. RenamedType  (type)
  2. RenamedType. (2 terms)
  3. bar          (Nat)
  4. myFoo        (Nat)
  5. stuff.       (1 term)
```

### Error case - source doesn't exist

``` ucm :error
scratch/main> rename nonexistent.thing NewName

  ⚠️

  There is no term, type, or namespace at nonexistent.thing.
```

## `moveTo` - Move items into a destination namespace

The `moveTo` command moves items INTO a destination namespace, preserving the final segment.

### Setup for moveTo tests

``` ucm
scratch/other> builtins.merge

  Done.
```

``` unison
alpha.one = 1
alpha.two = 2
beta.three = 3
unique type gamma.T = T
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type gamma.T

  + alpha.one  : Nat
  + alpha.two  : Nat
  + beta.three : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/other> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

### Move a single item into a namespace

`moveTo alpha.one dest` moves `alpha.one` to `dest.one`:

``` ucm
scratch/other> moveTo alpha.one dest

  Moved:

    alpha.one -> dest.one

scratch/other> ls dest

  1. one (Nat)

scratch/other> ls alpha

  1. two (Nat)
```

### Move multiple items into a namespace

`moveTo alpha.two beta.three newplace` moves both into `newplace`:

``` ucm
scratch/other> moveTo alpha.two beta.three newplace

  Moved:

    beta.three -> newplace.three
    alpha.two  -> newplace.two

scratch/other> ls newplace

  1. three (Nat)
  2. two   (Nat)
```

### Move a type

``` ucm
scratch/other> moveTo gamma.T types

  Moved:

    gamma.T -> types.T

scratch/other> ls types

  1. T  (type)
  2. T. (1 term)
```

### Move a namespace

``` unison
stuff.inner.x = 100
stuff.inner.y = 200
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + stuff.inner.x : Nat
  + stuff.inner.y : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/other> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/other> moveTo stuff.inner organized

  Moved:

    stuff.inner -> organized.inner

scratch/other> ls organized

  1. inner. (2 terms)

scratch/other> ls organized.inner

  1. x (Nat)
  2. y (Nat)
```

### Move items to the top level

`moveTo` can move items to the root namespace using `.` as the destination:

``` ucm
scratch/other> moveTo dest.one organized.inner .

  Moved:

    organized.inner -> .inner
    dest.one        -> .one

scratch/other> ls

  1. builtin.  (681 terms, 107 types)
  2. inner.    (2 terms)
  3. newplace. (2 terms)
  4. one       (Nat)
  5. types.    (1 term, 1 type)
```

### Error case - source doesn't exist

``` ucm :error
scratch/other> moveTo nonexistent.thing somewhere

  ⚠️

  There is no term, type, or namespace at nonexistent.thing.
```

## `moveTo` with naming conflicts

When multiple sources have the same final segment, `moveTo` moves non-conflicting
items and reports the conflicts.

``` ucm
scratch/conflict> builtins.merge

  Done.
```

``` unison
a.item = 1
b.item = 2
c.other = 3
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + a.item  : Nat
  + b.item  : Nat
  + c.other : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/conflict> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

Moving `a.item`, `b.item`, and `c.other` to `target` - `item` conflicts:

``` ucm :error
scratch/conflict> moveTo a.item b.item c.other target

  Moved:

    c.other -> target.other

  ⚠️

  I couldn't move some of the items, because they have the same
  final segment as some of the others, meaning that they would
  have duplicate names at the destination:

    1. b.item
    2. a.item

  You can rename them and then use `moveTo` again, for example:

    `rename 2 <newName>`

    `moveTo 1 a.<newName> target`
```

The non-conflicting item (`c.other`) should have been moved:

``` ucm
scratch/conflict> ls target

  1. other (Nat)
```

After using `rename` to resolve the conflict:

``` ucm
scratch/conflict> rename a.item itemA

  Renamed:

    a.item -> a.itemA

scratch/conflict> rename b.item itemB

  Renamed:

    b.item -> b.itemB

scratch/conflict> moveTo a.itemA b.itemB target

  Moved:

    a.itemA -> target.itemA
    b.itemB -> target.itemB

scratch/conflict> ls target

  1. itemA (Nat)
  2. itemB (Nat)
  3. other (Nat)
```
