# Test dependents and dependencies

``` ucm :hide
scratch/main> builtins.merge
```

``` unison :hide
type MyType = A | B

type AnotherType = C | D

myVal = A

myNum = 1

mySum = myNum + 2

myCase = match myVal with
  A -> myNum
  B -> 2
```

``` ucm :hide
scratch/main> update
```

## Dependents

``` ucm
scratch/main> dependents MyType

  Dependents of: type MyType

    Terms:

    1. myCase
    2. myVal

  Tip: Try `view 2` to see the source of any numbered item in
       the above list.

scratch/main> dependents AnotherType

  type AnotherType has no dependents.

scratch/main> dependents myNum

  Dependents of: myNum

    Terms:

    1. myCase
    2. mySum

  Tip: Try `view 2` to see the source of any numbered item in
       the above list.

scratch/main> dependents myVal

  Dependents of: myVal

    Terms:

    1. myCase

  Tip: Try `view 1` to see the source of any numbered item in
       the above list.

scratch/main> dependents A

  Dependents of: A

    Terms:

    1. myCase
    2. myVal

  Tip: Try `view 2` to see the source of any numbered item in
       the above list.

-- For better or worse, we don't have constructor-level granularity yet, so myVal shows here.
scratch/main> dependents B

  Dependents of: B

    Terms:

    1. myCase
    2. myVal

  Tip: Try `view 2` to see the source of any numbered item in
       the above list.
```

## Dependencies

``` ucm
scratch/main> dependencies myCase

  Dependencies of: myCase

    Types:

    1. builtin.Nat
    2. MyType

    Terms:

    3. myNum
    4. myVal

  Tip: Try `view 4` to see the source of any numbered item in
       the above list.

scratch/main> dependencies mySum

  Dependencies of: mySum

    Types:

    1. builtin.Nat

    Terms:

    2. builtin.Nat.+
    3. myNum

  Tip: Try `view 3` to see the source of any numbered item in
       the above list.

scratch/main> dependencies myNum

  Dependencies of: myNum

    Types:

    1. builtin.Nat

  Tip: Try `view 1` to see the source of any numbered item in
       the above list.

scratch/main> dependencies myVal

  Dependencies of: myVal

    Types:

    1. MyType

  Tip: Try `view 1` to see the source of any numbered item in
       the above list.

scratch/main> dependencies MyType

  type MyType has no dependencies.
```
