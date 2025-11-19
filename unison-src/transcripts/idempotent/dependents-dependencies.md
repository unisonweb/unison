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

``` ucm :hide
scratch/main> project.delete scratch
```

`dependents` has limited support for reporting dependents of things defined in the scratch file. At present,
`dependents foo` will simply fall back on reporting on dependents of the things suffixed `foo` in the latest typechecked
Unison file only when no things with suffix `foo` are found in the codebase.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtin
```

``` unison
foo = 17
bar = foo + foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + bar : Nat
  + foo : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

This demonstrates that when `foo` is found in the codebase, its codebase dependents are reported, regardless if new or
different dependents are staged in the scratch file.

``` unison
baz = foo + foo + foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + baz : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> dependents foo

  Dependents of: foo

    Terms:

    1. bar

  Tip: Try `view 1` to see the source of any numbered item in
       the above list.
```

This demonstrates that when `foo` is found in the codebase *and* is changed in the scratch file, same thing (but we
do call out that we are reporting dependents of the codebase version).

``` unison
foo = 18
baz = foo + foo + foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + baz : Nat
  ~ foo : Nat

  + (added), ~ (modified)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> dependents foo

  Dependents of: foo (in codebase)

    Terms:

    1. bar

  Tip: Try `view 1` to see the source of any numbered item in
       the above list.
```

This demonstrates falling back on the scratch file when the symbol isn't found in the codebase.

``` unison
baz = foo + foo + foo
qux = baz + baz
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + baz : Nat
  + qux : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> dependents baz

  Dependents of: baz (in file)

    Terms:

    1. qux

  Tip: Try `view 1` to see the source of any numbered item in
       the above list.
```

``` ucm :hide
scratch/main> project.delete scratch
```
