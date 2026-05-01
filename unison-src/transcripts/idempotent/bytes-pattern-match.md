# Pattern matching on Bytes literals

Bytes literals (`0xs...`) can be used as patterns, just like `Text`, `Nat`,
`Int`, `Float`, `Char`, and `Boolean` literals.

``` ucm :hide
scratch/main> builtins.mergeio
```

## Basic Bytes literal patterns

``` unison
classify : Bytes -> Text
classify = cases
  0xs0102 -> "one-two"
  0xsabcd -> "abcd"
  0xs     -> "empty"
  _       -> "other"

> classify 0xs0102
> classify 0xsabcd
> classify 0xs
> classify 0xsff
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + classify : Bytes -> Text

  Run `update` to apply these changes to your codebase.

    8 | > classify 0xs0102
          ⧩
          "one-two"

    9 | > classify 0xsabcd
          ⧩
          "abcd"

    10 | > classify 0xs
           ⧩
           "empty"

    11 | > classify 0xsff
           ⧩
           "other"
```

``` unison
matchBytes : Bytes -> Nat
matchBytes = cases
  0xsdeadbeef -> 1
  0xscafebabe -> 2
  _           -> 0

> matchBytes 0xsdeadbeef
> matchBytes 0xscafebabe
> matchBytes 0xs00
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + matchBytes : Bytes -> Nat

  Run `update` to apply these changes to your codebase.

    7 | > matchBytes 0xsdeadbeef
          ⧩
          1

    8 | > matchBytes 0xscafebabe
          ⧩
          2

    9 | > matchBytes 0xs00
          ⧩
          0
```

## Bytes patterns with underscores

Underscore separators work in pattern Bytes literals just as in
expression Bytes literals.

``` unison
underscoreBytes : Bytes -> Boolean
underscoreBytes = cases
  0xs01_ef -> true
  _        -> false

> underscoreBytes 0xs01ef
> underscoreBytes 0xs01_ef
> underscoreBytes 0xs02ef
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + underscoreBytes : Bytes -> Boolean

  Run `update` to apply these changes to your codebase.

    6 | > underscoreBytes 0xs01ef
          ⧩
          true

    7 | > underscoreBytes 0xs01_ef
          ⧩
          true

    8 | > underscoreBytes 0xs02ef
          ⧩
          false
```
