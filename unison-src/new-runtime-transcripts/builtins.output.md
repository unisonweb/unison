# Unit tests for builtin functions

```unison
check : Boolean -> [Result]
check b = if b then [Ok "Passed"] else [Fail "Failed"]

id x = x

checks : [Boolean] -> [Result]
checks bs =
  if all id bs then [Ok "Passed"]
  else [Fail "Failed"]

all : (a ->{m} Boolean) -> [a] ->{m} Boolean
all f = cases
  [] -> true
  h +: t -> f h && all f t

map : (a ->{m} b) -> [a] ->{m} [b]
map f xs =
  go acc = cases
    [] -> acc
    h +: t -> go (acc :+ f h) t
  go [] xs
```

## `Int` functions

```unison
use Int

test> Int.tests.arithmetic =
      checks [
        +1 + +1 `eq` +2,
        +10 - +4 == +6,
        +11 * +6 `eq` +66,
        +11 * +6 `eq` +66,
        +10 / +3 == +3,
        +10 / +5 == +2,
        +10 `mod` +3 == +1,
        +10 `mod` +2 == +0,
        -13 `mod` +3 == +2,
        -13 `mod` -3 == -1,
        -13 `mod` -5 == -3,
        -13 `mod` +5 == +2,
        negate +99 == -99,
        increment +99 == +100,
        not (isEven +99),
        isEven +100,
        isOdd +105,
        not (isOdd +108),
        signum +99 == +1,
        signum -3949 == -1,
        signum +0 == +0,
        +42 `gt` -1,
        +42 `lt` +1000,
        +43 `lteq` +43,
        +43 `lteq` +44,
        +43 `gteq` +43,
        +43 `gteq` +41
        ]

test> Int.tests.bitTwiddling =
      checks [
        +5 `and` +4 == +4,
        +5 `and` +1 == +1,
        +4 `or` +1 == +5,
        +5 `xor` +1 == +4,
        complement -1 == +0,
        popCount +1 == 1,
        popCount +2 == 1,
        popCount +4 == 1,
        popCount +5 == 2,
        popCount -1 == 64,
        leadingZeros +1 == 63,
        trailingZeros +1 == 0,
        leadingZeros +2 == 62,
        trailingZeros +2 == 1,
        pow +2 6 == +64,
        shiftLeft +1 6 == +64,
        shiftRight +64 6 == +1
        ]

test> Int.tests.conversions =
      checks [
        truncate0 -2438344 == 0,
        truncate0 +999 == 999,
        toText +0 == "0",
        toText +10 == "10",
        toText -1039 == "-1039",
        fromText "+0" == Some +0,
        fromText "a8f9djasdlfkj" == None,
        fromText "3940" == Some +3940,
        toFloat +9394 == 9394.0,
        toFloat -20349 == -20349.0
        ]
```

## `Nat` functions

```unison
use Nat

test> Nat.tests.arithmetic =
      checks [
        1 + 1 `eq` 2,
        10 `drop` 4 == 6,
        10 `sub` 12 == -2,
        11 * 6 `eq` 66,
        10 / 3 == 3,
        10 / 5 == 2,
        10 `mod` 3 == 1,
        10 `mod` 2 == 0,
        increment 99 == 100,
        not (isEven 99),
        isEven 100,
        isOdd 105,
        not (isOdd 108),
        42 `gt` 1,
        42 `lt` 1000,
        43 `lteq` 43,
        43 `lteq` 44,
        43 `gteq` 43,
        43 `gteq` 41
        ]

test> Nat.tests.bitTwiddling =
      checks [
        5 `and` 4 == 4,
        5 `and` 1 == 1,
        4 `or` 1 == 5,
        5 `xor` 1 == 4,
        complement (complement 0) == 0,
        popCount 1 == 1,
        popCount 2 == 1,
        popCount 4 == 1,
        popCount 5 == 2,
        popCount (complement 0) == 64,
        leadingZeros 1 == 63,
        trailingZeros 1 == 0,
        leadingZeros 2 == 62,
        trailingZeros 2 == 1,
        pow 2 6 == 64,
        shiftLeft 1 6 == 64,
        shiftRight 64 6 == 1
        ]

test> Nat.tests.conversions =
      checks [
        toFloat 2438344 == 2438344.0,
        toFloat 0 == 0.0,
        toText 0 == "0",
        toText 32939 == "32939",
        toText 10 == "10",
        fromText "ooga" == None,
        fromText "90" == Some 90
        ]
```

## Run the tests

```ucm
.> test

  Cached test results (`help testcache` to learn more)
  
  ◉ Int.tests.arithmetic     Passed
  ◉ Int.tests.bitTwiddling   Passed
  ◉ Int.tests.conversions    Passed
  ◉ Nat.tests.arithmetic     Passed
  ◉ Nat.tests.bitTwiddling   Passed
  ◉ Nat.tests.conversions    Passed
  
  ✅ 6 test(s) passing
  
  Tip: Use view Int.tests.arithmetic to view the source of a
       test.

```
