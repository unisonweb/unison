# Unit tests for builtin functions

This transcript defines unit tests for builtin functions. There's a single `.> test` execution at the end that will fail the transcript with a nice report if any of the tests fail.

## `Int` functions

```unison
use Int

-- Note: you can make the tests more fine-grained if you
-- want to be able to tell which one is failing
test> Int.tests.arithmetic =
      checks [
        eq (+1 + +1) +2,
        +10 - +4 == +6,
        eq (+11 * +6) +66,
        eq (+11 * +6) +66,
        +10 / +3 == +3,
        +10 / +5 == +2,
        mod +10 +3 == +1,
        mod +10 +2 == +0,
        mod -13 +3 == +2,
        mod -13 -3 == -1,
        mod -13 -5 == -3,
        mod -13 +5 == +2,
        negate +99 == -99,
        increment +99 == +100,
        not (isEven +99),
        isEven +100,
        isOdd +105,
        not (isOdd +108),
        signum +99 == +1,
        signum -3949 == -1,
        signum +0 == +0,
        gt +42 -1,
        lt +42 +1000,
        lteq +43 +43,
        lteq +43 +44,
        gteq +43 +43,
        gteq +43 +41
        ]

test> Int.tests.bitTwiddling =
      checks [
        and +5 +4 == +4,
        and +5 +1 == +1,
        or +4 +1 == +5,
        xor +5 +1 == +4,
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
        eq (1 + 1) 2,
        drop 10 4 == 6,
        sub 10 12 == -2,
        eq (11 * 6) 66,
        10 / 3 == 3,
        10 / 5 == 2,
        mod 10 3 == 1,
        mod 10 2 == 0,
        18446744073709551615 / 2 == 9223372036854775807,
        mod 18446744073709551615 2 == 1,
        increment 99 == 100,
        not (isEven 99),
        isEven 100,
        isOdd 105,
        not (isOdd 108),
        gt 42 1,
        lt 42 1000,
        lteq 43 43,
        lteq 43 44,
        gteq 43 43,
        gteq 43 41,
        ]

test> Nat.tests.bitTwiddling =
      checks [
        and 5 4 == 4,
        and 5 1 == 1,
        or 4 1 == 5,
        xor 5 1 == 4,
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
        fromText "90" == Some 90,
        unsnoc "abc" == Some ("ab", ?c),
        uncons "abc" == Some (?a, "bc"),
        unsnoc "" == None,
        uncons "" == None,
        Text.fromCharList (Text.toCharList "abc") == "abc",
        Bytes.fromList (Bytes.toList 0xsACE0BA5E) == 0xsACE0BA5E
        ]
```

## `Boolean` functions
```unison
test> Boolean.tests.orTable =
      checks [
        true || true == true,
        true || false == true,
        false || true == true,
        false || false == false
      ]
test> Boolean.tests.andTable =
      checks [
        true && true == true,
        false && true == false,
        true && false == false,
        false && false == false
      ]
test> Boolean.tests.notTable =
      checks [
        not true == false,
        not false == true
      ]
```

## `Text` functions

```unison
test> Text.tests.takeDropAppend =
      checks [
        "yabba" ++ "dabba" == "yabbadabba",
        Text.take 0 "yabba" == "",
        Text.take 2 "yabba" == "ya",
        Text.take 99 "yabba" == "yabba",
        Text.drop 0 "yabba" == "yabba",
        Text.drop 2 "yabba" == "bba",
        Text.drop 99 "yabba" == ""
        ]

test> Text.tests.repeat =
      checks [
        Text.repeat 4 "o" == "oooo",
        Text.repeat 0 "o" == ""
      ]

test> Text.tests.alignment =
      checks [
        Text.alignLeftWith 5 ?\s "a" == "a    ",
        Text.alignRightWith 5 ?_ "ababa" == "ababa",
        Text.alignRightWith 5 ?_ "ab" == "___ab"
      ]
```

## `Bytes` functions

```unison
test> Bytes.tests.at =
        bs = Bytes.fromList [77, 13, 12]
        checks [
          Bytes.at 1 bs == Some 13,
          Bytes.at 0 bs == Some 77,
          Bytes.at 99 bs == None
        ]

test> Bytes.tests.compression =
        roundTrip b =
          (Bytes.zlib.decompress (Bytes.zlib.compress b) == Right b)
            && (Bytes.gzip.decompress (Bytes.gzip.compress b) == Right b)

        isLeft = cases
          Left _ -> true
          Right _ -> false

        checks [
          roundTrip 0xs2093487509823745709827345789023457892345,
          roundTrip 0xs00000000000000000000000000000000000000000000,
          roundTrip 0xs,
          roundTrip 0xs11111111111111111111111111,
          roundTrip 0xsffffffffffffffffffffffffffffff,
          roundTrip 0xs222222222fffffffffffffffffffffffffffffff,
          -- these fail due to bad checksums and/or headers
          isLeft (zlib.decompress 0xs2093487509823745709827345789023457892345),
          isLeft (gzip.decompress 0xs201209348750982374593939393939709827345789023457892345)
        ]
```

## `Any` functions

```unison
> [Any "hi", Any (41 + 1)]

test> Any.test1 = checks [(Any "hi" == Any "hi")]
test> Any.test2 = checks [(not (Any "hi" == Any 42))]
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      Any.test1 : [Result]
      Any.test2 : [Result]
  
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

    1 | > [Any "hi", Any (41 + 1)]
          ⧩
          [Any "hi", Any 42]
  
    3 | test> Any.test1 = checks [(Any "hi" == Any "hi")]
    
    ✅ Passed Passed
  
    4 | test> Any.test2 = checks [(not (Any "hi" == Any 42))]
    
    ✅ Passed Passed

```
## Run the tests

Now that all the tests have been added to the codebase, let's view the test report. This will fail the transcript (with a nice message) if any of the tests are failing.

```ucm
.> test

  Cached test results (`help testcache` to learn more)
  
  ◉ Any.test1                   Passed
  ◉ Any.test2                   Passed
  ◉ Boolean.tests.andTable      Passed
  ◉ Boolean.tests.notTable      Passed
  ◉ Boolean.tests.orTable       Passed
  ◉ Bytes.tests.at              Passed
  ◉ Bytes.tests.compression     Passed
  ◉ Int.tests.arithmetic        Passed
  ◉ Int.tests.bitTwiddling      Passed
  ◉ Int.tests.conversions       Passed
  ◉ Nat.tests.arithmetic        Passed
  ◉ Nat.tests.bitTwiddling      Passed
  ◉ Nat.tests.conversions       Passed
  ◉ Text.tests.alignment        Passed
  ◉ Text.tests.repeat           Passed
  ◉ Text.tests.takeDropAppend   Passed
  
  ✅ 16 test(s) passing
  
  Tip: Use view Any.test1 to view the source of a test.

```
