# Unit tests for builtin functions

```ucm:hide
.> builtins.mergeio
.> cd builtin
.> load unison-src/transcripts-using-base/base.u
.> add
```

This transcript defines unit tests for builtin functions. There's a single `.> test` execution at the end that will fail the transcript with a nice report if any of the tests fail.

## `Int` functions

```unison:hide
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

```ucm:hide
.> add
```

## `Nat` functions

```unison:hide
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

```ucm:hide
.> add
```

## `Boolean` functions
```unison:hide
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

```ucm:hide
.> add
```

## `Text` functions

```unison:hide
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

test> Text.tests.literalsEq = checks [":)" == ":)"]

test> Text.tests.patterns =
  use Pattern many or run isMatch capture join replicate
  use Text.patterns literal digit letter anyChar space punctuation notCharIn charIn charRange notCharRange eof
  l = literal
  checks [
    run digit "1abc" == Some ([], "abc"),
    run (capture (many digit)) "11234abc" == Some (["11234"], "abc"),
    run (many letter) "abc11234abc" == Some ([], "11234abc"),
    run (join [many space, capture (many anyChar)]) "   abc123" == Some (["abc123"], ""),
    run (many punctuation) "!!!!,,,..." == Some ([], ""),
    run (charIn [?0,?1]) "0" == Some ([], ""),
    run (notCharIn [?0,?1]) "0" == None,
    run (many (notCharIn [?0,?1])) "asjdfskdfjlskdjflskdjf011" == Some ([], "011"),
    run (capture (many (charRange ?a ?z))) "hi123" == Some (["hi"], "123"),
    run (capture (many (notCharRange ?, ?,))) "abc123," == Some (["abc123"], ","),
    run (capture (many (notCharIn [?,,]))) "abracadabra,123" == Some (["abracadabra"], ",123"),
    -- this crashes with mismatched foreign calling convention for `Closure`
    run (capture (many (or digit letter))) "11234abc,remainder" == Some (["11234abc"], ",remainder"),
    isMatch (join [l "abra", many (l "cadabra")]) "abracadabracadabra" == true,

  ]
```

```ucm:hide
.> add
```

## `Bytes` functions

```unison:hide
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

```ucm:hide
.> add
```

## `Any` functions

```unison
> [Any "hi", Any (41 + 1)]

test> Any.test1 = checks [(Any "hi" == Any "hi")]
test> Any.test2 = checks [(not (Any "hi" == Any 42))]
```

```ucm:hide
.> add
```

## Sandboxing functions

```unison
openFile1 t = openFile t
openFile2 t = openFile1 t

openFiles =
  [ not (validateSandboxed [] openFile)
  , not (validateSandboxed [] openFile1)
  , not (validateSandboxed [] openFile2)
  ]

test> Sandbox.test1 = checks [validateSandboxed [] "hello"]
test> Sandbox.test2 = checks openFiles
test> Sandbox.test3 = checks [validateSandboxed [termLink openFile.impl]
openFile]
```

```ucm:hide
.> add
```

## Run the tests

Now that all the tests have been added to the codebase, let's view the test report. This will fail the transcript (with a nice message) if any of the tests are failing.

```ucm
.> test
```
