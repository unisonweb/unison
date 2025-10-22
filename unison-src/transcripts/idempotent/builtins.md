# Unit tests for builtin functions

``` ucm :hide
> builtins.mergeio

> load unison-src/transcripts-using-base/base.u

> add
```

This transcript defines unit tests for builtin functions. There's a single `scratch/main> test` execution at the end that will fail the transcript with a nice report if any of the tests fail.

## `Int` functions

``` unison :hide
use Int

-- used for some take/drop tests later
bigN = Nat.shiftLeft 1 63

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
        fromText "1000000000000000000000000000" == None,
        fromText "-1000000000000000000000000000" == None,
        toFloat +9394 == 9394.0,
        toFloat -20349 == -20349.0
        ]
```

``` ucm :hide
> add
```

## `Nat` functions

``` unison :hide
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
        fromText "-1" == None,
        fromText "100000000000000000000000000" == None,
        unsnoc "abc" == Some ("ab", ?c),
        uncons "abc" == Some (?a, "bc"),
        unsnoc "" == None,
        uncons "" == None,
        Text.fromCharList (Text.toCharList "abc") == "abc",
        Bytes.fromList (Bytes.toList 0xsACE0BA5E) == 0xsACE0BA5E
        ]
```

``` ucm :hide
> add
```

## `Natural` functions (arbitrary-precision natural numbers)

``` unison :hide
use Natural eq

test> Natural.tests.arithmetic =
      checks [
        Natural.eq (Natural.add (Natural.fromText "123456789012345678901234567890" |> Optional.getOrBug "Invalid natural") (Natural.fromText "987654321098765432109876543210" |> Optional.getOrBug "Invalid natural")) (Natural.fromText "1111111110111111111011111111100" |> Optional.getOrBug "Invalid natural"),
        eq (Natural.sub (Natural.fromNat 1000) (Natural.fromNat 500)) (Natural.fromNat 500),
        eq (Natural.mul (Natural.fromNat 123456789) (Natural.fromNat 987654321)) (Natural.fromText "121932631112635269" |> Optional.getOrBug "Invalid natural"),
        eq (Natural.div (Natural.fromNat 1000) (Natural.fromNat 3)) (Natural.fromNat 333),
        eq (Natural.mod (Natural.fromNat 1000) (Natural.fromNat 3)) (Natural.fromNat 1),
        eq (Natural.pow (Natural.fromNat 2) (Natural.fromNat 64)) (Natural.fromText "18446744073709551616" |> Optional.getOrBug "Invalid natural"),
        Natural.gt (Natural.fromNat 1000) (Natural.fromNat 500),
        Natural.lt (Natural.fromNat 500) (Natural.fromNat 1000),
        Natural.lteq (Natural.fromNat 500) (Natural.fromNat 500),
        Natural.lteq (Natural.fromNat 500) (Natural.fromNat 1000),
        Natural.gteq (Natural.fromNat 1000) (Natural.fromNat 500),
        Natural.gteq (Natural.fromNat 1000) (Natural.fromNat 1000),
        Natural.eq (Natural.fromNat 1000) (Natural.fromNat 1000),
        not (Natural.eq (Natural.fromNat 1000) (Natural.fromNat 999))
        ]

test> Natural.tests.bitwise =
      checks [
        eq (Natural.and (Natural.fromNat 5) (Natural.fromNat 4)) (Natural.fromNat 4),
        eq (Natural.and (Natural.fromNat 5) (Natural.fromNat 1)) (Natural.fromNat 1),
        eq (Natural.or (Natural.fromNat 4) (Natural.fromNat 1)) (Natural.fromNat 5),
        eq (Natural.xor (Natural.fromNat 5) (Natural.fromNat 1)) (Natural.fromNat 4),
        Natural.popCount (Natural.fromNat 1) Universal.== 1,
        Natural.popCount (Natural.fromNat 2) Universal.== 1,
        Natural.popCount (Natural.fromNat 4) Universal.== 1,
        Natural.popCount (Natural.fromNat 5) Universal.== 2,
        eq (Natural.shiftLeft (Natural.fromNat 1) 6) (Natural.fromNat 64),
        eq (Natural.shiftRight (Natural.fromNat 64) 6) (Natural.fromNat 1)
        ]

test> Natural.tests.conversions =
      checks [
        isSome (Natural.fromText "123456789012345678901234567890"),
        Natural.fromText "0" Universal.== Some (Natural.fromNat 0),
        Natural.fromText "invalid" Universal.== None,
        Natural.fromText "-1" Universal.== None,
        Natural.toText (Natural.fromNat 0) Universal.== "0",
        Natural.toText (Natural.fromText "123456789" |> Optional.getOrBug "Invalid natural") Universal.== "123456789",
        Natural.toFloat (Natural.fromText "123456789" |> Optional.getOrBug "Invalid natural") Universal.== 123456789.0,
        Natural.toFloat (Natural.fromNat 0) Universal.== 0.0
        ]

test> Natural.tests.parity =
      checks [
        not (Natural.isEven (Natural.fromNat 99)),
        Natural.isEven (Natural.fromNat 100),
        Natural.isOdd (Natural.fromNat 105),
        not (Natural.isOdd (Natural.fromNat 108))
        ]
```

``` ucm :hide
> add
```

## `Integer` functions (arbitrary-precision integers)

``` unison :hide
use Integer eq

test> Integer.tests.arithmetic =
      checks [
          Integer.eq (Integer.add (Integer.fromText "123456789012345678901234567890" |> Optional.getOrBug "Invalid integer") (Integer.fromText "987654321098765432109876543210" |> Optional.getOrBug "Invalid integer")) (Integer.fromText "1111111110111111111011111111100" |> Optional.getOrBug "Invalid integer"),
        eq (Integer.sub (Integer.fromInt +1000) (Integer.fromInt +500)) (Integer.fromInt +500),
        eq (Integer.mul (Integer.fromInt +123456789) (Integer.fromInt +987654321)) (Integer.fromText "121932631112635269" |> Optional.getOrBug "Invalid integer"),
        eq (Integer.div (Integer.fromInt +1000) (Integer.fromInt +3)) (Integer.fromInt +333),
        eq (Integer.mod (Integer.fromInt +1000) (Integer.fromInt +3)) (Integer.fromInt +1),
        eq (Integer.pow (Integer.fromInt +2) (Integer.fromInt +64)) (Integer.fromText "18446744073709551616" |> Optional.getOrBug "Invalid integer"),
        Integer.gt (Integer.fromInt +1000) (Integer.fromInt +500),
        Integer.lt (Integer.fromInt +500) (Integer.fromInt +1000),
        Integer.lteq (Integer.fromInt +500) (Integer.fromInt +500),
        Integer.lteq (Integer.fromInt +500) (Integer.fromInt +1000),
        Integer.gteq (Integer.fromInt +1000) (Integer.fromInt +500),
        Integer.gteq (Integer.fromInt +1000) (Integer.fromInt +1000),
        Integer.eq (Integer.fromInt +1000) (Integer.fromInt +1000),
        not (Integer.eq (Integer.fromInt +1000) (Integer.fromInt +999)),
        eq (Integer.abs (Integer.fromInt +1000)) (Integer.fromInt +1000),
        eq (Integer.abs (Integer.fromInt -1000)) (Integer.fromInt +1000),
        not (Int.eq (Integer.signum (Integer.fromInt +1)) (Integer.signum (Integer.fromInt -1)))
        ]

test> Integer.tests.bitwise =
      checks [
        eq (Integer.and (Integer.fromInt +5) (Integer.fromInt +4)) (Integer.fromInt +4),
        eq (Integer.and (Integer.fromInt +5) (Integer.fromInt +1)) (Integer.fromInt +1),
        eq (Integer.or (Integer.fromInt +4) (Integer.fromInt +1)) (Integer.fromInt +5),
        eq (Integer.xor (Integer.fromInt +5) (Integer.fromInt +1)) (Integer.fromInt +4),
        Integer.popCount (Integer.fromInt +1) Universal.== 1,
        Integer.popCount (Integer.fromInt +2) Universal.== 1,
        Integer.popCount (Integer.fromInt +4) Universal.== 1,
        Integer.popCount (Integer.fromInt +5) Universal.== 2,
        eq (Integer.shiftLeft (Integer.fromInt +1) 6) (Integer.fromInt +64),
        eq (Integer.shiftRight (Integer.fromInt +64) 6) (Integer.fromInt +1)
        ]

test> Integer.tests.conversions =
      checks [
        isSome (Integer.fromText "123456789012345678901234567890"),
        Integer.fromText "0" Universal.== Some (Integer.fromInt +0),
        Integer.fromText "invalid" Universal.== None,
        Integer.fromText "-1" Universal.== Some (Integer.fromInt -1),
        Integer.toText (Integer.fromInt +0) Universal.== "0",
        Integer.toText (Integer.fromText "123456789" |> Optional.getOrBug "Invalid integer") Universal.== "123456789",
        Integer.toFloat (Integer.fromText "123456789" |> Optional.getOrBug "Invalid integer") Universal.== 123456789.0,
        Integer.toFloat (Integer.fromInt +0) Universal.== 0.0
        ]

test> Integer.tests.parity =
      checks [
        not (Integer.isEven (Integer.fromInt +99)),
        Integer.isEven (Integer.fromInt +100),
        Integer.isOdd (Integer.fromInt +105),
        not (Integer.isOdd (Integer.fromInt +108))
        ]
```

``` ucm :hide
> add
```

## `Boolean` functions

``` unison :hide
test> Boolean.tests.orTable =
      checks [
        (true || true) == true,
        (true || false) == true,
        (false || true) == true,
        (false || false) == false
      ]
test> Boolean.tests.andTable =
      checks [
        (true && true) == true,
        (false && true) == false,
        (true && false) == false,
        (false && false) == false
      ]
test> Boolean.tests.notTable =
      checks [
        not true == false,
        not false == true
      ]
```

``` ucm :hide
> add
```

## `Text` functions

``` unison :hide
test> Text.tests.takeDropAppend =
      checks [
        "yabba" ++ "dabba" == "yabbadabba",
        Text.take 0 "yabba" == "",
        Text.take 2 "yabba" == "ya",
        Text.take 99 "yabba" == "yabba",
        Text.drop 0 "yabba" == "yabba",
        Text.drop 2 "yabba" == "bba",
        Text.drop 99 "yabba" == "",
        Text.take bigN "yabba" == "yabba",
        Text.drop bigN "yabba" == ""
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
  use Text.patterns literal digit letter anyChar space punctuation notCharIn charIn charRange notCharRange eof lookbehind1
  use Char.Class any number not
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
    run (capture (many (or digit letter))) "11234abc,remainder" == Some (["11234abc"], ",remainder"),
    run (capture (replicate 1 5 (or digit letter))) "1a2ba aaa" == Some (["1a2ba"], " aaa"),
    run (captureAs "foo" (many (or digit letter))) "11234abc,remainder" == Some (["foo"], ",remainder"),
    run (join [(captureAs "foo" (many digit)), captureAs "bar" (many letter)]) "11234abc,remainder" == Some (["foo", "bar"], ",remainder"),
    -- Regression test for: https://github.com/unisonweb/unison/issues/3530
    run (capture (replicate 0 1 (join [literal "a", literal "b"]))) "ac" == Some ([""], "ac"),
    isMatch (join [many letter, eof]) "aaaaabbbb" == true,
    isMatch (join [many letter, eof]) "aaaaabbbb1" == false,
    isMatch (join [l "abra", many (l "cadabra")]) "abracadabracadabra" == true,
    run (Pattern.many.corrected (join [negativeLookahead (literal "GO STOP"), literal "GO "])) "GO GO GO GO STOP GO GO" == Some ([], "GO STOP GO GO"),
    run (Pattern.many.corrected (join [literal "GO ", lookahead (literal "GO")])) "GO GO GO GO STOP GO GO" == Some ([], "GO STOP GO GO"),
    run (Pattern.many.corrected (join [negativeLookbehind number, char any])) "abddc1234" == Some ([], "234"),
  ]


test> Text.tests.indexOf =
   haystack = "01020304" ++ "05060708" ++ "090a0b0c01"
   needle1 = "01"
   needle2 = "02"
   needle3 = "0304"
   needle4 = "05"
   needle5 = "0405"
   needle6 = "0c"
   needle7 = haystack
   needle8 = "lopez"
   needle9 = ""
   checks [
     Text.indexOf needle1 haystack == Some 0,
     Text.indexOf needle2 haystack == Some 2,
     Text.indexOf needle3 haystack == Some 4,
     Text.indexOf needle4 haystack == Some 8,
     Text.indexOf needle5 haystack == Some 6,
     Text.indexOf needle6 haystack == Some 22,
     Text.indexOf needle7 haystack == Some 0,
     Text.indexOf needle8 haystack == None,
     Text.indexOf needle9 haystack == Some 0,
   ]

test> Text.tests.indexOfEmoji =
  haystack = "clap 👏 your 👏 hands 👏 if 👏 you 👏 love 👏 unison"
  needle1 = "👏"
  needle2 = "👏 "
  checks [
    Text.indexOf needle1 haystack == Some 5,
    Text.indexOf needle2 haystack == Some 5,
  ]

```

``` ucm :hide
> add
```

## `Bytes` functions

``` unison :hide
test> Bytes.tests.at =
        bs = Bytes.fromList [77, 13, 12]
        checks [
          Bytes.at 1 bs == Some 13,
          Bytes.at 0 bs == Some 77,
          Bytes.at 99 bs == None,
          Bytes.take bigN bs == bs,
          Bytes.drop bigN bs == empty
        ]

test> Bytes.tests.compression =
        roundTrip b =
          (Bytes.zlib.decompress (Bytes.zlib.compress b) == Right b)
            && (Bytes.gzip.decompress (Bytes.gzip.compress b) == Right b)
            && (Bytes.zstd.decompress (Bytes.zstd.compress +3 b) == Right b)

        checks [
          roundTrip 0xs2093487509823745709827345789023457892345,
          roundTrip 0xs00000000000000000000000000000000000000000000,
          roundTrip 0xs,
          roundTrip 0xs11111111111111111111111111,
          roundTrip 0xsffffffffffffffffffffffffffffff,
          roundTrip 0xs222222222fffffffffffffffffffffffffffffff,
          -- these fail due to bad checksums and/or headers
          isLeft (zlib.decompress 0xs2093487509823745709827345789023457892345),
          isLeft (gzip.decompress 0xs201209348750982374593939393939709827345789023457892345),
          isLeft (zstd.decompress 0xs28b52ffd20ffffffffffffffff)
        ]

test> Bytes.tests.fromBase64UrlUnpadded =
  checks [Exception.catch
           '(fromUtf8
              (raiseMessage () (Bytes.fromBase64UrlUnpadded (toUtf8 "aGVsbG8gd29ybGQ")))) == Right "hello world"
         , isLeft (Bytes.fromBase64UrlUnpadded (toUtf8 "aGVsbG8gd29ybGQ="))]

test> Bytes.tests.indexOf =
   haystack = 0xs01020304 ++ 0xs05060708 ++ 0xs090a0b0c01
   needle1 = 0xs01
   needle2 = 0xs02
   needle3 = 0xs0304
   needle4 = 0xs05
   needle5 = 0xs0405
   needle6 = 0xs0c
   needle7 = haystack
   needle8 = 0xsffffff
   checks [
     Bytes.indexOf needle1 haystack == Some 0,
     Bytes.indexOf needle2 haystack == Some 1,
     Bytes.indexOf needle3 haystack == Some 2,
     Bytes.indexOf needle4 haystack == Some 4,
     Bytes.indexOf needle5 haystack == Some 3,
     Bytes.indexOf needle6 haystack == Some 11,
     Bytes.indexOf needle7 haystack == Some 0,
     Bytes.indexOf needle8 haystack == None,

   ]

test> Bytes.tests.byteArray =
  bs = 0xs0102030405
  checks [
    ImmutableByteArray.toBytes (ImmutableByteArray.fromBytes bs) 0 5 == bs
  ]

```

``` ucm :hide
> add
```

## `List` comparison

``` unison :hide
test> checks [
        compare [] [1,2,3] == -1,
        compare [1,2,3] [1,2,3,4] == -1,
        compare [1,2,3,4] [1,2,3] == +1,
        compare [1,2,3] [1,2,3] == +0,
        compare [3] [1,2,3] == +1,
        compare [1,2,3] [1,2,4] == -1,
        compare [1,2,2] [1,2,1,2] == +1,
        compare [1,2,3,4] [3,2,1] == -1
      ]
```

``` ucm :hide
> add
```

Other list functions

``` unison :hide
test> checks [
        List.take bigN [1,2,3] == [1,2,3],
        List.drop bigN [1,2,3] == []
      ]
```

## `Any` functions

``` unison
> [Any "hi", Any (41 + 1)]

test> Any.test1 = checks [(Any "hi" == Any "hi")]
test> Any.test2 = checks [(not (Any "hi" == Any 42))]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + Any.test1 : [Result]
  + Any.test2 : [Result]

  Run `update` to apply these changes to your codebase.

    1 | > [Any "hi", Any (41 + 1)]
          ⧩
          [Any "hi", Any 42]

    3 | test> Any.test1 = checks [(Any "hi" == Any "hi")]
    
    ✅ Passed Passed

    4 | test> Any.test2 = checks [(not (Any "hi" == Any 42))]
    
    ✅ Passed Passed
```

``` ucm :hide
> add
```

## Sandboxing functions

``` unison
openFile1 t = openFile t
openFile2 t = openFile1 t

validateSandboxedSimpl ok v =
  match Value.validateSandboxed ok v with
    Right [] -> true
    _ -> false

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

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + openFile1              : Text
                             -> FileMode
                             ->{IO, Exception} Handle
  + openFile2              : Text
                             -> FileMode
                             ->{IO, Exception} Handle
  + openFiles              : [Boolean]
  + Sandbox.test1          : [Result]
  + Sandbox.test2          : [Result]
  + Sandbox.test3          : [Result]
  + validateSandboxedSimpl : [Link.Term] -> Value ->{IO} Boolean

  Run `update` to apply these changes to your codebase.

    15 | test> Sandbox.test1 = checks [validateSandboxed [] "hello"]
    
    ✅ Passed Passed

    16 | test> Sandbox.test2 = checks openFiles
    
    ✅ Passed Passed

    17 | test> Sandbox.test3 = checks [validateSandboxed [termLink openFile.impl]
    
    ✅ Passed Passed
```

``` ucm :hide
> add
```

``` unison
openFilesIO = do
  checks
    [ not (validateSandboxedSimpl [] (value openFile))
    , not (validateSandboxedSimpl [] (value openFile1))
    , not (validateSandboxedSimpl [] (value openFile2))
    , sandboxLinks (termLink openFile)
        == sandboxLinks (termLink openFile1)
    , sandboxLinks (termLink openFile1)
        == sandboxLinks (termLink openFile2)
    ]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + openFilesIO : '{IO} [Result]

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

> io.test openFilesIO

    New test results:

    1. openFilesIO   ◉ Passed

  ✅ 1 test(s) passing

  Tip: Use view 1 to view the source of a test.
```

## Universal hash functions

Just exercises the function

``` unison
> Universal.murmurHash 1
test> Universal.murmurHash.tests = checks [Universal.murmurHash [1,2,3] == Universal.murmurHash [1,2,3]]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + Universal.murmurHash.tests : [Result]

  Run `update` to apply these changes to your codebase.

    1 | > Universal.murmurHash 1
          ⧩
          1208954131003843843

    2 | test> Universal.murmurHash.tests = checks [Universal.murmurHash [1,2,3] == Universal.murmurHash [1,2,3]]
    
    ✅ Passed Passed
```

``` ucm :hide
> add
```

## Pinned Array and Mutable Byte Array tests

Test the pinned array and mutable byte array functionality

``` unison
-- Test PinnedArray.cast functionality with real operations
PinnedByteArray.tests.cast.operations = do
  -- Create a pinned array using IO.pinnedByteArray
  pinned = IO.pinnedByteArray 10

  -- Cast the pinned array to a mutable byte array
  mutable = PinnedByteArray.cast pinned

  -- Write some test data to the mutable array
  write8 mutable 0 42
  write8 mutable 1 123
  write8 mutable 2 255
  write16be mutable 3 12345
  write32be mutable 5 987654321

  -- Read the data back and verify it's correct
  read8_0 = read8 mutable 0
  read8_1 = read8 mutable 1
  read8_2 = read8 mutable 2
  read16be_3 = read16be mutable 3
  read32be_5 = read32be mutable 5

  -- Verify all the values are correct
  checks [
    read8_0 == 42,
    read8_1 == 123,
    read8_2 == 255,
    read16be_3 == 12345,
    read32be_5 == 987654321
  ]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + PinnedByteArray.tests.cast.operations : '{IO, Exception} [Result]

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

> io.test PinnedByteArray.tests.cast.operations

    New test results:

    1. operations   ◉ Passed

  ✅ 1 test(s) passing

  Tip: Use view 1 to view the source of a test.
```

## Test comprehensive byte array operations

Now let's add comprehensive tests for all the byte array read/write functions

``` unison
-- Test all byte array read/write operations
ByteArray.tests.allOperations = do
  mutable = IO.bytearray 100

  -- Write test data using all available functions
  write8 mutable 0 0x12
  write8 mutable 1 0x34
  write16be mutable 2 0x5678
  write16le mutable 4 0x5678
  -- For 24-bit values, we need to write them manually since there's no write24be/write24le
  write16be mutable 6 0x1234
  write8 mutable 8 0x56

  write16le mutable 9 0x3456
  write8 mutable 11 0x12

  write32be mutable 12 0x12345678
  write32le mutable 16 0x12345678
  write64be mutable 20 0x0123456789abcdef
  write64le mutable 28 0x0123456789abcdef

  -- Read the data back and verify it's correct
  read8_0 = read8 mutable 0
  read8_1 = read8 mutable 1
  read16be_2 = read16be mutable 2
  read16le_4 = read16le mutable 4
  read24be_6 = read24be mutable 6
  read24le_9 = read24le mutable 9
  read32be_12 = read32be mutable 12
  read32le_16 = read32le mutable 16
  read64be_20 = read64be mutable 20
  read64le_28 = read64le mutable 28

  -- Read 40-bit value
  read40be_20 = read40be mutable 20
  read40le_20 = read40le mutable 20

  -- Read in reverse byte order
  read16le_2 = read16le mutable 2
  read16be_4 = read16be mutable 4
  read32le_12 = read32le mutable 12
  read32be_16 = read32be mutable 16
  read64le_20 = read64le mutable 20
  read64be_28 = read64be mutable 28

  -- Verify all the values are correct
  checks [
    read8_0 == 0x12,
    read8_1 == 0x34,
    read16be_2 == 0x5678,
    read16le_4 == 0x5678,
    read24be_6 == 0x123456,
    read24le_9 == 0x123456,
    read32be_12 == 0x12345678,
    read32le_16 == 0x12345678,
    read64be_20 == 0x0123456789abcdef,
    read64le_28 == 0x0123456789abcdef,
    read40be_20 == 0x0123456789,
    read40le_20 == 0x8967452301,
    read16le_2 == 0x7856,
    read16be_4 == 0x7856,
    read32le_12 == 0x78563412,
    read32be_16 == 0x78563412,
    read64le_20 == 0xefcdab8967452301,
    read64be_28 == 0xefcdab8967452301
  ]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + ByteArray.tests.allOperations : '{IO, Exception} [Result]

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

> io.test ByteArray.tests.allOperations

    New test results:

    1. allOperations   ◉ Passed

  ✅ 1 test(s) passing

  Tip: Use view 1 to view the source of a test.
```

## Run the tests

Now that all the tests have been added to the codebase, let's view the test report. This will fail the transcript (with a nice message) if any of the tests are failing.

``` ucm
> test

  Cached test results (`help testcache` to learn more)

    1.  Any.test1                           ◉ Passed
    2.  Any.test2                           ◉ Passed
    3.  Boolean.tests.andTable              ◉ Passed
    4.  Boolean.tests.notTable              ◉ Passed
    5.  Boolean.tests.orTable               ◉ Passed
    6.  Bytes.tests.at                      ◉ Passed
    7.  Bytes.tests.byteArray               ◉ Passed
    8.  Bytes.tests.compression             ◉ Passed
    9.  Bytes.tests.fromBase64UrlUnpadded   ◉ Passed
    10. Bytes.tests.indexOf                 ◉ Passed
    11. Int.tests.arithmetic                ◉ Passed
    12. Int.tests.bitTwiddling              ◉ Passed
    13. Int.tests.conversions               ◉ Passed
    14. Integer.tests.arithmetic            ◉ Passed
    15. Integer.tests.bitwise               ◉ Passed
    16. Integer.tests.conversions           ◉ Passed
    17. Integer.tests.parity                ◉ Passed
    18. Nat.tests.arithmetic                ◉ Passed
    19. Nat.tests.bitTwiddling              ◉ Passed
    20. Nat.tests.conversions               ◉ Passed
    21. Natural.tests.arithmetic            ◉ Passed
    22. Natural.tests.bitwise               ◉ Passed
    23. Natural.tests.conversions           ◉ Passed
    24. Natural.tests.parity                ◉ Passed
    25. Sandbox.test1                       ◉ Passed
    26. Sandbox.test2                       ◉ Passed
    27. Sandbox.test3                       ◉ Passed
    28. test.ebobca6b0t                     ◉ Passed
    29. Text.tests.alignment                ◉ Passed
    30. Text.tests.indexOf                  ◉ Passed
    31. Text.tests.indexOfEmoji             ◉ Passed
    32. Text.tests.literalsEq               ◉ Passed
    33. Text.tests.patterns                 ◉ Passed
    34. Text.tests.repeat                   ◉ Passed
    35. Text.tests.takeDropAppend           ◉ Passed
    36. Universal.murmurHash.tests          ◉ Passed

  ✅ 36 test(s) passing

  Tip: Use view 1 to view the source of a test.
```
