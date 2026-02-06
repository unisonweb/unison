# Integer and Natural value serialization

Tests that `Integer` and `Natural` values can be serialized and deserialized correctly.

``` ucm :hide
> builtins.mergeio lib.builtins
```

``` unison
checks : [Boolean] -> [Result]
checks bs =
  List.map (b -> if b then Ok "passed" else Fail "failed") bs

roundtrip : a -> {IO, Exception} Boolean
roundtrip x =
  use Value serialize deserialize value load
  v = value x
  bytes = serialize v
  match deserialize bytes with
    Left _ -> false
    Right v' -> match load v' with
      Left _ -> false
      Right x' -> x == x'

getOrBug : Text -> Optional a -> a
getOrBug msg = cases
  Some a -> a
  None -> bug msg

-- Round-trip test for Integer serialization
Integer.serialization.roundtrip : '{IO, Exception} [Result]
Integer.serialization.roundtrip = do
  -- Small positive integer
  i1 = Integer.fromInt +42
  -- Small negative integer
  i2 = Integer.fromInt -42
  -- Large positive integer (larger than Int64)
  i3 = getOrBug "parse" (Integer.fromText "123456789012345678901234567890")
  -- Large negative integer
  i4 = getOrBug "parse" (Integer.fromText "-123456789012345678901234567890")
  -- Zero
  i5 = Integer.fromInt +0

  checks [
    roundtrip i1,
    roundtrip i2,
    roundtrip i3,
    roundtrip i4,
    roundtrip i5
  ]

-- Round-trip test for Natural serialization
Natural.serialization.roundtrip : '{IO, Exception} [Result]
Natural.serialization.roundtrip = do
  -- Small natural
  n1 = getOrBug "parse" (Natural.fromText "42")
  -- Large natural (larger than Word64)
  n2 = getOrBug "parse" (Natural.fromText "123456789012345678901234567890")
  -- Zero
  n3 = getOrBug "parse" (Natural.fromText "0")
  -- Exactly 2^64 (first value that doesn't fit in Word64)
  n4 = getOrBug "parse" (Natural.fromText "18446744073709551616")

  checks [
    roundtrip n1,
    roundtrip n2,
    roundtrip n3,
    roundtrip n4
  ]

-- Test that Integer/Natural inside other structures serialize correctly
nested.serialization.roundtrip : '{IO, Exception} [Result]
nested.serialization.roundtrip = do
  i = getOrBug "parse" (Integer.fromText "999999999999999999999")
  n = getOrBug "parse" (Natural.fromText "888888888888888888888")

  -- Tuple containing Integer and Natural
  pair = (i, n)
  -- List of Integers
  intList = [Integer.fromInt +1, Integer.fromInt -1, i]
  -- Optional Integer
  optInt = Some i

  checks [
    roundtrip pair,
    roundtrip intList,
    roundtrip optInt
  ]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + checks                          : [Boolean] -> [Result]
  + getOrBug                        : Text -> Optional a -> a
  + Integer.serialization.roundtrip : '{IO, Exception} [Result]
  + Natural.serialization.roundtrip : '{IO, Exception} [Result]
  + nested.serialization.roundtrip  : '{IO, Exception} [Result]
  + roundtrip                       : a
                                      ->{IO, Exception} Boolean

  Run `update` to apply these changes to your codebase.
```

``` ucm :hide
> add
```

``` ucm
> io.test Integer.serialization.roundtrip

    New test results:

    1. Integer.serialization.roundtrip   ◉ passed
                                         ◉ passed
                                         ◉ passed
                                         ◉ passed
                                         ◉ passed

  ✅ 5 test(s) passing

  Tip: Use view 1 to view the source of a test.

> io.test Natural.serialization.roundtrip

    New test results:

    1. Natural.serialization.roundtrip   ◉ passed
                                         ◉ passed
                                         ◉ passed
                                         ◉ passed

  ✅ 4 test(s) passing

  Tip: Use view 1 to view the source of a test.

> io.test nested.serialization.roundtrip

    New test results:

    1. nested.serialization.roundtrip   ◉ passed
                                        ◉ passed
                                        ◉ passed

  ✅ 3 test(s) passing

  Tip: Use view 1 to view the source of a test.
```
