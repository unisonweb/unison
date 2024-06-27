```unison
unique type EncDec = EncDec Text (Nat -> Bytes) (Bytes -> Optional (Nat, Bytes))

BE64 = EncDec "64 bit Big Endian" encodeNat64be decodeNat64be
LE64 = EncDec "64 bit Little Endian" encodeNat64le decodeNat64le
BE32 = EncDec "32 bit Big Endian" encodeNat32be decodeNat32be
LE32 = EncDec "32 bit Little Endian" encodeNat32le decodeNat32le
BE16 = EncDec "16 bit Big Endian" encodeNat16be decodeNat16be
LE16 = EncDec "16 bit Little Endian" encodeNat16le decodeNat16le

testRoundTrip : Nat -> EncDec -> {IO, Stream Result} ()
testRoundTrip n = cases
  EncDec label enc dec ->
    encoded = enc n
    match dec encoded with
      Some (n', remain) ->
        if n == n' then
          emit (Ok ("successfully decoded " ++ (toText n) ++ " using " ++ label))
        else
          emit (Fail ("decoded " ++ (toText n') ++ " instead of " ++ (toText n) ++ " using " ++ label))
        if (size remain) > 0 then
          emit (Fail ("unconsumed input using " ++ label))
        else
          emit (Ok ("consumed all input"))
      None -> emit (Fail ("failed to decode " ++ (toText n) ++ " using " ++ label))

testNat : Nat -> '{IO, Stream Result} ()
testNat n _ =
  if n >= (shiftLeft 1 32) then
    testRoundTrip n BE64
    testRoundTrip n LE64
  else if n >= (shiftLeft 1 16) then
    testRoundTrip n BE64
    testRoundTrip n LE64
    testRoundTrip n BE32
    testRoundTrip n LE32
  else
    testRoundTrip n BE64
    testRoundTrip n LE64
    testRoundTrip n BE32
    testRoundTrip n LE32
    testRoundTrip n BE16
    testRoundTrip n LE16


testABunchOfNats _ =
  (runTest (testNat 0xFFFFFFFF)) ++
  (runTest (testNat 0x41000000)) ++
  (runTest (testNat 0x00410000)) ++
  (runTest (testNat 0x00004100)) ++
  (runTest (testNat 0x86753099)) ++
  (runTest (testNat 0x00000041)) ++
  (runTest (testNat 0))
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      type EncDec
      BE16             : EncDec
      BE32             : EncDec
      BE64             : EncDec
      LE16             : EncDec
      LE32             : EncDec
      LE64             : EncDec
      testABunchOfNats : ∀ _. _ ->{IO} [Result]
      testNat          : Nat -> '{IO, Stream Result} ()
      testRoundTrip    : Nat -> EncDec ->{IO, Stream Result} ()

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    type EncDec
    BE16             : EncDec
    BE32             : EncDec
    BE64             : EncDec
    LE16             : EncDec
    LE32             : EncDec
    LE64             : EncDec
    testABunchOfNats : ∀ _. _ ->{IO} [Result]
    testNat          : Nat -> '{IO, Stream Result} ()
    testRoundTrip    : Nat -> EncDec ->{IO, Stream Result} ()

.> io.test testABunchOfNats

    New test results:
  
    1.  ◉ testABunchOfNats   successfully decoded 4294967295 using 64 bit Big Endian
    2.  ◉ testABunchOfNats   consumed all input
    3.  ◉ testABunchOfNats   successfully decoded 4294967295 using 64 bit Little Endian
    4.  ◉ testABunchOfNats   consumed all input
    5.  ◉ testABunchOfNats   successfully decoded 4294967295 using 32 bit Big Endian
    6.  ◉ testABunchOfNats   consumed all input
    7.  ◉ testABunchOfNats   successfully decoded 4294967295 using 32 bit Little Endian
    8.  ◉ testABunchOfNats   consumed all input
    9.  ◉ testABunchOfNats   successfully decoded 1090519040 using 64 bit Big Endian
    10. ◉ testABunchOfNats   consumed all input
    11. ◉ testABunchOfNats   successfully decoded 1090519040 using 64 bit Little Endian
    12. ◉ testABunchOfNats   consumed all input
    13. ◉ testABunchOfNats   successfully decoded 1090519040 using 32 bit Big Endian
    14. ◉ testABunchOfNats   consumed all input
    15. ◉ testABunchOfNats   successfully decoded 1090519040 using 32 bit Little Endian
    16. ◉ testABunchOfNats   consumed all input
    17. ◉ testABunchOfNats   successfully decoded 4259840 using 64 bit Big Endian
    18. ◉ testABunchOfNats   consumed all input
    19. ◉ testABunchOfNats   successfully decoded 4259840 using 64 bit Little Endian
    20. ◉ testABunchOfNats   consumed all input
    21. ◉ testABunchOfNats   successfully decoded 4259840 using 32 bit Big Endian
    22. ◉ testABunchOfNats   consumed all input
    23. ◉ testABunchOfNats   successfully decoded 4259840 using 32 bit Little Endian
    24. ◉ testABunchOfNats   consumed all input
    25. ◉ testABunchOfNats   successfully decoded 16640 using 64 bit Big Endian
    26. ◉ testABunchOfNats   consumed all input
    27. ◉ testABunchOfNats   successfully decoded 16640 using 64 bit Little Endian
    28. ◉ testABunchOfNats   consumed all input
    29. ◉ testABunchOfNats   successfully decoded 16640 using 32 bit Big Endian
    30. ◉ testABunchOfNats   consumed all input
    31. ◉ testABunchOfNats   successfully decoded 16640 using 32 bit Little Endian
    32. ◉ testABunchOfNats   consumed all input
    33. ◉ testABunchOfNats   successfully decoded 16640 using 16 bit Big Endian
    34. ◉ testABunchOfNats   consumed all input
    35. ◉ testABunchOfNats   successfully decoded 16640 using 16 bit Little Endian
    36. ◉ testABunchOfNats   consumed all input
    37. ◉ testABunchOfNats   successfully decoded 2255827097 using 64 bit Big Endian
    38. ◉ testABunchOfNats   consumed all input
    39. ◉ testABunchOfNats   successfully decoded 2255827097 using 64 bit Little Endian
    40. ◉ testABunchOfNats   consumed all input
    41. ◉ testABunchOfNats   successfully decoded 2255827097 using 32 bit Big Endian
    42. ◉ testABunchOfNats   consumed all input
    43. ◉ testABunchOfNats   successfully decoded 2255827097 using 32 bit Little Endian
    44. ◉ testABunchOfNats   consumed all input
    45. ◉ testABunchOfNats   successfully decoded 65 using 64 bit Big Endian
    46. ◉ testABunchOfNats   consumed all input
    47. ◉ testABunchOfNats   successfully decoded 65 using 64 bit Little Endian
    48. ◉ testABunchOfNats   consumed all input
    49. ◉ testABunchOfNats   successfully decoded 65 using 32 bit Big Endian
    50. ◉ testABunchOfNats   consumed all input
    51. ◉ testABunchOfNats   successfully decoded 65 using 32 bit Little Endian
    52. ◉ testABunchOfNats   consumed all input
    53. ◉ testABunchOfNats   successfully decoded 65 using 16 bit Big Endian
    54. ◉ testABunchOfNats   consumed all input
    55. ◉ testABunchOfNats   successfully decoded 65 using 16 bit Little Endian
    56. ◉ testABunchOfNats   consumed all input
    57. ◉ testABunchOfNats   successfully decoded 0 using 64 bit Big Endian
    58. ◉ testABunchOfNats   consumed all input
    59. ◉ testABunchOfNats   successfully decoded 0 using 64 bit Little Endian
    60. ◉ testABunchOfNats   consumed all input
    61. ◉ testABunchOfNats   successfully decoded 0 using 32 bit Big Endian
    62. ◉ testABunchOfNats   consumed all input
    63. ◉ testABunchOfNats   successfully decoded 0 using 32 bit Little Endian
    64. ◉ testABunchOfNats   consumed all input
    65. ◉ testABunchOfNats   successfully decoded 0 using 16 bit Big Endian
    66. ◉ testABunchOfNats   consumed all input
    67. ◉ testABunchOfNats   successfully decoded 0 using 16 bit Little Endian
    68. ◉ testABunchOfNats   consumed all input
  
  ✅ 68 test(s) passing
  
  Tip: Use view 1 to view the source of a test.

```
