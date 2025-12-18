``` ucm :hide
scratch/dll-ffi> builtins.mergeio
```

``` unison
testu64Spec = arr uint64 (base uint64 uint64)
testu32Spec = arr uint32 (base uint32 uint32)
testu16Spec = arr uint16 (base uint16 uint16)
testi64Spec = arr int64 (base int64 int64)
testi32Spec = arr int32 (base int32 int32)
testi16Spec = arr int16 (base int16 int16)
testdSpec = arr double (base double double)
testfSpec = arr float (base float float)

libtest = do openDLL "unison-src/transcripts-manual/dll-ffi/libtest.dll"

doTest = do
  dll = libtest()
  tu64 = getDLLSym dll "testu64" testu64Spec
  tu32 = getDLLSym dll "testu32" testu32Spec
  tu16 = getDLLSym dll "testu16" testu16Spec
  ti64 = getDLLSym dll "testi64" testi64Spec
  ti32 = getDLLSym dll "testi32" testi32Spec
  ti16 = getDLLSym dll "testi16" testi16Spec
  td = getDLLSym dll "testd" testdSpec
  tf = getDLLSym dll "testf" testfSpec
  ( tu64 1 2, tu32 1 2, tu16 1 2
  , ti64 +1 +2, ti32 +1 +2, ti16 +1 +2
  , td 1.0 2.0, tf 1.0 2.0
  )

testmbaSpec = arr uint64 (baseIO pinnedByteArray void)

doArrTest = do
  dll = libtest()
  ta = getDLLSym dll "testptr" testmbaSpec
  pa = IO.pinnedByteArray 32
  ta 32 pa
  freeze! (PinnedByteArray.cast pa)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + doArrTest   : '{IO, Exception} ImmutableByteArray
  + doTest      : '{IO, Exception} ( Nat,
                    Nat,
                    Nat,
                    Int,
                    Int,
                    Int,
                    Float,
                    Float)
  + libtest     : '{IO, Exception} DLL
  + testdSpec   : Spec (Float -> Float -> Float)
  + testfSpec   : Spec (Float -> Float -> Float)
  + testi16Spec : Spec (Int -> Int -> Int)
  + testi32Spec : Spec (Int -> Int -> Int)
  + testi64Spec : Spec (Int -> Int -> Int)
  + testmbaSpec : Spec (Nat -> PinnedByteArray {IO} ->{IO} ())
  + testu16Spec : Spec (Nat -> Nat -> Nat)
  + testu32Spec : Spec (Nat -> Nat -> Nat)
  + testu64Spec : Spec (Nat -> Nat -> Nat)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/dll-ffi> run doTest

  (4, 4, 4, +4, +4, +4, 4.0, 4.0)

scratch/dll-ffi> run doArrTest

  fromBytes
    0xs0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20
```
