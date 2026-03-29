``` ucm :hide
scratch/dll-ffi> builtins.mergeio
```

``` unison
libtest = do openDLL "unison-src/transcripts-manual/dll-ffi/libtest.so"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + libtest : '{IO, Exception} DLL

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/dll-ffi> update

  Done.
```

``` unison
testu64Spec = arr uint64 (base uint64 uint64)
testu32Spec = arr uint32 (base uint32 uint32)
testu16Spec = arr uint16 (base uint16 uint16)
testu8Spec = arr uint8 (base uint8 uint8)
testi64Spec = arr int64 (base int64 int64)
testi32Spec = arr int32 (base int32 int32)
testi16Spec = arr int16 (base int16 int16)
testi8Spec = arr int8 (base int8 int8)
testdSpec = arr double (base double double)
testfSpec = arr float (base float float)

doTest = do
  dll = libtest()
  tu64 = getDLLSym dll "testu64" testu64Spec
  tu32 = getDLLSym dll "testu32" testu32Spec
  tu16 = getDLLSym dll "testu16" testu16Spec
  tu8 = getDLLSym dll "testu8" testu8Spec
  ti64 = getDLLSym dll "testi64" testi64Spec
  ti32 = getDLLSym dll "testi32" testi32Spec
  ti16 = getDLLSym dll "testi16" testi16Spec
  ti8 = getDLLSym dll "testi8" testi8Spec
  td = getDLLSym dll "testd" testdSpec
  tf = getDLLSym dll "testf" testfSpec
  ( tu64 1 2, tu32 1 2, tu16 1 2, tu8 1 2
  , ti64 +1 +2, ti32 +1 +2, ti16 +1 +2, ti8 +1 +2
  , td 1.0 2.0, tf 1.0 2.0
  )

testmbaSpec = arr uint64 (baseIO pinnedByteArray void)
testpSpec = arr uint64 (baseIO ptr void)
getpSpec = baseIO void ptr

doArrTest = do
  dll = libtest()
  ta = getDLLSym dll "testptr" testmbaSpec
  pa = IO.pinnedByteArray 32
  ta 32 pa
  freeze! (PinnedByteArray.cast pa)

doPTest = do
  dll = libtest()
  gp = getDLLSym dll "getptr" getpSpec
  tp = getDLLSym dll "testptr2" testpSpec
  aa = getDLLSym dll "accessarr" (baseIO uint64 uint32)

  p = gp()
  tp 10 p
  map aa [0,1,2,3,4,5,6,7,8,9]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + doArrTest   : '{IO, Exception} ImmutableByteArray
  + doPTest     : '{IO, Exception} [Nat]
  + doTest      : '{IO, Exception} ( Nat,
                    Nat,
                    Nat,
                    Nat,
                    Int,
                    Int,
                    Int,
                    Int,
                    Float,
                    Float)
  + getpSpec    : Spec ('{IO} Ptr a)
  + testdSpec   : Spec (Float -> Float -> Float)
  + testfSpec   : Spec (Float -> Float -> Float)
  + testi16Spec : Spec (Int -> Int -> Int)
  + testi32Spec : Spec (Int -> Int -> Int)
  + testi64Spec : Spec (Int -> Int -> Int)
  + testi8Spec  : Spec (Int -> Int -> Int)
  + testmbaSpec : Spec (Nat -> PinnedByteArray {IO} ->{IO} ())
  + testpSpec   : Spec (Nat -> Ptr a ->{IO} ())
  + testu16Spec : Spec (Nat -> Nat -> Nat)
  + testu32Spec : Spec (Nat -> Nat -> Nat)
  + testu64Spec : Spec (Nat -> Nat -> Nat)
  + testu8Spec  : Spec (Nat -> Nat -> Nat)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/dll-ffi> run doTest

  (4, 4, 4, 4, +4, +4, +4, +4, 4.0, 4.0)

scratch/dll-ffi> run doArrTest

  fromBytes
    0xs0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20

scratch/dll-ffi> run doPTest

  [10, 11, 12, 13, 14, 15, 16, 17, 18, 19]
```

``` unison
allocPSpec = baseIO void ptr
freePSpec = baseIO ptr void

stdout = stdHandle StdOut

newline = toUtf8 "\n"

putBytes h bs = match putBytes.impl h bs with
  Left e -> raise e
  Right _ -> ()

printLine txt =
  putBytes stdout (toUtf8 txt)
  putBytes stdout newline

-- foreign finalizer
doFPTest = do
  dll = libtest()
  alloc = getDLLSym dll "allocptr" allocPSpec
  final = getDLLSymPtr dll "freeptr" freePSpec

  loop = cases
    0 -> ()
    n ->
      p : Ptr Nat32
      p = alloc()

      fp = ForeignPtr.new.foreign final p

      loop (drop n 1)

  loop 100000

-- unison finalizer
doUPTest = do
  dll = libtest()
  alloc = getDLLSym dll "allocptr" allocPSpec
  free : Ptr a ->{IO} ()
  free = getDLLSym dll "freeptr" freePSpec

  final : Ptr a -> '{IO} ()
  final p = do free p

  loop = cases
    0 -> ()
    n ->
      p : Ptr Nat32
      p = alloc()

      fp = ForeignPtr.new (final p) p

      loop (drop n 1)

  loop 100000

doAPTest = do
  loop = cases
    0 -> ()
    n ->
      fp = ForeignPtr.Int.allocate 1
      ForeignPtr.addFinalizer fp do ()
      loop (drop n 1)
  loop 100000
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + allocPSpec : Spec ('{IO} Ptr a)
  + doAPTest   : '{IO} ()
  + doFPTest   : '{IO, Exception} ()
  + doUPTest   : '{IO, Exception} ()
  + freePSpec  : Spec (Ptr a ->{IO} ())
  + newline    : Bytes
  + printLine  : Text ->{IO, Exception} ()
  + putBytes   : Handle -> Bytes ->{IO, Exception} ()
  + stdout     : Handle

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/dll-ffi> run doFPTest

  ()

scratch/dll-ffi> run doUPTest

  ()

scratch/dll-ffi> run doAPTest

  ()
```
