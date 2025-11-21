``` ucm :hide
scratch/dll-ffi> builtins.mergeio
```

``` unison
testSpec = arr int64 (base int64 int64)
libtest = do openDLL "unison-src/transcripts-manual/dll-ffi/libtest.dll"

doTest = do
  dll = libtest()
  f = getDLLSym dll "test" testSpec
  f +1 +2
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + doTest   : '{IO, Exception} Int
  + libtest  : '{IO, Exception} DLL
  + testSpec : Spec (Int -> Int -> Int)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/dll-ffi> run doTest

  +4
```
