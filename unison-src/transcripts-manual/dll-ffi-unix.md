``` ucm :hide
scratch/dll-ffi> builtins.mergeio
```

``` unison
testSpec = arr int64 (base int64 int64)
libtest = do openDLL "unison-src/transcripts-manual/dll-ffi/libtest.so"

doTest = do
  dll = libtest()
  f = getDLLSym dll "test" testSpec
  f +1 +2
```

``` ucm
scratch/dll-ffi> run doTest
```
