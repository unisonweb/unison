``` ucm :hide
> lib.install @runarorama/avroSpeedTest/releases/1.0.1
```

This transcript checks that the function replacements are improving the
performance of Avro decoding.

``` ucm
> test.io checkRuntimeOptimizations
    New test results:
    1. checkRuntimeOptimizations   ◉ Passed
  ✅ 1 test(s) passing
  Tip: Use view 1 to view the source of a test.
```

🛑

The transcript failed due to an error in the stanza above. The error is:

``` 
❗️

Sorry – I've encountered a bug in the Unison runtime.

  deoptOf: no source for User "seed''" in the frames of 131073

Please report it at
https://github.com/unisonweb/unison/issues/new/choose.
```
