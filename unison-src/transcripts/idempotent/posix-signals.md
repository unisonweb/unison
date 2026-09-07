The signal builtins typecheck and support close/await through the Unison
runtime. Platforms without POSIX signal support return an empty catalog.

``` ucm :hide
scratch/signals> builtins.mergeio

scratch/signals> load unison-src/transcripts-manual/posix-signals.u
```

``` ucm
scratch/signals> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/signals> run signalPortableChecks

  true
```
