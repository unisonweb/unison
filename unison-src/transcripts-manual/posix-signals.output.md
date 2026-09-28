This transcript exercises real SIGHUP and SIGWINCH delivery on macOS and Linux.
Run it in a disposable UCM process: its child shell signals its UCM parent.

``` ucm :hide
scratch/signals> builtins.mergeio

scratch/signals> load unison-src/transcripts-manual/posix-signals.u
```

``` ucm
scratch/signals> add

  Done.

scratch/signals> run signalPortableChecks

  true

scratch/signals> run signalChecks

  (true, true, true, true)
```
