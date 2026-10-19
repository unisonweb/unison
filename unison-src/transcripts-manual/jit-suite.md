# The full benchmark suite, for the JIT

Runs `suite` from `@pchiusano/misc-benchmarks` (lists, maps, text, JSON, abilities: ordinary
library code) in the `jit` testing codebase (`scripts/testing-codebase.sh jit` downloads it once and prints its directory). It takes about five and a half minutes. Use an
optimized build, once with the JIT off and once with it on, and compare:

```
UNISON_JIT=off stack exec --work-dir .stack-work-opt unison -- -C "$(scripts/testing-codebase.sh jit)" transcript.fork unison-src/transcripts-manual/jit-suite.md
UNISON_JIT=on  stack exec --work-dir .stack-work-opt unison -- -C "$(scripts/testing-codebase.sh jit)" transcript.fork unison-src/transcripts-manual/jit-suite.md
```

The timings are printed to the console as the transcript runs. With `UNISON_JIT_STATS=each`
the exits native code took are listed after each benchmark's line (the busiest sites), which
says why a benchmark is slow with the JIT on. See docs/jit/m6.md.

``` ucm
jit-tests/main> run suite
```
