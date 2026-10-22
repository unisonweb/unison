# Private copies of callees, and re-entry functions in batches

2026-10-04. A module that calls an already compiled small callee compiles it again as a
private, internal copy so that LLVM can inline it, and re-entry functions asked for on demand
are held and compiled together. Neither changes generated code; what they change is compile
work and module count.

**Compile totals**, test transcript with `THRESHOLD=1`, fast build: 185 modules, 16.2 MB of
IR, 6.84 s of compile thread time before; 93 modules, 13.3 MB, 5.96 s after. 38 copies were
made in 28 modules and every one was inlined and deleted by O2 (counted in the dumped
`.opt.ll` files). Re-entry functions: 110 in 18 modules instead of 107 in 107.

**The broad `suite`** with `on`, optimized build, two runs each way: taking the better run of
each pair, every row is within 4% except these two.

| Benchmark | `on` before (two runs) | `on` with copies and re-entry batches (two runs) |
| --- | --- | --- |
| Mutate a local Remote.Ref 10k times | 28.1 ms, 27.9 ms | 27.7 ms, 24.7 ms |
| List.map murmurHash (range 0 1000) | 425 µs, 421 µs | 475 µs, 443 µs |

Compile totals for the whole suite run (`UNISON_JIT_STATS`): before, 149 modules, 94 re-entry functions on demand, 13 MB of IR, 5.2 s of compile thread time; after, 85 modules, 99 re-entry functions on demand, 14 MB of IR, 5.1 s (the IR is a little larger: the copies are counted before O2 deletes them).

The `Remote.Ref` row is one fast run on a row that has always been noisy. `List.map
murmurHash` was flagged as open here (the benchmark is a `List.map` whose body is a foreign
call, a call-out per element) and closed the next day when the row came back faster than
before in the arrays run. `jitSuite` steady state is unchanged on every row: its definitions'
callees are in their own batches already. Where this should show, and hasn't been measured, is
a program whose hot loop calls small library functions from other definitions.
