# The full `suite` with the JIT on, after M5

2026-10-01, optimized build, `run suite` in `jit_codebase`, one run each, no statistics. The
first time the broad suite was run with the JIT, and the input to M6: 9 rows faster, 5 the
same, 24 slower.

| Benchmark | `off` | `on` | |
| --- | --- | --- | --- |
| Mutate a Ref 1000 times | 111.6 µs | 13.1 µs | 8.5× faster |
| Mutate a local Remote.Ref 10k times | 19.7 ms | 104 ms | 5.3× slower |
| Do 10k arithmetic operations | 88.2 µs | 90.2 µs | same |
| List.map increment (range 0 1000) | 142 µs | 201 µs | 1.4× slower |
| List.map murmurHash (range 0 1000) | 392 µs | 600 µs | 1.5× slower |
| Multimap.fromList (range 0 1000) | 65.8 µs | 70.7 µs | 1.1× slower |
| Stream functions | 6.01 ms | 14.4 ms | 2.4× slower |
| Value.serializeUncompressed (10k element map) | 11.5 ms | 11.2 ms | same |
| Value.serializeCompressed (10k element map) | 20.3 ms | 20.8 ms | same |
| Value.deserializeCompressed (10k element map) | 23.4 ms | 27.0 ms | 1.2× slower |
| Json.toText (per document) | 7.07 µs | 7.43 µs | same |
| Json parsing (per document) | 7.20 µs | 7.85 µs | 1.1× slower |
| Json complex parsing (per document) | 10.7 µs | 11.3 µs | 1.1× slower |
| Json complex decoding (per document) | 62.9 µs | 122 µs | 1.9× slower |
| Decode Nat | 167 ns | 200 ns | 1.2× slower |
| Generate 100 random numbers | 83.9 µs | 58.7 µs | 1.4× faster |
| List.foldLeft | 1.23 ms | 2.24 ms | 1.8× slower |
| Count to 1 million | 43.5 ms | 323 µs | 135× faster |
| Count to N (per element) | 83 ns | 4 ns | 21× faster |
| Count to 1000 | 82.9 µs | 4.92 µs | 17× faster |
| CAS an IO.ref 1000 times | 128 µs | 326 µs | 2.5× slower |
| List.range (per element) | 43 ns | 46 ns | 1.1× slower |
| List.range 0 1000 | 59.9 µs | 63.9 µs | 1.1× slower |
| Set.fromList (range 0 1000) | 31.5 µs | 33.4 µs | 1.1× slower |
| Map.fromList (range 0 1000) | 29.1 µs | 31.3 µs | 1.1× slower |
| NatMap.fromList (range 0 1000) | 2.65 ms | 1.88 ms | 1.4× faster |
| Map.lookup (1k element map) | 202 ns | 252 ns | 1.2× slower |
| Map.insert (1k element map) | 275 ns | 323 ns | 1.2× slower |
| Shuffle a 1000 element array | 1.34 ms | 1.73 ms | 1.3× slower |
| Mutably mergesort a 1000 element array | 3.83 ms | 3.48 ms | 1.1× faster |
| List.at (1k element list) | 151 ns | 203 ns | 1.3× slower |
| Text.split / | 2.95 µs | 4.39 µs | 1.5× slower |
| Two match | 5.69 ms | 383 µs | 15× faster |
| Four match | 5.86 ms | 393 µs | 15× faster |
| Thirty match | 5.35 ms | 67.0 µs | 80× faster |
| fib1 | 144 µs | 291 µs | 2.0× slower |
| fib2 | 334 µs | 822 µs | 2.5× slower |
| fib3 | 334 µs | 839 µs | 2.5× slower |

- 9 faster, 5 the same, 24 slower. The cause is always the same: the code leaves native code
  and comes back, often. A round trip through the trampoline cost about 50 ns at the time
  (`Map.lookup`, whose benchmark body is one foreign call, went from 202 to 252 ns), an
  interpreted instruction 5 to 10, so a function that does one call-out per call and little
  else is slower compiled; and an exit inside a callee unwinds every native caller above it.
  The suite took 107 million exits (60 M resumes, 47 M call-outs). By kind:

  | Exits | What | Example | Fix (M6 step) |
  | --- | --- | --- | --- |
  | 28 M | `Let` whose binding can't be called natively | `fib1`: its body starts with `NMatch`, which the generator refused at the start of a function, so every recursive call exited. Also calls to builtin functions passed as values (`List.map increment`): builtins had no cell | the static exit rule; generator gaps; cells for builtins |
  | 37 M | list primitives: `VWLS` (pattern match on a list) 19 M, `SNOC` 12 M, `IDXS` 6 M | every list loop in base | native lists |
  | 17 M | foreign calls (`Map_lookup`, `Map_insert`, JSON, patterns) | functions that are a wrapper around one foreign call get compiled, and exit | the static exit rule; a cheaper round trip |
  | 10 M | `App (Dyn i)`: a call to an ability handler | the ability `fib`s, `Stream`, `Remote.Ref` | left to the interpreter by the rule (see the ideas, "Native ability requests") |
  | 6 M | `Name` (build a partial application), `SetAff`, `Reset` | effectful loops | native partial applications |
  | 5 M | primitives with no native version: `NOTB`, `REFN`, `RRFC`, `TIKR`, `RefCAS`, `EQLT`, `LZRO`, `Seq` | | generator gaps, then M7 |
- How to run it: `unison-src/transcripts-manual/jit-suite.md`, about 5.5 minutes per mode.
- Statistics used to distort timings badly (counting an exit summed every site's counter).
  Since M6 step 0 counting is one atomic add, and a run with `UNISON_JIT_STATS=each` is at
  most 7% slower than a plain one on the entries that exit most. Still, quote timings from
  runs without statistics.
- Re-measured at M6 step 0 (same day, same build plus the statistics change): every entry
  within a few percent of this table, except `Remote.Ref` (80 ms with `on`, was 104).
- To look at the MCode of a site named in the statistics (`CIx ... <group> <n>`): run with
  `UNISON_JIT=off UNISON_JIT_DUMP_MCODE=1`, kill it once loading is done, and search the
  dump for `<group>:<n>:`.
