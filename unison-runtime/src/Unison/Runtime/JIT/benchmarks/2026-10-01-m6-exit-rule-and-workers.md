# M6: the exit rule, the cheaper round trip, and workers

`suite` and `jitSuite`, optimized build, one run each, 2026-10-01; ratios are to `off`. M6's
goal was that no `suite` row be more than 15% slower with the JIT on; each step is listed
with the rows that moved or were still over that line. The full `suite` table is at step 8
and the workers table at steps 9 to 11.

**The round trip** (step 3), measured with a loop of 100000 whose body is `Nat.pow acc 1`, a
call-out: 80.7 ns per iteration interpreted, 77.3 ns compiled before the step, 46.8 ns
after. Taking out the instruction itself (about 10 ns), a round trip went from about 67 ns
to about 37. What changed: the exit and frame tables became arrays (the lookup was 12% of the
profile); the trampoline's results come back in spare words of the unboxed stack, so a round
trip allocates nothing (the pinned buffer was 15%, an unpinned one still 7%, since
`newByteArray#` is a call into the runtime); one capability lookup and one thread-local
lookup instead of three; the trace and statistics flags read once per trampoline run (each
read of a CAF is an indirect jump); a call-out's re-entry loops inside the trampoline
instead of calling it afresh. What is left is spread thin: native entry and exit code 20%,
`unison_jit_enter` 9%, thread-local lookups 6%, the interpreter's dispatch of the
instruction and the trampoline's Haskell the rest.

**The exit cost E** stayed 7 after the round trip got cheaper: the suite was run with E = 7
and E = 4, and 4 is worse everywhere it differs (`Remote.Ref` 1.40× against 1.22×, JSON
decoding 1.19× against 1.04×, `List.foldLeft` 1.14× against 0.99×). An exit costs more than
its round trip: after a resume the rest of the function runs interpreted, and the native
callers above are unwound.

**Workers** (step 9) took `fib 20` from 88.6 µs to 41.2 µs in five measured changes:

| Change | `fib 20` |
| --- | --- |
| M5 | 88.6 µs |
| branch weights on every exit and slow-path branch (no workers yet) | 74.0 µs |
| workers: arguments and result in registers | 57.4 µs |
| no `ap` parameter (pending arguments handled in the wrapper), the C stack check once at entry instead of per call, the allocation budget read only by code that allocates | 49.5 µs |
| frame offsets computed where they are used, not at entry | 48.2 µs |
| the fast entry: base cases return before any check or register saving | 41.2 µs |


- 2026-10-01, step 1 (the static exit rule, E = 7). 389 functions left interpreted, 117
  compiled plus 110 re-entry functions, 5 MB of IR, 2.1 s of compile time over the run.
  Over 15% slower: `Remote.Ref` 1.39×, `List.map increment` 1.18×, `Stream` 1.16×,
  `Decode Nat` 1.18×, `CAS` 1.18×, `List.at` 1.22×, `fib1` 1.96×, `fib2`/`fib3` 1.97×.
  Close to it: `Map.lookup` 1.15×, `Map.insert` 1.11×, JSON decoding 1.10×. `jitSuite`:
  no change (319 µs, 89.7 µs, 21.6 µs, 5.03 µs, 250 µs, 63.4 µs, 48.2 µs, 59.2 µs, 269 µs,
  1.45 ms, 12.7 ms).
- 2026-10-01, step 2 (generator gaps). `fib1` 144 µs to 11.9 µs, "Do 10k arithmetic
  operations" 89.5 µs to 1.56 µs, `NatMap.fromList` 2.65 ms to 403 µs, `Shuffle` 0.84×.
  Still over 15%: `Remote.Ref` 1.25×, `List.map increment` 1.19×, `Stream` 1.18×,
  `Decode Nat` 1.16×, `CAS` 1.17×, `List.at` 1.21×, `fib2`/`fib3` 1.96×.
- 2026-10-01, step 3 (cheaper round trip: about 37 ns, was about 67). Over 15% now:
  `Remote.Ref` 1.22×, `fib2`/`fib3` 1.20×. Everything else is at or below 1.07×:
  `List.map increment` 1.01×, `Stream` 1.01×, `Decode Nat` 0.92×, `CAS` 0.87×, `Map.lookup`
  0.94×, `List.at` 0.93×, JSON decoding 1.04×. `jitSuite` with `on`: 318 µs, 88.6 µs,
  21.2 µs, 5.07 µs, 272 µs, 64.0 µs, 48.2 µs, 58.9 µs, 260 µs, and the text benchmarks
  1.04 ms and 9.16 ms (`off`: 1.48 ms and 12.8 ms; they were at break-even before).
- 2026-10-01, step 4 (builtins get cells). No change beyond noise except `List.foldLeft`
  0.99× to 1.08×. Over 15%: `Remote.Ref` 1.20×, `fib2`/`fib3` 1.19×. `jitSuite` unchanged.
- 2026-10-01, step 8 (the rule counts exits of callees and weighs recursive arms; small
  functions aren't entered from the interpreter; `Any` registered). Two runs of `on` and a
  fresh `off`, ratios to `off`:

  | Benchmark | `off` | `on`, run 1 | `on`, run 2 |
  | --- | --- | --- | --- |
  | Mutate a Ref 1000 times | 113 µs | 0.10 | 0.10 |
  | Mutate a local Remote.Ref 10k times (one run of 20 ms) | 19.7 ms | 1.22 | 1.30 |
  | Do 10k arithmetic operations | 88.9 µs | 0.02 | 0.02 |
  | List.map increment | 145 µs | 1.01 | 1.00 |
  | List.map murmurHash | 396 µs | 1.01 | 1.01 |
  | Multimap.fromList | 63.9 µs | 0.99 | 1.04 |
  | Stream functions | 5.80 ms | 1.01 | 1.08 |
  | Value.serialize, compressed, deserialize | 10.9, 20.0, 21.9 ms | 1.00, 1.01, 1.01 | 1.00, 1.02, 1.04 |
  | Json.toText, parsing, complex parsing | 7.05, 7.19, 10.7 µs | 1.00, 1.01, 1.00 | 1.08, 1.02, 1.01 |
  | Json complex decoding | 62.1 µs | 1.04 | 1.05 |
  | Decode Nat | 167 ns | 1.08 | 0.94 |
  | Generate 100 random numbers | 83.5 µs | 0.51 | 0.51 |
  | List.foldLeft | 1.20 ms | 1.00 | 1.07 |
  | Count to 1 million, to N, to 1000 | 43.1 ms, 82 ns, 83.2 µs | 0.01, 0.05, 0.06 | same |
  | CAS an IO.ref 1000 times | 127 µs | 1.09 | 0.90 |
  | List.range (per element), 0 1000 | 42 ns, 58.1 µs | 1.02, 1.00 | 1.05, 1.01 |
  | Set.fromList, Map.fromList | 31.4, 29.0 µs | 1.00, 1.00 | 1.03, 1.07 |
  | NatMap.fromList | 2.65 ms | 0.14 | 0.14 |
  | Map.lookup, Map.insert | 199, 274 ns | 1.07, 1.04 | 0.97, 0.99 |
  | Shuffle a 1000 element array | 1.32 ms | 0.74 | 0.75 |
  | Mutably mergesort a 1000 element array | 3.57 ms | 0.20 | 0.20 |
  | List.at | 146 ns | 1.10 | 0.94 |
  | Text.split / | 2.87 µs | 1.02 | 1.02 |
  | Two, Four, Thirty match | 5.64, 5.83, 5.30 ms | 0.08, 0.07, 0.01 | same |
  | fib1 | 144 µs | 0.08 | 0.08 |
  | fib2, fib3 | 336, 336 µs | 0.99, 1.00 | 1.00, 1.00 |

  `jitSuite` with `on`: 315 µs, 88.9 µs, 22.5 µs, 5.01 µs, 270 µs, 63.8 µs, 48.1 µs,
  58.8 µs, 270 µs, 1.06 ms, 9.07 ms. The two regimes and `Remote.Ref` are discussed in
  the ideas doc. The tree-insert benchmark has been at 270 µs since step 3 (250 before): to
  look at with workers.
- Debug-runtime (`-DS`) runs, 2026-10-01. After part 1: the tests with `on` and
  `THRESHOLD=1` and with `eager`, and the benchmark transcript with `on` and `THRESHOLD=1`,
  passed with no assertion failures; the run with `install=5,pool=8,alloc=64,poll=7` failed
  with "native code disappeared from its cell": with direct calls a function can run, and
  take an exit that says "call me again", before the compile thread has written its cell;
  the trampoline now waits for the cell. After workers (commit e91d83367) all four pass with no assertion
  failures: 7.5 min, 7.5 min, 9 min and 4 min.
- 2026-10-01, steps 9 to 11 (workers). `jitSuite`, optimized build, one run each:

  | Benchmark | `off` | `on` | `on`, no workers | `on`, `BATCH=1` | `eager` |
  | --- | --- | --- | --- | --- | --- |
  | Sum 0 to 1 million | 66.4 ms | 319 µs | 322 µs | 320 µs | 319 µs |
  | fib 20 | 1.40 ms | 41.2 µs | 73.6 µs | 41.4 µs | 41.1 µs |
  | Cons list: map with a lambda | 86.9 µs | 17.2 µs | 21.3 µs | 17.0 µs | 16.7 µs |
  | Cons list: foldLeft with a lambda | 86.8 µs | 5.12 µs | 4.89 µs | 4.96 µs | 4.86 µs |
  | Binary tree: 1000 inserts | 1.69 ms | 232 µs | 276 µs | 243 µs | 223 µs |
  | Binary tree: 1000 lookups | 768 µs | 63.0 µs | 67.2 µs | 77.1 µs | 74.1 µs |
  | Apply a function argument 10000 times | 1.17 ms | 49.1 µs | 48.7 µs | 48.9 µs | 48.5 µs |
  | Mutate a Ref 10000 times | 834 µs | 50.9 µs | 51.4 µs | 51.5 µs | 52.7 µs |
  | Calls across definitions (Collatz) | 7.86 ms | 69.4 µs | 262 µs | 258 µs | 264 µs |
  | Text: append "hi" 10000 times | 1.49 ms | 1.08 ms | 1.07 ms | 1.06 ms | 1.06 ms |
  | Text: drop 1, 100000 times | 12.9 ms | 9.19 ms | 9.15 ms | 9.09 ms | 9.04 ms |

  "No workers" is `UNISON_JIT_DISABLE=worker`; it already has the branch weights, which is
  why its `fib 20` is 74 µs and not M5's 89. `suite` with workers: as at step 8, except
  `fib1` 11.9 µs to 5.9 µs, `NatMap.fromList` 380 µs to 294 µs, `Two match` 430 µs to
  403 µs; `Remote.Ref` (the one-shot entry) read 1.42× in this run.
