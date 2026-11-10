# 2026-11-05: the Remote.Ref row, a partial application after a box

The library suite's "Mutate a local Remote.Ref 10k times" ran at 0.58x of trunk on the branch
(optimization-ideas.md had it as a regression to look at later). `UNISON_JIT_STATS=each` on the row
alone (`run suite.localCloud`) showed 10,000 exits in 10,000 iterations, all at one site: a worker of
the cloud library's local `Remote` handler applies a four-argument function (the handler's CAS step)
to three arguments, `App (Env f) (VArgN [5,0,2])`, and left native code there every time. The
code generator built such a partial application in place only when it was a binding's whole value;
here the binding starts with `Box 0`, since the first captured value arrived wide (a call's result,
Unison.Runtime.MCode.Wide), so the binding went through `genApp`, which had no under-application
case and exited. Trunk has no boxes, so the same binding was the bare application there and was
built in place. The fix (Codegen.hs, `genApp`) builds the closure in place and yields it; with
pending arguments (a thunk forced as `f x ()`) the yield exits to the interpreter at a yield of the
closure's slot, not at the application, whose slot-relative arguments would read the closure as `p`
(found by `!(escapeClosure 1)` in jit-tests.md, which came back as `do (3, 3)`). jit-tests.md has a
new row, `captured 3000`, with the shape.

Trunk's optimized build at 25234f241 against the branch's at this change, both in their own work
dirs, interleaved, on an idle machine. Times in milliseconds; the ratio is trunk over branch, so
above 1 means the branch is faster.

## The row alone, `run suite.localCloud`, `UNISON_JIT=eager`

Five runs per line, three lines each, alternating binaries. Before the fix the branch's row was
7.2 to 8.4 ms in the same setting (five runs, and 8.2 and 8.4 in the two suite runs of
[2026-11-05-shrink-wrap.md](2026-11-05-shrink-wrap.md)).

| binary | runs (ms) |
|---|---|
| trunk | 5.255 4.308 4.045 4.191 4.155 |
| branch | 4.690 3.759 3.835 3.757 3.881 |
| trunk | 5.249 4.457 4.218 4.203 4.061 |
| branch | 4.873 4.373 4.009 3.892 3.966 |
| trunk | 6.358 5.212 5.048 5.009 5.058 |
| branch | 4.878 4.047 4.022 3.872 3.869 |

The exits at the site are gone; what the row still takes is the handler's requests.

## `jit-suite.md`, `UNISON_JIT=eager`, best of two runs each, interleaved

| row | trunk (ms) | branch (ms) | ratio |
|---|---|---|---|
| Mutate a local Remote.Ref 10k times | 4.421 | 3.909 | 1.13x |
| Do 10k arithmetic operations | 0.002 | 0.002 | 0.99x |
| List.map increment (range 0 1000) | 0.019 | 0.020 | 0.98x |
| List.map murmurHash (range 0 1000) | 0.370 | 0.391 | 0.95x |
| Multimap.fromList (range 0 1000) | 0.080 | 0.071 | 1.12x |
| Stream functions | 0.981 | 1.077 | 0.91x |
| Value.serializeUncompressed (10k element map) | 10.926 | 10.998 | 0.99x |
| Value.serializeCompressed (10k element map) | 20.460 | 20.282 | 1.01x |
| Value.deserializeCompressed (10k element map) | 21.624 | 22.315 | 0.97x |
| Json.toText (per document) | 0.007 | 0.007 | 0.99x |
| Json parsing (per document) | 0.010 | 0.010 | 1.01x |
| Json complex parsing (per document) | 0.017 | 0.017 | 1.01x |
| Json complex decoding (per document) | 0.035 | 0.035 | 1.00x |
| Generate 100 random numbers | 0.043 | 0.036 | 1.19x |
| List.foldLeft | 0.163 | 0.160 | 1.02x |
| Count to 1 million | 0.318 | 0.319 | 1.00x |
| Count to 1000 | 0.005 | 0.005 | 1.00x |
| CAS an IO.ref 1000 times | 0.014 | 0.015 | 0.95x |
| List.range 0 1000 | 0.050 | 0.050 | 1.01x |
| Set.fromList (range 0 1000) | 0.108 | 0.086 | 1.25x |
| Map.fromList (range 0 1000) | 0.073 | 0.058 | 1.24x |
| NatMap.fromList (range 0 1000) | 0.165 | 0.114 | 1.44x |
| Shuffle a 1000 element array | 0.480 | 0.407 | 1.18x |
| Mutably mergesort a 1000 element array | 0.133 | 0.142 | 0.94x |
| Text.split / | 0.003 | 0.003 | 0.99x |
| Two match | 0.363 | 0.053 | 6.83x |
| Four match | 0.357 | 0.053 | 6.72x |
| Thirty match | 0.096 | 0.088 | 1.09x |
| fib1 | 0.005 | 0.005 | 0.97x |
| fib2 | 0.050 | 0.052 | 0.96x |
| fib3 | 0.050 | 0.052 | 0.96x |

The Remote.Ref row is 1.13x of trunk, from 0.58x; every other row matches the branch's earlier
runs against trunk ([2026-11-03-wide-anf-pass.md](2026-11-03-wide-anf-pass.md) and later): the
match rows 6.8x, the map and set constructions 1.1x to 1.4x, the rest within noise.

## `jit-benchmarks.md`, `UNISON_JIT=on`, the branch before and after the fix

Before is the best of the two runs of [2026-11-05-shrink-wrap.md](2026-11-05-shrink-wrap.md)'s
"off" arm (the committed flag, binary at fc1ffbe4b); after is one run of this binary, so a little
below 1 is the expected skew of one run against a best of two. Nothing in this suite has the shape.

| row | before (ms) | after (ms) | ratio |
|---|---|---|---|
| fib 20 | 0.037 | 0.037 | 0.99x |
| Cons list: map with a lambda (1000 elements) | 0.014 | 0.014 | 0.99x |
| Cons list: foldLeft with a lambda (1000 elements) | 0.005 | 0.005 | 1.00x |
| Binary tree: 1000 inserts | 0.093 | 0.095 | 0.98x |
| Binary tree: 1000 lookups | 0.014 | 0.014 | 0.98x |
| Cons list: filterMap with a lambda returning Optional (1000 elements) | 0.011 | 0.011 | 1.01x |
| Cons list: head, 10000 times | 0.024 | 0.024 | 1.00x |
| State tuple through a non-tail call, 10000 steps | 0.005 | 0.005 | 1.00x |
| Ok or Err from a step function, 10000 steps | 0.022 | 0.022 | 1.00x |
| Loop state: a tuple rebuilt each round, 100000 rounds | 0.032 | 0.032 | 0.99x |
| Loop state: a three-field constructor rebuilt each round, 100000 rounds | 0.055 | 0.055 | 1.00x |
| Loop state: a record updated each round, 100000 rounds | 0.077 | 0.077 | 0.99x |
| Loop state: an Either alternating each round, 100000 rounds | 0.062 | 0.062 | 1.00x |
| Known constructor: a match on two arguments at once, 100000 rounds | 0.101 | 0.102 | 0.99x |
| Case of case: an Optional built in branches and matched, 100000 rounds | 0.188 | 0.188 | 1.00x |
| Apply a function argument 10000 times | 0.049 | 0.049 | 1.00x |
| Mutate a Ref 10000 times | 0.035 | 0.035 | 1.00x |
| Calls across definitions: Collatz steps for 1 to 1000 | 0.077 | 0.076 | 1.01x |
| Calls across definitions: callee first reached once the caller is hot, 100000 iterations | 0.147 | 0.147 | 1.00x |
| Calls across definitions: callee native before the caller, Collatz steps for 1 to 1000 | 0.066 | 0.067 | 0.99x |
| Calls across definitions: callee native before the caller, 100000 calls of a one-step callee | 0.494 | 0.510 | 0.97x |
| Text: append "hi" 10000 times | 0.195 | 0.196 | 1.00x |
| Text: drop 1, 100000 times | 1.879 | 1.887 | 1.00x |
| Bytes: append 2 bytes 10000 times | 0.195 | 0.196 | 1.00x |
| Bytes: drop 1, 100000 times | 1.827 | 1.836 | 1.00x |
| Bytes: at, 100000 times | 2.065 | 2.032 | 1.02x |
| Text: uncons walk over 100000 characters | 3.258 | 3.258 | 1.00x |
| List: cons 10000 elements, then a pattern walk | 0.284 | 0.285 | 0.99x |
| List: at, 10000 times | 0.149 | 0.148 | 1.01x |
| Nat.toText and Nat.fromText, 10000 times | 0.710 | 0.713 | 1.00x |
| Float: sqrt, sin, cos, arithmetic and conversions, 100000 times | 1.002 | 1.003 | 1.00x |
| MutableByteArray: 4 writes and 6 reads, both byte orders, 100000 times | 0.486 | 0.486 | 1.00x |
| FFI.Ptr: 4 writes and 6 reads, 100000 times | 0.538 | 0.538 | 1.00x |
| MutableArray: fill, freeze and sum 10000 elements | 0.042 | 0.043 | 0.98x |
| Ref.cas loop, 10000 times | 0.129 | 0.132 | 0.98x |
| murmurHashUntyped of Some (i, "x"), 10000 times | 0.275 | 0.279 | 0.99x |
| Records: a literal, an update, a pattern and two getters, 100000 times | 1.367 | 1.398 | 0.98x |
| Records: a 16-field literal and four getters, 100000 times | 0.096 | 0.096 | 1.00x |
| Counter: 100000 requests | 2.380 | 2.527 | 0.94x |
| Store: get and put, 100000 iterations | 5.205 | 5.575 | 0.93x |
| handle installed per iteration, 10000 iterations | 4.681 | 4.681 | 1.00x |
| Store whose put requests an outer Counter, 100000 iterations | 8.464 | 8.817 | 0.96x |
| Bytes: decodeNat64be walk over 80000 bytes | 0.446 | 0.468 | 0.95x |

Within noise on every row (0.93x to 1.02x).
