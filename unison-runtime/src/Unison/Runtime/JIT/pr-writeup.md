# Unison's JIT compiler

The `jit` branch adds an LLVM-based JIT to the Unison runtime. Functions that get hot are
compiled to native code and run there; anything the JIT doesn't handle runs on the
interpreter as before, with the same results. Nothing in the language, the serialized form of
code and values, or `Universal.compare` changes. The design is in
[design.md](design.md): an overview in Part 1, then each mechanism in Part 2 with links into
the code.

## Using it

```bash
unison --jit on
```

or `UNISON_JIT=on`; the default is off. It needs LLVM 20 or newer installed as a shared
library (Homebrew's `llvm` on macOS, `llvm-20-dev` on Ubuntu, the LLVM release on Windows;
the full table is in [usage.md](usage.md#installing-llvm)); ucm loads it at startup and prints
`JIT compilation activated ⚡🚀`, or the reason it couldn't and runs everything interpreted.
`--jit eager` compiles every definition as it loads, for testing. The settings, what to
expect and the troubleshooting are in [usage.md](usage.md). It applies to compiled programs
(`run.compiled`) as well as to a codebase. Built and tested on macOS (Apple silicon and
Intel), Linux (x86-64 and arm64) and Windows, each with a CI smoke test.

## Highlights

- **Loops, arithmetic and calls run 10 to 200× faster** than trunk: counting to a million
  66.5 ms → 319 µs, Collatz steps 116×, `fib 20` 38×, float arithmetic 30×.
- **Data, text, bytes, lists and arrays, 4 to 130×**: the native code builds and takes apart
  constructors, ropes, finger trees and arrays itself, without calling into Haskell. Binary
  tree inserts 16×, a 16-field record loop 19×, mutable byte arrays 127×, `murmurHash` 24×.
- **Ability requests on affine handlers, 4 to 5×**: a request is a call of the handler found
  in the environment, and the handler's case returns to the requester, with no continuation
  captured ([Ability requests](design.md#ability-requests)).
- **What doesn't speed up** is time spent in Haskell: serialization, the JSON parser, the
  cloud library's `Remote.Ref`. Those rows are at 1×. A few rows are below 1×, explained
  under the tables.

## Performance against trunk

Measured 2026-10-16 on one Mac (macOS arm64), both binaries optimized `stack build`s: trunk
at `66a0f5002` (plus the schema-27 migration commit of unisonweb/unison#6307, so that it can
open the benchmark codebase; it doesn't touch the runtime) against `jit` at `b5cbbe098` with
`--jit on`. Improvement is what to multiply trunk's throughput by. The JIT's own suite
(`unison-src/transcripts-manual/jit-benchmarks.md`) was run twice and the second round is
shown; the rounds agree within 3%. The broad suite (`jit-suite.md`, the
`@pchiusano/misc-benchmarks` suite) was run once per binary.

### The JIT's suite

| Improvement | trunk | `--jit on` | Benchmark |
| --- | --- | --- | --- |
| **209×** | 66.5 ms | 319 µs | Sum 0 to 1 million |
| **38×** | 1.34 ms | 35.3 µs | fib 20 |
| **6.1×** | 81.3 µs | 13.4 µs | Cons list: map with a lambda (1000 elements) |
| **17×** | 83.1 µs | 4.85 µs | Cons list: foldLeft with a lambda (1000 elements) |
| **16×** | 1.67 ms | 104 µs | Binary tree: 1000 inserts |
| **20×** | 762 µs | 38.3 µs | Binary tree: 1000 lookups |
| **20×** | 1.14 ms | 58 µs | Apply a function argument 10000 times |
| **20×** | 831 µs | 41.8 µs | Mutate a Ref 10000 times |
| **116×** | 7.68 ms | 66.3 µs | Calls across definitions: Collatz steps for 1 to 1000 |
| **131×** | 15.5 ms | 119 µs | Calls across definitions: callee first reached once the caller is hot, 100000 iterations |
| **6.2×** | 1.49 ms | 239 µs | Text: append "hi" 10000 times |
| **6.8×** | 12.9 ms | 1.88 ms | Text: drop 1, 100000 times |
| **6×** | 1.41 ms | 237 µs | Bytes: append 2 bytes 10000 times |
| **7.4×** | 13.5 ms | 1.82 ms | Bytes: drop 1, 100000 times |
| **6.3×** | 13.6 ms | 2.15 ms | Bytes: at, 100000 times |
| **4.8×** | 16 ms | 3.36 ms | Text: uncons walk over 100000 characters |
| **5.5×** | 1.61 ms | 296 µs | List: cons 10000 elements, then a pattern walk |
| **13×** | 1.87 ms | 143 µs | List: at, 10000 times |
| **11×** | 8.08 ms | 719 µs | Nat.toText and Nat.fromText, 10000 times |
| **30×** | 30.4 ms | 1 ms | Float: sqrt, sin, cos, arithmetic and conversions, 100000 times |
| **127×** | 63.5 ms | 500 µs | MutableByteArray: 4 writes and 6 reads, both byte orders, 100000 times |
| **54×** | 28.9 ms | 539 µs | FFI.Ptr: 4 writes and 6 reads, 100000 times |
| **86×** | 3.93 ms | 45.7 µs | MutableArray: fill, freeze and sum 10000 elements |
| **7.8×** | 1.03 ms | 132 µs | Ref.cas loop, 10000 times |
| **24×** | 6.9 ms | 283 µs | murmurHashUntyped of Some (i, "x"), 10000 times |
| **15×** | 109 ms | 7.45 ms | Records: a literal, an update, a pattern and two getters, 100000 times |
| **19×** | 99.5 ms | 5.25 ms | Records: a 16-field literal and four getters, 100000 times |
| **4.9×** | 16.1 ms | 3.3 ms | Counter: 100000 requests |
| **3.9×** | 26.2 ms | 6.67 ms | Store: get and put, 100000 iterations |
| **1.4×** | 6.13 ms | 4.26 ms | handle installed per iteration, 10000 iterations |
| **3.6×** | 38.1 ms | 10.5 ms | Store whose put requests an outer Counter, 100000 iterations |
| **5.1×** | 2.4 ms | 472 µs | Bytes: decodeNat64be walk over 80000 bytes |

### The broad suite

| Improvement | trunk | `--jit on` | Benchmark |
| --- | --- | --- | --- |
| **12×** | 109 µs | 9.46 µs | Mutate a Ref 1000 times |
| **0.90×** | 20 ms | 22.4 ms | Mutate a local Remote.Ref 10k times |
| **57×** | 88.5 µs | 1.54 µs | Do 10k arithmetic operations |
| **6.6×** | 137 µs | 20.8 µs | List.map increment (range 0 1000) |
| **1.1×** | 398 µs | 361 µs | List.map murmurHash (range 0 1000) |
| **0.72×** | 64.1 µs | 89.6 µs | Multimap.fromList (range 0 1000) |
| **4.3×** | 5.65 ms | 1.3 ms | Stream functions |
| **0.97×** | 10.8 ms | 11.2 ms | Value.serializeUncompressed (10k element map) |
| **0.97×** | 20 ms | 20.7 ms | Value.serializeCompressed (10k element map) |
| **0.99×** | 21.8 ms | 22.1 ms | Value.deserializeCompressed (10k element map) |
| **0.97×** | 6.99 µs | 7.2 µs | Json.toText (per document) |
| **0.72×** | 7.13 µs | 9.91 µs | Json parsing (per document) |
| **0.59×** | 10.6 µs | 17.9 µs | Json complex parsing (per document) |
| **1.7×** | 60.6 µs | 34.9 µs | Json complex decoding (per document) |
| **5.8×** | 167 ns | 29 ns | Decode Nat |
| **2.1×** | 81.6 µs | 38.9 µs | Generate 100 random numbers |
| **7.1×** | 1.16 ms | 164 µs | List.foldLeft |
| **133×** | 42.4 ms | 319 µs | Count to 1 million |
| **20×** | 79 ns | 4 ns | Count to N (per element) |
| **17×** | 81.8 µs | 4.86 µs | Count to 1000 |
| **8.6×** | 124 µs | 14.4 µs | CAS an IO.ref 1000 times |
| **0.67×** | 43 ns | 64 ns | List.range (per element) |
| **1.2×** | 62.3 µs | 53.1 µs | List.range 0 1000 |
| **0.28×** | 31.1 µs | 112 µs | Set.fromList (range 0 1000) |
| **0.39×** | 28.8 µs | 73.7 µs | Map.fromList (range 0 1000) |
| **26×** | 2.7 ms | 103 µs | NatMap.fromList (range 0 1000) |
| **6.4×** | 197 ns | 31 ns | Map.lookup (1k element map) |
| **1.8×** | 299 ns | 163 ns | Map.insert (1k element map) |
| **2.9×** | 1.31 ms | 453 µs | Shuffle a 1000 element array |
| **28×** | 3.62 ms | 130 µs | Mutably mergesort a 1000 element array |
| **7.2×** | 143 ns | 20 ns | List.at (1k element list) |
| **1×** | 2.92 µs | 2.87 µs | Text.split / |
| **16×** | 5.66 ms | 349 µs | Two match |
| **17×** | 5.77 ms | 348 µs | Four match |
| **33×** | 5.29 ms | 159 µs | Thirty match |
| **26×** | 138 µs | 5.2 µs | fib1 |
| **6.5×** | 307 µs | 47.3 µs | fib2 |
| **6.6×** | 308 µs | 46.9 µs | fib3 |

The rows below 1×:

- `Set.fromList` and `Map.fromList` (0.28× and 0.39×): on trunk these are Haskell `Data.Map`
  call-outs. Under the JIT the `Map` family runs as compiled Unison code, which wins on
  `lookup` and `insert` (6× and 1.8× above) and on maps of the sizes programs commonly use,
  but loses on bulk construction and above a few thousand keys. `UNISON_REPLACEMENTS=on`
  brings the Haskell maps back ([the `Map` caveat](usage.md#what-to-expect);
  [measurements](benchmarks/2026-10-11-replacements.md)).
- JSON parsing (0.59× to 0.72×) and `List.range` per element (0.67×) are not losses; they
  are work trunk's benchmark never does. On trunk the parser and `List.range` build a
  `Data.Sequence`, which is lazy in its elements, so a JSON value's children and a range's
  numbers stay thunks when the result is ignored, as these rows ignore it. The branch's
  list is strict (native code reads it directly), so the values are built. Measured on the
  2026-10-17 follow-up: the Haskell parser alone takes 6.9 µs per document on both; with the
  children forced, 9.8 µs on trunk and 10.6 on the branch; parse then `Json.toText` from
  Unison, 13.97 µs on trunk and 13.91 on the branch's interpreter. `Nat.range 0 100000` then
  a fold: 16.3 ms on trunk, 17.6 interpreted on the branch, 4.1 with the JIT on.
- `Multimap.fromList` (0.72×) and `Remote.Ref` (0.90×), the same map and call-out stories.

This run is [2026-10-16 against trunk](benchmarks/2026-10-16-against-trunk.md). Earlier runs along the way, including the interpreter of this branch against its own native
code, are in [benchmarks/](benchmarks/).

## What changed in the interpreter

The branch also changes the interpreter, since native code reads its heap objects directly:

- Data constructors with up to six fields are flat closures, the first six constructors of
  `GClosure` carry GHC's pointer tags, and texts, lists and bytes are closure constructors of
  their own rather than `Foreign` wrappers ([Layouts](design.md#layouts)).
- Lists are a strict finger tree (`Deque`) and `Text` and `Bytes` ropes over it, in
  `lib/unison-util-rope`, with their own tests and benchmarks.
- The dispatch types (`GSection`, `GInstr`, `Prim1`, `Prim2`, `Foreign`) are ordered by
  measured dispatch frequency, so that the hot constructors have pointer tags.
- Structural records from trunk are native too (merged 2026-10-15).
- A chunk that an append made has slack: the next append to it writes in place after a
  compare-and-swap on the array's used length, instead of copying the chunk
  ([2026-10-17 slack chunks](benchmarks/2026-10-17-slack-chunks.md)).
- Two interpreter bugs surfaced because every native path is checked against the
  interpreter: the sized `Int` foreign conventions tagged their results `Nat`, and
  `Universal.compare` ordered every negative float above `0.0` (bit 64 cleared instead of
  bit 63). Both are fixed on both sides.

## What the diff is made of

`git diff --numstat` from the merge base with `trunk` (`66a0f5002`), 2026-10-16: 104 files,
33,126 lines added, 835 removed, 33,961 in all. Lines below are added plus removed.

| Kind | Files | Lines | Share |
| --- | --- | --- | --- |
| C: `cbits/jit_rt.c` 3,990, `jit_helpers.c` 756, `jit_llvm.c` 494, headers 394, and the generated `jit_helpers.ll` 3,939 (the IR of `jit_helpers.c`, embedded) | 8 | 9,641 | 28% |
| New Haskell: `Runtime/JIT.hs` and `Runtime/JIT/*.hs` | 13 | 9,174 | 27% |
| Markdown docs: design, usage, development, builtins, ideas, this file, and the benchmark runs | 43 | 5,041 | 15% |
| Transcripts: tests 3,704, benchmarks 413, the broad suite and the replacements suite | 5 | 4,313 | 13% |
| `lib/unison-util-rope`: `Deque`, `Skews`, the `Rope` rework, tests and benchmarks | 10 | 3,422 | 10% |
| Existing Haskell, modified: `Machine.hs` +498 -246, `Stack.hs` +411 -185, `MCode.hs` +373 -81, `Foreign/Function.hs` +106 -65, fourteen others under 40 lines each | 18 | 2,203 | 6% |
| Build config, CI and `scripts/jit-test-matrix.sh` | 7 | 167 | 0% |

By role, counting the new code:

| Area | Where | Lines |
| --- | --- | --- |
| Code generation, MCode to LLVM IR | `Codegen.hs` 5,091, `Strict.hs`, `Helpers.hs` | 5,156 |
| Native builtins: lists, ropes, the rest of Text and Bytes, arrays and refs, the hash and comparison walkers | `jit_rt.c` 3,200 of its sections, `jit_helpers.c`, `jit_internal.h`, `Native.hs` | 4,600 |
| Startup checks: the closure layouts probed and every helper checked against the interpreter | `Layout.hs` | 1,521 |
| Compile driver: what to compile and when, budgets, configuration, the LLVM shim | `JIT.hs`, `Compile.hs`, `Config.hs`, `Estimate.hs`, `LLVM.hs`, `jit_llvm.c` | 2,213 |
| Trampoline and interpreter integration: native cells, exits, frames, the constant pool, partial applications, the handler environment | `Exits.hs`, `Frames.hs`, `Pool.hs`, `jit_rt.h`, `jit_rt.c` 800, and the modified interpreter modules | 3,500 |
| Runtime data structures | `lib/unison-util-rope` | 3,422 |
| Tests and benchmarks | the transcripts and the matrix script | 4,434 |
| Documentation | the Markdown files | 5,041 |

The compiler proper (generation, layouts, driver, trampoline) is about 12,400 lines; the native
builtins and the data-structure rework 8,000; tests and docs 9,500.
