# Unison's JIT compiler

This PR adds an LLVM-based JIT to the Unison runtime, enabled with `ucm --jit on` (or the environment variable `UNISON_JIT=on`). It is up to ~ 200x faster than the interpreter for toy programs (a simple arithmetic loop where all the data fits in registers) and more typical code is often 5-10x faster. More numbers below. Nothing in the language semantics is changed by this PR, and the serialized form of code stays the same.

The big idea behind the design is to allow for partial compilation--a native-compiled function can exit back to the interpreter at any point in its execution. This allows the JIT to be useful immediately rather than requiring it to cover the entirety of the Unison language on day 1. Read more in [design.md](design.md).

The PR is quite big, however the interesting parts of the code are under 10k lines. Beyond that, there are lots of docs, benchmarks and C implementations of various Unison builtins. See the detailed breakdown below. I used Claude + Fable 5.1 for the implementation, notes on my methodology at the end.

## Performance highlights

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

## Using it

```bash
unison --jit on
```

or set the environment variable `UNISON_JIT=on`; the default is off. It needs LLVM 20 or newer installed as a shared
library (Homebrew's `llvm` on macOS, `llvm-20-dev` on Ubuntu, the LLVM release on Windows;
the full table is in [usage.md](usage.md#installing-llvm)); ucm loads it at startup and prints
`JIT compilation activated ⚡🚀`, or the reason it couldn't and runs everything interpreted.
`--jit eager` compiles every definition as it loads, for testing. The settings, what to
expect and the troubleshooting are in [usage.md](usage.md). It applies to compiled programs
(`run.compiled`) as well as to a codebase. Built and tested on macOS (Apple silicon and
Intel), Linux (x86-64 and arm64) and Windows, each with a CI smoke test.

## Performance against trunk

Improvement column is what to multiply trunk to get the JIT's times.

### The JIT's suite

See `unison-src/transcripts-manual/jit-benchmarks.md` for source. This was a custom benchmark suite built to test out different parts of the JIT:

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

See `jit-suite.md`, the `@pchiusano/misc-benchmarks` suite.

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

This run is [2026-10-16 against trunk](benchmarks/2026-10-16-against-trunk.md). Since then
the two append rows improved by 15% (text append 239 → 203 µs, bytes append 237 → 200 µs)
from the slack-chunk change, and the rest of the suite is unchanged
([2026-10-17](benchmarks/2026-10-17-exit-routine-as-ir.md)); a compiled program runs the
suite at the same speed as the codebase does
([2026-10-18](benchmarks/2026-10-18-compiled-programs.md)). Earlier runs along the way,
including the interpreter of this branch against its own native code, are in
[benchmarks/](benchmarks/).

## What changed in the interpreter

The branch doesn't make any major changes to the interpreter, but there were a few things, some for performance, and some to ensure that native code wouldn't ever be given Haskell thunks. Native code now has to pattern match directly on Haskell data types, and any thunks cause it to exit back to the interpreter.

Here's what changed in the interpreter:

- Data constructors with up to six fields are flat closures, the first six constructors of
  `GClosure` carry GHC's pointer tags, and texts, lists and bytes are closure constructors of
  their own rather than `Foreign` wrappers ([Layouts](design.md#layouts)).
- Lists are a strict finger tree (`Deque`) and `Text` and `Bytes` ropes over it, in
  `lib/unison-util-rope`, with their own tests and benchmarks.
- The dispatch types (`GSection`, `GInstr`, `Prim1`, `Prim2`, `Foreign`) are ordered by
  measured dispatch frequency, so that the hot constructors have pointer tags.
- Some tweaks to record type handling, spend up record access and setters.
- A chunk that an append made has slack: the next append to it writes in place after a
  compare-and-swap on the array's used length, instead of copying the chunk
  ([2026-10-17 slack chunks](benchmarks/2026-10-17-slack-chunks.md)).
- Compiled programs (`run.compiled`) load through the same cell-attaching path as a codebase,
  and the `.uc` file carries the constructor arities of the data types the program uses, which
  the JIT needs to compile a match; a compiled file is tied to the exact ucm version as before.
- Two interpreter bugs surfaced because every native path is checked against the
  interpreter: the sized `Int` foreign conventions tagged their results `Nat`, and
  `Universal.compare` ordered every negative float above `0.0` (bit 64 cleared instead of
  bit 63). Both are fixed on both sides.

## An overview of the diff

This is a big PR.

`git diff --numstat` from the merge base with `trunk` (`cea754e16`), 2026-10-18: 114 files,
34,655 lines added, 905 removed, 35,560 in all. Lines below are added plus removed.

| Kind | Files | Lines | Share |
| --- | --- | --- | --- |
| C: `cbits/jit_rt.c` 4,113, `jit_helpers.c` 756, `jit_llvm.c` 562, `jit_exit.c` 83, headers 401, the generated `jit_helpers.ll` 3,939 and `jit_exit.ll` 155 (the IR of the two C files, embedded), and the shell script that generates them | 9 | 10,038 | 28% |
| New Haskell: `Runtime/JIT.hs`, `Runtime/JIT/*.hs` and `cbits/gen-helpers.hs`, the IR stripper | 14 | 9,410 | 26% |
| Markdown docs: design, usage, development, builtins, ideas, this file, and the benchmark runs | 48 | 5,382 | 15% |
| Transcripts: tests 3,710, benchmarks 413, the broad suite, the replacements suite, the compiled-program test | 6 | 4,359 | 12% |
| `lib/unison-util-rope`: `Deque`, the `Rope` rework, tests and benchmarks | 10 | 3,422 | 10% |
| Existing Haskell, modified: `Machine.hs` +498 -246, `Stack.hs` +411 -185, `MCode.hs` +373 -81, `Bytes.hs` +117 -35, `Foreign/Function.hs` +106 -65, `Interface.hs` +94 -25, `Util/Text.hs` +91 -20 and its tests +121, nine others under 40 lines each | 18 | 2,656 | 7% |
| Build config, CI (the `jit` job on five platforms) and `scripts/jit-test-matrix.sh` | 9 | 293 | 1% |

By role, counting the new code:

| Area | Where | Lines |
| --- | --- | --- |
| Code generation, MCode to LLVM IR | `Codegen.hs` 5,134, `Strict.hs`, `Helpers.hs` | 5,203 |
| Native builtins: lists, ropes, the rest of Text and Bytes, arrays and refs, the hash and comparison walkers | `jit_rt.c` 3,200 of its sections, `jit_helpers.c`, `jit_internal.h`, `Native.hs` | 4,640 |
| Startup checks: the closure layouts probed and every helper checked against the interpreter | `Layout.hs`, `Native/Probes.hs` | 1,521 |
| Compile driver: what to compile and when, budgets, configuration, the LLVM shim | `JIT.hs`, `Compile.hs`, `Config.hs`, `Estimate.hs`, `LLVM.hs`, `jit_llvm.c` | 2,311 |
| Trampoline and interpreter integration: native cells, exits, frames, the constant pool, partial applications, the handler environment, the exit routine, the stack bounds per platform | `Exits.hs`, `Frames.hs`, `Pool.hs`, `jit_rt.h`, `jit_exit.c`, `jit_rt.c` 900, and the modified interpreter modules | 4,150 |
| Runtime data structures | `lib/unison-util-rope` | 3,422 |
| Tests and benchmarks | the transcripts and the matrix script | 4,501 |
| Documentation | the Markdown files | 5,382 |

The compiler proper (generation, layouts, driver, trampoline) is about 13,200 lines; the native
builtins and the data-structure rework 8,100; tests and docs 9,900.

## Notes on methodology 

I did design work up front, starting from the basic idea of partial compilation (credit to @dolio, @ChrisPenner and @aryairani for this idea, I'm not sure which of us first proposed it) and refining that until I thought it seemed reasonable. After that, I had my coding assistant produce an implementation plan, which led to some iteration and steering on that and further refinements on the design. Then I let the coding assistant rip. There were a few phases to the initial implementation which you can see in the commit history.

After something was working, there was considerable back and forth and steering on the implementation and to do further optimizations and tweaks. My goal was to meet or exceed the interpreter across the board.

Random observations:

* The LLM was amazing at writing finnicky code whose requirements were clear. For instance, with a bit of design coaching, it produced a new strict finger tree type in Haskell (used by Unison `Text` and `Bytes`), then ported the implementation to C for use by native-compiled Unison code. 
* The LLM was also great at having an encyclopedic knowledge and being relentless during debugging.
* Less good: 
  * Even with me approving the implementation plans in advance, there were a few weird architecture decisions that cropped up that I only discovered during review.  
  * For ideation, the coding assistant was kind of a mixed bag. Sometimes I would use it as a thought partner and get some mileage out of that; other times I'd realize this wasn't helping that much (or it was hurting, proposing bad ideas) and I got more mileage out of going for a walk with a notebook and only circling back when I had ideas that seemed implementable. 
  * Haskell compile times were a bottleneck during more interactive work. It doesn't accurately track dependencies, so stack recompiles everything even if you add a comment in a module that lots of things transitively depend on.
  * It's really easy to get into "LGTM" territory, where you are tempted to just trust the output. For certain kinds of code, I think this is okay, like if code is well-specificied and tested, it is fine to "lazily evaluate" understanding it in detail. But for design or architecture decisions, if you don't understand and approve of these in detail, tech debt can start piling up. Or worse, you lack enough of an understanding of the system to steer further development effectively.
  
Overall, I really enjoyed the process and felt like I learned a lot about how to get more mileage out of the tools. It still feels creative and interesting to me.

## What's next?

There are a bunch of further optimization ideas in `optimization-ideas.md`, but I think this branch is in a decent spot for people to be able to try it out. It should be low risk since it's behind a flag and it doesn't require LLVM to be present. 

Later, we can look at making the JIT the default.
