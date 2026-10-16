# 2026-10-15: flat data constructors

Constructors with 3 to 6 fields became flat closures (`GData3` … `GData6`), `DataG`, `PAp`
and `Captured` hold their two segment arrays directly, and the pointer tags were
reassigned ("Layouts" in design.md; the plan is `data-layout-plan.local.md`). Optimized
builds of the commit before and of this one, the suite (`jit-benchmarks.md`) run twice per
binary and mode, alternating the binaries, best of the two per cell. The interpreter
columns are from the final matcher (the second commit below); the JIT columns from the
round before it, which the matcher doesn't touch.

| row | interpreter before | interpreter after | JIT before | JIT after |
| --- | --- | --- | --- | --- |
| Sum 0 to 1 million | 66.2 ms | 67.2 ms | 0.315 ms | 0.316 ms |
| fib 20 | 1.39 ms | 1.41 ms | 0.0354 ms | 0.0354 ms |
| Cons list: map with a lambda (1000 elements) | 0.0873 ms | 0.084 ms | 0.0135 ms | 0.013 ms |
| Cons list: foldLeft with a lambda (1000 elements) | 0.0879 ms | 0.085 ms | 0.00486 ms | 0.00483 ms |
| Binary tree: 1000 inserts | 1.69 ms | 1.3 ms | 0.189 ms | 0.102 ms |
| Binary tree: 1000 lookups | 0.764 ms | 0.721 ms | 0.0461 ms | 0.0437 ms |
| Apply a function argument 10000 times | 1.17 ms | 1.16 ms | 0.0481 ms | 0.057 ms |
| Mutate a Ref 10000 times | 0.83 ms | 0.845 ms | 0.0407 ms | 0.0416 ms |
| Calls across definitions: Collatz steps for 1 to 1000 | 7.78 ms | 7.93 ms | 0.0663 ms | 0.0672 ms |
| Calls across definitions: callee first reached once the caller is hot, 100000 iterations | 16.0 ms | 16.1 ms | 0.119 ms | 0.118 ms |
| Text: append "hi" 10000 times | 1.07 ms | 1.09 ms | 0.261 ms | 0.264 ms |
| Text: drop 1, 100000 times | 9.52 ms | 9.67 ms | 2.05 ms | 2.05 ms |
| Bytes: append 2 bytes 10000 times | 1.03 ms | 1.04 ms | 0.258 ms | 0.261 ms |
| Bytes: drop 1, 100000 times | 8.76 ms | 8.89 ms | 2.06 ms | 2.07 ms |
| Bytes: at, 100000 times | 10.1 ms | 10.1 ms | 2.2 ms | 2.23 ms |
| Text: uncons walk over 100000 characters | 10.8 ms | 11.0 ms | 3.61 ms | 3.58 ms |
| Nat.toText and Nat.fromText, 10000 times | 7.88 ms | 7.94 ms | 0.721 ms | 0.72 ms |
| Float: sqrt, sin, cos, arithmetic and conversions, 100000 times | 30.4 ms | 30.7 ms | 1 ms | 1.01 ms |
| MutableByteArray: 4 writes and 6 reads, both byte orders, 100000 times | 64.6 ms | 64.0 ms | 0.49 ms | 0.487 ms |
| FFI.Ptr: 4 writes and 6 reads, 100000 times | 28.9 ms | 28.9 ms | 0.54 ms | 0.54 ms |
| MutableArray: fill, freeze and sum 10000 elements | 3.9 ms | 4 ms | 0.0457 ms | 0.0452 ms |
| Ref.cas loop, 10000 times | 1.03 ms | 1.04 ms | 0.127 ms | 0.128 ms |
| murmurHashUntyped of Some (i, "x"), 10000 times | 6.82 ms | 7.3 ms | 0.277 ms | 0.282 ms |
| Records: a literal, an update, a pattern and two getters, 100000 times | 94.4 ms | 95.2 ms | 7.5 ms | 7.59 ms |
| Records: a 16-field literal and four getters, 100000 times | 73.1 ms | 73.9 ms | 5.12 ms | 5.09 ms |
| Counter: 100000 requests | 16.2 ms | 16.3 ms | 3.66 ms | 3.25 ms |
| Store: get and put, 100000 iterations | 27.1 ms | 27.6 ms | 6.72 ms | 6.6 ms |
| handle installed per iteration, 10000 iterations | 6.31 ms | 6.22 ms | 4.44 ms | 4.33 ms |
| Store whose put requests an outer Counter, 100000 iterations | 38.7 ms | 38.9 ms | 10.8 ms | 10.4 ms |
| Bytes: decodeNat64be walk over 80000 bytes | 1.51 ms | 1.53 ms | 0.482 ms | 0.516 ms |

What it shows:

- **Building a 4-field constructor** (the tree's `Node`) is where the change was aimed:
  inserts 0.189 → 0.102 ms native (one object of 11 words instead of five of 23, and no box
  checks when the node is matched) and 1.69 → 1.30 ms in the interpreter, which reads the
  slots into the constructor directly instead of through a segment. Lookups, which only
  read fields, gain a little on both sides (three loads per field became one).
- **Nothing else moved** within the run-to-run noise, after three regressions found on the
  way were fixed. A generic hash walker with a loop over a dynamic field count, inlined
  into `mh_val`, cost the hash row 40% natively and tripped LLVM's fixpoint self-check;
  straight-line code for the 0-, 1- and 2-field shapes with the wider ones out of line
  restored it (development.md has the lesson). The first generic `dataBranch` allocated a
  closure per match, 15% on every interpreter row with an `if`. The second scrutinized the
  closure twice per match, once for the tag and once to push the fields, which read as 3
  to 6% on every interpreter row that matches data and 18% on the uncons walk (two matches
  per character); one case per match, each alternative looking its arm up and pushing
  its own fields, is the final form (the interpreter columns above).
- **Open**: "Apply a function argument" reads 0.048 → 0.057 ms native in every round,
  0.9 ns per closure call, and 49 → 58 µs in a standalone loop. The optimized IR and the
  assembly (`llc` on the dumps) of the loop after are a strict subset of the ones before:
  the two box checks, two dependent loads and a branch are gone and nothing was added. So
  the emitted instructions are better and the time is elsewhere: the placement of the
  loop in the JIT's code memory (branch-target alignment at this size is worth about
  this much), or the arrays the pooled PAp now points at directly. Not resolved. The interpreter's hash row reads 7% slower; its hash goes through the
  `DataC` view, which has more cases now.

The records rows are a control (records are their own closure kind) and are unchanged.
