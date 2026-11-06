# 2026-10-28: wide arguments for known calls

`unison-src/transcripts-manual/jit-benchmarks.md` on the optimized build
(`.stack-work-opt`), `UNISON_JIT=on`, best of two runs each, the machine
otherwise idle (macOS arm64). "Before" is commit 1a26f7164 (the wide
convention with narrow workers, `benchmarks/2026-10-28-wide-jit.md`'s third
round); "after" is the checkpoint commit this file is part of: a function
that only matches on an argument takes it as its three slots through a
worker, with the rule that a position some call in its own group passes a
plain value stays narrow. Times are the transcript's own, in milliseconds;
the ratio is the first column over the second, so above 1 is a gain for the
second. The row "Wide arguments: an Optional from a call passed to a
recursive callee" was added for this measurement and is not in "before".

## The previous commit against this one

| row | before (ms) | after (ms) | ratio |
|---|---|---|---|
| Sum 0 to 1 million | 0.319 | 0.319 | 1.00x |
| fib 20 | 0.037 | 0.037 | 1.00x |
| Cons list: map with a lambda (1000 elements) | 0.020 | 0.020 | 1.00x |
| Cons list: foldLeft with a lambda (1000 elements) | 0.005 | 0.005 | 1.00x |
| Binary tree: 1000 inserts | 0.102 | 0.103 | 0.99x |
| Binary tree: 1000 lookups | 0.013 | 0.013 | 1.00x |
| Cons list: filterMap with a lambda returning Optional (1000 elements) | 0.014 | 0.014 | 1.00x |
| Cons list: head, 10000 times | 0.022 | 0.022 | 1.00x |
| State tuple through a non-tail call, 10000 steps | 0.089 | 0.089 | 1.00x |
| Ok or Err from a step function, 10000 steps | 0.021 | 0.021 | 1.00x |
| Apply a function argument 10000 times | 0.048 | 0.048 | 1.00x |
| Mutate a Ref 10000 times | 0.034 | 0.035 | 0.97x |
| Calls across definitions: Collatz steps for 1 to 1000 | 0.078 | 0.076 | 1.03x |
| Calls across definitions: callee first reached once the caller is hot, 100000 iterations | 0.147 | 0.147 | 1.00x |
| Text: append "hi" 10000 times | 0.199 | 0.202 | 0.98x |
| Text: drop 1, 100000 times | 1.895 | 1.916 | 0.99x |
| Bytes: append 2 bytes 10000 times | 0.198 | 0.200 | 0.99x |
| Bytes: drop 1, 100000 times | 1.879 | 1.877 | 1.00x |
| Bytes: at, 100000 times | 2.113 | 2.096 | 1.01x |
| Text: uncons walk over 100000 characters | 3.316 | 3.258 | 1.02x |
| List: cons 10000 elements, then a pattern walk | 0.289 | 0.281 | 1.03x |
| List: at, 10000 times | 0.150 | 0.151 | 0.99x |
| Nat.toText and Nat.fromText, 10000 times | 0.694 | 0.692 | 1.00x |
| Float: sqrt, sin, cos, arithmetic and conversions, 100000 times | 1.003 | 1.002 | 1.00x |
| MutableByteArray: 4 writes and 6 reads, both byte orders, 100000 times | 0.486 | 0.486 | 1.00x |
| FFI.Ptr: 4 writes and 6 reads, 100000 times | 0.538 | 0.539 | 1.00x |
| MutableArray: fill, freeze and sum 10000 elements | 0.044 | 0.045 | 0.99x |
| Ref.cas loop, 10000 times | 0.128 | 0.127 | 1.01x |
| murmurHashUntyped of Some (i, "x"), 10000 times | 0.347 | 0.346 | 1.00x |
| Records: a literal, an update, a pattern and two getters, 100000 times | 7.112 | 7.173 | 0.99x |
| Records: a 16-field literal and four getters, 100000 times | 5.032 | 5.015 | 1.00x |
| Counter: 100000 requests | 2.316 | 2.300 | 1.01x |
| Store: get and put, 100000 iterations | 5.032 | 4.999 | 1.01x |
| handle installed per iteration, 10000 iterations | 4.646 | 4.651 | 1.00x |
| Store whose put requests an outer Counter, 100000 iterations | 8.035 | 8.035 | 1.00x |
| Bytes: decodeNat64be walk over 80000 bytes | 0.456 | 0.460 | 0.99x |

## The same binary with wide arguments off (`UNISON_WIDE_ARGS=off`) and on

| row | args-off (ms) | args-on (ms) | ratio |
|---|---|---|---|
| Sum 0 to 1 million | 0.319 | 0.319 | 1.00x |
| fib 20 | 0.037 | 0.037 | 1.00x |
| Cons list: map with a lambda (1000 elements) | 0.020 | 0.020 | 1.00x |
| Cons list: foldLeft with a lambda (1000 elements) | 0.005 | 0.005 | 1.00x |
| Binary tree: 1000 inserts | 0.102 | 0.103 | 0.99x |
| Binary tree: 1000 lookups | 0.014 | 0.013 | 1.04x |
| Cons list: filterMap with a lambda returning Optional (1000 elements) | 0.014 | 0.014 | 0.99x |
| Cons list: head, 10000 times | 0.022 | 0.022 | 1.00x |
| State tuple through a non-tail call, 10000 steps | 0.089 | 0.089 | 1.00x |
| Ok or Err from a step function, 10000 steps | 0.021 | 0.021 | 1.00x |
| Wide arguments: an Optional from a call passed to a recursive callee, 10000 steps | 0.024 | 0.024 | 1.00x |
| Apply a function argument 10000 times | 0.012 | 0.048 | 0.26x |
| Mutate a Ref 10000 times | 0.034 | 0.035 | 0.97x |
| Calls across definitions: Collatz steps for 1 to 1000 | 0.076 | 0.076 | 1.00x |
| Calls across definitions: callee first reached once the caller is hot, 100000 iterations | 0.147 | 0.147 | 1.00x |
| Text: append "hi" 10000 times | 0.206 | 0.202 | 1.02x |
| Text: drop 1, 100000 times | 1.932 | 1.916 | 1.01x |
| Bytes: append 2 bytes 10000 times | 0.201 | 0.200 | 1.01x |
| Bytes: drop 1, 100000 times | 1.884 | 1.877 | 1.00x |
| Bytes: at, 100000 times | 2.101 | 2.096 | 1.00x |
| Text: uncons walk over 100000 characters | 3.309 | 3.258 | 1.02x |
| List: cons 10000 elements, then a pattern walk | 0.284 | 0.281 | 1.01x |
| List: at, 10000 times | 0.150 | 0.151 | 0.99x |
| Nat.toText and Nat.fromText, 10000 times | 0.696 | 0.692 | 1.01x |
| Float: sqrt, sin, cos, arithmetic and conversions, 100000 times | 1.006 | 1.002 | 1.00x |
| MutableByteArray: 4 writes and 6 reads, both byte orders, 100000 times | 0.486 | 0.486 | 1.00x |
| FFI.Ptr: 4 writes and 6 reads, 100000 times | 0.538 | 0.539 | 1.00x |
| MutableArray: fill, freeze and sum 10000 elements | 0.044 | 0.045 | 0.98x |
| Ref.cas loop, 10000 times | 0.128 | 0.127 | 1.00x |
| murmurHashUntyped of Some (i, "x"), 10000 times | 0.344 | 0.346 | 1.00x |
| Records: a literal, an update, a pattern and two getters, 100000 times | 7.070 | 7.173 | 0.99x |
| Records: a 16-field literal and four getters, 100000 times | 5.029 | 5.015 | 1.00x |
| Counter: 100000 requests | 2.323 | 2.300 | 1.01x |
| Store: get and put, 100000 iterations | 4.952 | 4.999 | 0.99x |
| handle installed per iteration, 10000 iterations | 4.643 | 4.651 | 1.00x |
| Store whose put requests an outer Counter, 100000 iterations | 8.040 | 8.035 | 1.00x |
| Bytes: decodeNat64be walk over 80000 bytes | 0.456 | 0.460 | 0.99x |

## The wide convention off and on, this binary

| row | off (ms) | on (ms) | ratio |
|---|---|---|---|
| Sum 0 to 1 million | 0.319 | 0.319 | 1.00x |
| fib 20 | 0.035 | 0.037 | 0.95x |
| Cons list: map with a lambda (1000 elements) | 0.014 | 0.020 | 0.70x |
| Cons list: foldLeft with a lambda (1000 elements) | 0.005 | 0.005 | 0.99x |
| Binary tree: 1000 inserts | 0.089 | 0.103 | 0.87x |
| Binary tree: 1000 lookups | 0.016 | 0.013 | 1.22x |
| Cons list: filterMap with a lambda returning Optional (1000 elements) | 0.011 | 0.014 | 0.76x |
| Cons list: head, 10000 times | 0.032 | 0.022 | 1.44x |
| State tuple through a non-tail call, 10000 steps | 0.064 | 0.089 | 0.72x |
| Ok or Err from a step function, 10000 steps | 0.040 | 0.021 | 1.85x |
| Wide arguments: an Optional from a call passed to a recursive callee, 10000 steps | 0.030 | 0.024 | 1.25x |
| Apply a function argument 10000 times | 0.055 | 0.048 | 1.14x |
| Mutate a Ref 10000 times | 0.036 | 0.035 | 1.02x |
| Calls across definitions: Collatz steps for 1 to 1000 | 0.064 | 0.076 | 0.85x |
| Calls across definitions: callee first reached once the caller is hot, 100000 iterations | 0.118 | 0.147 | 0.80x |
| Text: append "hi" 10000 times | 0.209 | 0.202 | 1.03x |
| Text: drop 1, 100000 times | 1.913 | 1.916 | 1.00x |
| Bytes: append 2 bytes 10000 times | 0.207 | 0.200 | 1.03x |
| Bytes: drop 1, 100000 times | 1.839 | 1.877 | 0.98x |
| Bytes: at, 100000 times | 2.109 | 2.096 | 1.01x |
| Text: uncons walk over 100000 characters | 3.293 | 3.258 | 1.01x |
| List: cons 10000 elements, then a pattern walk | 0.304 | 0.281 | 1.08x |
| List: at, 10000 times | 0.145 | 0.151 | 0.96x |
| Nat.toText and Nat.fromText, 10000 times | 0.703 | 0.692 | 1.02x |
| Float: sqrt, sin, cos, arithmetic and conversions, 100000 times | 1.002 | 1.002 | 1.00x |
| MutableByteArray: 4 writes and 6 reads, both byte orders, 100000 times | 0.486 | 0.486 | 1.00x |
| FFI.Ptr: 4 writes and 6 reads, 100000 times | 0.538 | 0.539 | 1.00x |
| MutableArray: fill, freeze and sum 10000 elements | 0.044 | 0.045 | 0.98x |
| Ref.cas loop, 10000 times | 0.129 | 0.127 | 1.01x |
| murmurHashUntyped of Some (i, "x"), 10000 times | 0.272 | 0.346 | 0.79x |
| Records: a literal, an update, a pattern and two getters, 100000 times | 7.103 | 7.173 | 0.99x |
| Records: a 16-field literal and four getters, 100000 times | 5.041 | 5.015 | 1.01x |
| Counter: 100000 requests | 2.591 | 2.300 | 1.13x |
| Store: get and put, 100000 iterations | 5.063 | 4.999 | 1.01x |
| handle installed per iteration, 10000 iterations | 4.320 | 4.651 | 0.93x |
| Store whose put requests an outer Counter, 100000 iterations | 8.154 | 8.035 | 1.01x |
| Bytes: decodeNat64be walk over 80000 bytes | 0.466 | 0.460 | 1.01x |

## What it showed

- **No row gains from wide arguments**, not even the one written for them:
  `walk (next i) acc`, a recursive callee matching on an `Optional` from a
  call, is 0.024 ms either way. With arguments off the `Optional` is boxed
  at the call, and the loop is one native function after inlining, so the
  object is a bump allocation and three stores whose loads LLVM forwards;
  that is about what the three slot copies cost. The return side's gains
  (Ok/Err 1.85x, head 1.44x) come from a value crossing a call boundary
  that is not inlined, where the caller would otherwise load it back from
  memory after the call. The convention off, the row is 1.25x slower, which
  is the return side again (`next`'s result).
- **Before the group rule, two rows lost**: `Cons.map` 0.79x and tree
  inserts 0.92x, measured as the only rows that move between
  `UNISON_WIDE_ARGS=off` and on with the earlier binary (runs `wb16`,
  `wb17`, not tabulated). Both take their structure as a wide position and
  pass a field of the matched object back to themselves, so the slots only
  carried copies, and the worker's call outgrew arm64's eight argument
  registers. The rule that keeps such a position narrow (`plainlyCalled`)
  brings both back to 1.00x against the previous commit.
- **`Apply a function argument` at 0.012 ms with arguments off** against
  0.048 ms on and 0.055 with the convention off is unexplained: the row
  (`applyN 10000 (x -> x + 3) 0`) has no small constructor in it. It did
  not reproduce in the other configurations and the machinery is being
  reverted, so it was not chased; it is noted in case it is a batching
  difference worth finding later.
- Everything else is within 3% in all three tables, and the convention's
  own profile is unchanged from the third round of
  `2026-10-28-wide-jit.md`.

The outcome: the machinery is reverted in the commit after this one. See
the plan (`delayed-data-construction.local.md`, "Wide arguments for known
calls") for the reasoning and the simpler replacement planned for loop
state.
