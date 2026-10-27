# The wide calling convention and delayed construction in the interpreter

2026-10-27, `jitSuite` on the optimized build, `UNISON_JIT=off`, `UNISON_WIDE=off` against `UNISON_WIDE=on` (steps 3 and 4 of delayed-data-construction.local.md, at commit `7032c6c6f` plus the base-writing tail constructor). Medians of one run each; the machine was idle. Ratio is off over on, so below 1 is slower under the convention.

| row | off (ms) | wide (ms) | ratio |
|---|---|---|---|
| Sum 0 to 1 million | 63.41 | 63.43 | 1.00x |
| fib 20 | 1.40 | 1.45 | 0.97x |
| Cons list: map with a lambda (1000 elements) | 0.10 | 0.12 | 0.82x |
| Cons list: foldLeft with a lambda (1000 elements) | 0.09 | 0.10 | 0.97x |
| Binary tree: 1000 inserts | 1.29 | 1.32 | 0.98x |
| Binary tree: 1000 lookups | 0.62 | 0.62 | 0.99x |
| Cons list: filterMap with a lambda returning Optional (1000 elements) | 0.11 | 0.12 | 0.91x |
| Cons list: head, 10000 times | 1.13 | 1.16 | 0.97x |
| State tuple through a non-tail call, 10000 steps | 1.30 | 1.35 | 0.97x |
| Ok or Err from a step function, 10000 steps | 1.46 | 1.50 | 0.98x |
| Apply a function argument 10000 times | 1.15 | 1.18 | 0.98x |
| Mutate a Ref 10000 times | 0.80 | 0.80 | 1.00x |
| Calls across definitions: Collatz steps for 1 to 1000 | 7.48 | 7.61 | 0.98x |
| Calls across definitions: callee first reached once the caller is hot, 100000 iterations | 16.96 | 17.31 | 0.98x |
| Text: append "hi" 10000 times | 0.89 | 0.89 | 1.00x |
| Text: drop 1, 100000 times | 9.06 | 9.07 | 1.00x |
| Bytes: append 2 bytes 10000 times | 0.87 | 0.87 | 1.00x |
| Bytes: drop 1, 100000 times | 8.22 | 8.22 | 1.00x |
| Bytes: at, 100000 times | 10.73 | 10.41 | 1.03x |
| Text: uncons walk over 100000 characters | 11.47 | 11.53 | 0.99x |
| List: cons 10000 elements, then a pattern walk | 1.25 | 1.25 | 1.00x |
| List: at, 10000 times | 1.49 | 1.52 | 0.98x |
| Nat.toText and Nat.fromText, 10000 times | 7.94 | 7.97 | 1.00x |
| Float: sqrt, sin, cos, arithmetic and conversions, 100000 times | 30.08 | 30.07 | 1.00x |
| MutableByteArray: 4 writes and 6 reads, both byte orders, 100000 times | 68.71 | 68.76 | 1.00x |
| FFI.Ptr: 4 writes and 6 reads, 100000 times | 33.80 | 33.88 | 1.00x |
| MutableArray: fill, freeze and sum 10000 elements | 3.97 | 3.88 | 1.02x |
| Ref.cas loop, 10000 times | 1.06 | 1.06 | 1.00x |
| murmurHashUntyped of Some (i, "x"), 10000 times | 7.31 | 7.39 | 0.99x |
| Records: a literal, an update, a pattern and two getters, 100000 times | 98.35 | 98.94 | 0.99x |
| Records: a 16-field literal and four getters, 100000 times | 76.00 | 77.13 | 0.99x |
| Counter: 100000 requests | 16.14 | 16.37 | 0.99x |
| Store: get and put, 100000 iterations | 27.17 | 27.56 | 0.99x |
| handle installed per iteration, 10000 iterations | 6.43 | 6.56 | 0.98x |
| Store whose put requests an outer Counter, 100000 iterations | 40.14 | 40.97 | 0.98x |
| Bytes: decodeNat64be walk over 80000 bytes | 1.62 | 1.60 | 1.01x |

The interpreter gains nothing from keeping constructors out of the heap: GHC's allocation is a pointer bump, so what the convention saves there is less than the slot traffic it adds (three slots moved where one was, a witness written, a boxed object built at the escape), and a constructor that escapes right after the call (`Cons.map`: every cell goes into the next) pays the traffic for nothing. The rows the convention targets (head, the state tuple, Ok or Err) are within 3% once the tail constructor writes its slots at the frame's base. The gain is the JIT's, where the slots are registers and the match on a known tag folds away (step 5).
