# 2026-10-24: the security review's runtime checks

Phase 1 of the fixes from the security review (security-review.local.md and
security-fixes-plan.local.md next to the JIT): the closure call's stack-room check measured from the
copy's base, the array bounds checks that no longer add offset and length, the frame-record buffer
checked before it is written, `allocateMightFail` for program-sized objects, `PinnedByteArray.contents`
checking pinnedness, and the small items (case mapping by arithmetic, the MutVar barrier called only
for a clean variable, exact list allocation sizes, probe and dump housekeeping). Each adds a compare
to a path that already made a call or an exit, so nothing was expected to move.

## The suite

`jit-benchmarks.md`, optimized build, `--jit on`, two runs against run 2 of
[2026-10-21 match dispatch](2026-10-21-match-dispatch.md). Everything is within noise except
"Mutate a Ref", 0.83×: `unison_jit_write_mutvar` used to call the runtime's `dirty_MUT_VAR` on every
write and now calls it only when the variable is clean, as compiled Haskell does, so the hot loop's
writes to an already dirty `Ref` skip the call. The request rows (Counter, Store) read 2 to 4% faster
in both runs, which is at the edge of their day-to-day variation.

| before | run 1 | run 2 | run 2 / before | Benchmark |
| --- | --- | --- | --- | --- |
| 317.591µs | 318.787µs | 318.455µs | 1.00× | Sum 0 to 1 million |
| 35.375µs | 35.336µs | 35.402µs | 1.00× | fib 20 |
| 13.412µs | 13.618µs | 13.714µs | 1.02× | Cons list: map with a lambda (1000 elements) |
| 4.845µs | 4.85µs | 4.818µs | 0.99× | Cons list: foldLeft with a lambda (1000 elements) |
| 89.523µs | 89.175µs | 89.338µs | 1.00× | Binary tree: 1000 inserts |
| 16.18µs | 15.682µs | 16.332µs | 1.01× | Binary tree: 1000 lookups |
| 57.622µs | 57.638µs | 57.628µs | 1.00× | Apply a function argument 10000 times |
| 41.453µs | 34.637µs | 34.297µs | 0.83× | Mutate a Ref 10000 times |
| 70.103µs | 66.335µs | 66.31µs | 0.95× | Calls across definitions: Collatz steps for 1 to 1000 |
| 117.869µs | 117.906µs | 118.132µs | 1.00× | Calls across definitions: callee first reached once the caller is hot, 100000 iterations |
| 203.574µs | 202.38µs | 201.855µs | 0.99× | Text: append "hi" 10000 times |
| 1.905517ms | 1.895757ms | 1.908903ms | 1.00× | Text: drop 1, 100000 times |
| 204.085µs | 198.159µs | 197.649µs | 0.97× | Bytes: append 2 bytes 10000 times |
| 1.812338ms | 1.797147ms | 1.81765ms | 1.00× | Bytes: drop 1, 100000 times |
| 2.246209ms | 2.12155ms | 2.136546ms | 0.95× | Bytes: at, 100000 times |
| 3.235325ms | 3.285412ms | 3.303775ms | 1.02× | Text: uncons walk over 100000 characters |
| 295.221µs | 286.701µs | 289.559µs | 0.98× | List: cons 10000 elements, then a pattern walk |
| 146.537µs | 144.104µs | 146.926µs | 1.00× | List: at, 10000 times |
| 719.368µs | 706.483µs | 704.134µs | 0.98× | Nat.toText and Nat.fromText, 10000 times |
| 1.00326ms | 1.002447ms | 1.001483ms | 1.00× | Float: sqrt, sin, cos, arithmetic and conversions, 100000 times |
| 486.336µs | 485.99µs | 485.914µs | 1.00× | MutableByteArray: 4 writes and 6 reads, both byte orders, 100000 times |
| 538.875µs | 541.351µs | 538.041µs | 1.00× | FFI.Ptr: 4 writes and 6 reads, 100000 times |
| 44.488µs | 44.154µs | 45.129µs | 1.01× | MutableArray: fill, freeze and sum 10000 elements |
| 129.049µs | 127.55µs | 127.28µs | 0.99× | Ref.cas loop, 10000 times |
| 281.176µs | 276.856µs | 275.517µs | 0.98× | murmurHashUntyped of Some (i, "x"), 10000 times |
| 7.539168ms | 7.4094ms | 7.609693ms | 1.01× | Records: a literal, an update, a pattern and two getters, 100000 times |
| 5.040006ms | 5.022953ms | 5.002528ms | 0.99× | Records: a 16-field literal and four getters, 100000 times |
| 2.695991ms | 2.562547ms | 2.550305ms | 0.95× | Counter: 100000 requests |
| 5.197637ms | 4.8682ms | 4.902871ms | 0.94× | Store: get and put, 100000 iterations |
| 4.216646ms | 4.187468ms | 4.206884ms | 1.00× | handle installed per iteration, 10000 iterations |
| 8.646934ms | 7.903912ms | 7.917331ms | 0.92× | Store whose put requests an outer Counter, 100000 iterations |
| 450.714µs | 470.624µs | 470.031µs | 1.04× | Bytes: decodeNat64be walk over 80000 bytes |
