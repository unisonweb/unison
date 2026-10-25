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

## Phase 2: the MCode verifier

The verifier runs once per group at load time, so the suite's timings can't see it; the one
generated-code change is the packed-tag compare in a lone wide-constructor arm, which the suite's
types (the tree's `Node` has four fields, a `GData4`) never take. Same build settings, two runs
against phase 1's run 2. Every row is within noise.

| phase 1 | run 1 | run 2 | run 2 / phase 1 | Benchmark |
| --- | --- | --- | --- | --- |
| 318.455µs | 318.502µs | 318.5µs | 1.00× | Sum 0 to 1 million |
| 35.402µs | 35.316µs | 35.303µs | 1.00× | fib 20 |
| 13.714µs | 13.544µs | 13.508µs | 0.98× | Cons list: map with a lambda (1000 elements) |
| 4.818µs | 4.823µs | 4.836µs | 1.00× | Cons list: foldLeft with a lambda (1000 elements) |
| 89.338µs | 88.461µs | 88.689µs | 0.99× | Binary tree: 1000 inserts |
| 16.332µs | 16.366µs | 16.244µs | 0.99× | Binary tree: 1000 lookups |
| 57.628µs | 57.642µs | 58.192µs | 1.01× | Apply a function argument 10000 times |
| 34.297µs | 34.238µs | 34.629µs | 1.01× | Mutate a Ref 10000 times |
| 66.31µs | 66.317µs | 66.943µs | 1.01× | Calls across definitions: Collatz steps for 1 to 1000 |
| 118.132µs | 117.898µs | 117.966µs | 1.00× | Calls across definitions: callee first reached once the caller is hot, 100000 iterations |
| 201.855µs | 206.674µs | 202.882µs | 1.01× | Text: append "hi" 10000 times |
| 1.908903ms | 1.898728ms | 1.909619ms | 1.00× | Text: drop 1, 100000 times |
| 197.649µs | 198.209µs | 199.169µs | 1.01× | Bytes: append 2 bytes 10000 times |
| 1.81765ms | 1.856779ms | 1.78908ms | 0.98× | Bytes: drop 1, 100000 times |
| 2.136546ms | 2.102471ms | 2.108839ms | 0.99× | Bytes: at, 100000 times |
| 3.303775ms | 3.301078ms | 3.316181ms | 1.00× | Text: uncons walk over 100000 characters |
| 289.559µs | 288.177µs | 293.611µs | 1.01× | List: cons 10000 elements, then a pattern walk |
| 146.926µs | 144.161µs | 144.216µs | 0.98× | List: at, 10000 times |
| 704.134µs | 704.141µs | 708.681µs | 1.01× | Nat.toText and Nat.fromText, 10000 times |
| 1.001483ms | 1.002066ms | 1.002995ms | 1.00× | Float: sqrt, sin, cos, arithmetic and conversions, 100000 times |
| 485.914µs | 485.734µs | 485.973µs | 1.00× | MutableByteArray: 4 writes and 6 reads, both byte orders, 100000 times |
| 538.041µs | 538.305µs | 539.336µs | 1.00× | FFI.Ptr: 4 writes and 6 reads, 100000 times |
| 45.129µs | 44.006µs | 43.922µs | 0.97× | MutableArray: fill, freeze and sum 10000 elements |
| 127.28µs | 127.102µs | 128.059µs | 1.01× | Ref.cas loop, 10000 times |
| 275.517µs | 277.97µs | 280.085µs | 1.02× | murmurHashUntyped of Some (i, "x"), 10000 times |
| 7.609693ms | 7.508187ms | 7.503515ms | 0.99× | Records: a literal, an update, a pattern and two getters, 100000 times |
| 5.002528ms | 4.975328ms | 4.988037ms | 1.00× | Records: a 16-field literal and four getters, 100000 times |
| 2.550305ms | 2.58206ms | 2.5714ms | 1.01× | Counter: 100000 requests |
| 4.902871ms | 4.922131ms | 4.924062ms | 1.00× | Store: get and put, 100000 iterations |
| 4.206884ms | 4.203215ms | 4.206575ms | 1.00× | handle installed per iteration, 10000 iterations |
| 7.917331ms | 7.930878ms | 7.989378ms | 1.01× | Store whose put requests an outer Counter, 100000 iterations |
| 470.031µs | 452.025µs | 454.728µs | 0.97× | Bytes: decodeNat64be walk over 80000 bytes |

## Phase 3: the fences

The cell's code pointer is published with an atomic release store and read with atomic loads
(`readNativeCode`, `readNativeEntry`; generated code's cell loads are `load atomic ... acquire`).
On arm64 that is an `ldar` where a plain `ldr` was, on every interpreter entry into native code and
every native call through a cell. Same build settings, two runs against phase 2's run 2. Every
row is within noise: the two that move most, the function-argument row at 0.91× and Collatz at
1.04×, are call-heavy rows that swing both ways between runs of the same binary (the first was
57.6 and 58.2 µs in the two phase 2 runs, Collatz 66.3 and 66.9).

| phase 2 | run 1 | run 2 | run 2 / phase 2 | Benchmark |
| --- | --- | --- | --- | --- |
| 318.5µs | 317.289µs | 318.856µs | 1.00× | Sum 0 to 1 million |
| 35.303µs | 35.348µs | 35.409µs | 1.00× | fib 20 |
| 13.508µs | 13.728µs | 13.492µs | 1.00× | Cons list: map with a lambda (1000 elements) |
| 4.836µs | 4.847µs | 4.835µs | 1.00× | Cons list: foldLeft with a lambda (1000 elements) |
| 88.689µs | 90.046µs | 90.885µs | 1.02× | Binary tree: 1000 inserts |
| 16.244µs | 15.567µs | 16.678µs | 1.03× | Binary tree: 1000 lookups |
| 58.192µs | 52.571µs | 52.788µs | 0.91× | Apply a function argument 10000 times |
| 34.629µs | 34.741µs | 35.057µs | 1.01× | Mutate a Ref 10000 times |
| 66.943µs | 70.184µs | 69.465µs | 1.04× | Calls across definitions: Collatz steps for 1 to 1000 |
| 117.966µs | 117.957µs | 117.925µs | 1.00× | Calls across definitions: callee first reached once the caller is hot, 100000 iterations |
| 202.882µs | 202.872µs | 204.624µs | 1.01× | Text: append "hi" 10000 times |
| 1.909619ms | 1.900563ms | 1.903635ms | 1.00× | Text: drop 1, 100000 times |
| 199.169µs | 198.327µs | 199.673µs | 1.00× | Bytes: append 2 bytes 10000 times |
| 1.78908ms | 1.805201ms | 1.811546ms | 1.01× | Bytes: drop 1, 100000 times |
| 2.108839ms | 2.107582ms | 2.11376ms | 1.00× | Bytes: at, 100000 times |
| 3.316181ms | 3.272406ms | 3.295259ms | 0.99× | Text: uncons walk over 100000 characters |
| 293.611µs | 286.992µs | 289.085µs | 0.98× | List: cons 10000 elements, then a pattern walk |
| 144.216µs | 144.686µs | 143.874µs | 1.00× | List: at, 10000 times |
| 708.681µs | 706.449µs | 728.512µs | 1.03× | Nat.toText and Nat.fromText, 10000 times |
| 1.002995ms | 1.002153ms | 1.000859ms | 1.00× | Float: sqrt, sin, cos, arithmetic and conversions, 100000 times |
| 485.973µs | 486.267µs | 486.316µs | 1.00× | MutableByteArray: 4 writes and 6 reads, both byte orders, 100000 times |
| 539.336µs | 539.302µs | 540.428µs | 1.00× | FFI.Ptr: 4 writes and 6 reads, 100000 times |
| 43.922µs | 43.999µs | 44.25µs | 1.01× | MutableArray: fill, freeze and sum 10000 elements |
| 128.059µs | 126.958µs | 128.04µs | 1.00× | Ref.cas loop, 10000 times |
| 280.085µs | 276.719µs | 279.239µs | 1.00× | murmurHashUntyped of Some (i, "x"), 10000 times |
| 7.503515ms | 7.411846ms | 7.453081ms | 0.99× | Records: a literal, an update, a pattern and two getters, 100000 times |
| 4.988037ms | 4.985656ms | 5.005653ms | 1.00× | Records: a 16-field literal and four getters, 100000 times |
| 2.5714ms | 2.627453ms | 2.576553ms | 1.00× | Counter: 100000 requests |
| 4.924062ms | 4.998143ms | 4.930631ms | 1.00× | Store: get and put, 100000 iterations |
| 4.206575ms | 4.222028ms | 4.222853ms | 1.00× | handle installed per iteration, 10000 iterations |
| 7.989378ms | 8.07319ms | 8.032331ms | 1.01× | Store whose put requests an outer Counter, 100000 iterations |
| 454.728µs | 453.055µs | 455.901µs | 1.00× | Bytes: decodeNat64be walk over 80000 bytes |

## Phase 4: field counts from the object

The generator no longer reads constructor arities from type declarations: each match arm carries
the number of fields it binds, the dispatch is over the arms' shapes, and a wide constructor's arm
checks the object's two array lengths (`requireWide`) before reading. For a type whose constructors
all have arms and whose match has no default the generated dispatch is the same as before: the
pointer tag selects the shape and a shape with one arm branches straight to it. A match with a
default arm now loads the packed tag for a shape with one arm (a constructor without an arm could
share the shape), where before the declaration said whether one did; and a wide arm adds two loads
and a compare. The interpreter's `dataBranch` makes room for a wide object's fields before pushing
them (finding 14), one compare on its `DataG` path. Same build settings, two runs against phase 3's
run 2. Every row is within noise: the list map at 1.05× moved in run 2 only (13.7 µs in run 1,
against 13.5), and the function-argument and Collatz rows are the call-heavy ones that swing both
ways between runs of one binary (phase 3 saw 0.91× and 1.04× on them). The tree rows, whose `Node`
match has four fields and no default, are unchanged at 0.98× and 0.99×.

| phase 3 | run 1 | run 2 | run 2 / phase 3 | Benchmark |
| --- | --- | --- | --- | --- |
| 318.856µs | 319.505µs | 318.734µs | 1.00× | Sum 0 to 1 million |
| 35.409µs | 35.357µs | 35.328µs | 1.00× | fib 20 |
| 13.492µs | 13.733µs | 14.158µs | 1.05× | Cons list: map with a lambda (1000 elements) |
| 4.835µs | 4.842µs | 4.837µs | 1.00× | Cons list: foldLeft with a lambda (1000 elements) |
| 90.885µs | 88.842µs | 88.789µs | 0.98× | Binary tree: 1000 inserts |
| 16.678µs | 16.29µs | 16.568µs | 0.99× | Binary tree: 1000 lookups |
| 52.788µs | 52.629µs | 54.553µs | 1.03× | Apply a function argument 10000 times |
| 35.057µs | 34.462µs | 35.104µs | 1.00× | Mutate a Ref 10000 times |
| 69.465µs | 67.052µs | 67.215µs | 0.97× | Calls across definitions: Collatz steps for 1 to 1000 |
| 117.925µs | 117.862µs | 118.011µs | 1.00× | Calls across definitions: callee first reached once the caller is hot, 100000 iterations |
| 204.624µs | 201.167µs | 202.327µs | 0.99× | Text: append "hi" 10000 times |
| 1.903635ms | 1.884282ms | 1.90237ms | 1.00× | Text: drop 1, 100000 times |
| 199.673µs | 197.478µs | 196.884µs | 0.99× | Bytes: append 2 bytes 10000 times |
| 1.811546ms | 1.819499ms | 1.798735ms | 0.99× | Bytes: drop 1, 100000 times |
| 2.11376ms | 2.107529ms | 2.110619ms | 1.00× | Bytes: at, 100000 times |
| 3.295259ms | 3.247696ms | 3.277156ms | 0.99× | Text: uncons walk over 100000 characters |
| 289.085µs | 287.432µs | 286.606µs | 0.99× | List: cons 10000 elements, then a pattern walk |
| 143.874µs | 144.202µs | 144.664µs | 1.01× | List: at, 10000 times |
| 728.512µs | 705.574µs | 707.832µs | 0.97× | Nat.toText and Nat.fromText, 10000 times |
| 1.000859ms | 1.002855ms | 1.004889ms | 1.00× | Float: sqrt, sin, cos, arithmetic and conversions, 100000 times |
| 486.316µs | 488.388µs | 486.917µs | 1.00× | MutableByteArray: 4 writes and 6 reads, both byte orders, 100000 times |
| 540.428µs | 539.133µs | 537.82µs | 1.00× | FFI.Ptr: 4 writes and 6 reads, 100000 times |
| 44.25µs | 44.637µs | 44.109µs | 1.00× | MutableArray: fill, freeze and sum 10000 elements |
| 128.04µs | 126.657µs | 126.747µs | 0.99× | Ref.cas loop, 10000 times |
| 279.239µs | 276.003µs | 277.608µs | 0.99× | murmurHashUntyped of Some (i, "x"), 10000 times |
| 7.453081ms | 7.432746ms | 7.474281ms | 1.00× | Records: a literal, an update, a pattern and two getters, 100000 times |
| 5.005653ms | 5.017143ms | 4.975712ms | 0.99× | Records: a 16-field literal and four getters, 100000 times |
| 2.576553ms | 2.618291ms | 2.575792ms | 1.00× | Counter: 100000 requests |
| 4.930631ms | 4.939056ms | 4.91015ms | 1.00× | Store: get and put, 100000 iterations |
| 4.222853ms | 4.218581ms | 4.213684ms | 1.00× | handle installed per iteration, 10000 iterations |
| 8.032331ms | 7.960668ms | 7.951003ms | 0.99× | Store whose put requests an outer Counter, 100000 iterations |
| 455.901µs | 451.187µs | 452.838µs | 0.99× | Bytes: decodeNat64be walk over 80000 bytes |

## Phase 4, second step: the arm carries its field count, and the interpreter checks it

The per-arm field count moved from a map on `DMatch`/`RMatch` into the branch itself (`Arm` in
`GBranch`, unpacked into `Test1`/`Test2`), and the interpreter's `dataBranch` now runs an arm only
when the object has the arm's count, dying otherwise: one integer compare on every interpreted data
match, against a count already in the constructor it just matched on. Native code is unchanged in
what it emits. Two runs with the JIT on against phase 4's run 2, and one run with the JIT off on
each of the two binaries (the interpreter alone, where the compare lives). Every row is within
noise in both: the `on` rows that move most (list map 0.95×, tree lookups and the function argument
0.96×) are the ones that swung the other way in the previous step, and the `off` rows are all within
3%, with the compare's path (the tree rows, records, the Store handler) if anything slightly faster.

| phase 4 | run 1 | run 2 | run 2 / phase 4 | off, before | off, after | after / before | Benchmark |
| --- | --- | --- | --- | --- | --- | --- | --- |
| 318.734µs | 314.677µs | 318.487µs | 1.00× | 63.30065ms | 62.7962ms | 0.99× | Sum 0 to 1 million |
| 35.328µs | 35.428µs | 35.306µs | 1.00× | 1.177063ms | 1.171196ms | 1.00× | fib 20 |
| 14.158µs | 13.385µs | 13.506µs | 0.95× | 82.407µs | 82.421µs | 1.00× | Cons list: map with a lambda (1000 elements) |
| 4.837µs | 4.833µs | 4.877µs | 1.01× | 85.697µs | 85.146µs | 0.99× | Cons list: foldLeft with a lambda (1000 elements) |
| 88.789µs | 91.673µs | 90.065µs | 1.01× | 1.201395ms | 1.180632ms | 0.98× | Binary tree: 1000 inserts |
| 16.568µs | 15.789µs | 15.851µs | 0.96× | 613.505µs | 593.386µs | 0.97× | Binary tree: 1000 lookups |
| 54.553µs | 52.468µs | 52.138µs | 0.96× | 1.050728ms | 1.039413ms | 0.99× | Apply a function argument 10000 times |
| 35.104µs | 34.625µs | 33.885µs | 0.97× | 803.772µs | 800.182µs | 1.00× | Mutate a Ref 10000 times |
| 67.215µs | 66.32µs | 67.73µs | 1.01× | 6.934906ms | 6.899359ms | 0.99× | Calls across definitions: Collatz steps for 1 to 1000 |
| 118.011µs | 118.235µs | 117.89µs | 1.00× | 15.17ms | 15.050737ms | 0.99× | Calls across definitions: callee first reached once the caller is hot, 100000 iterations |
| 202.327µs | 201.931µs | 199.833µs | 0.99× | 900.433µs | 891.89µs | 0.99× | Text: append "hi" 10000 times |
| 1.90237ms | 1.886173ms | 1.91196ms | 1.01× | 9.040559ms | 9.0045ms | 1.00× | Text: drop 1, 100000 times |
| 196.884µs | 197.567µs | 195.455µs | 0.99× | 861.3µs | 861.823µs | 1.00× | Bytes: append 2 bytes 10000 times |
| 1.798735ms | 1.812005ms | 1.791107ms | 1.00× | 8.249212ms | 8.154475ms | 0.99× | Bytes: drop 1, 100000 times |
| 2.110619ms | 2.149562ms | 2.152275ms | 1.02× | 10.259859ms | 10.338862ms | 1.01× | Bytes: at, 100000 times |
| 3.277156ms | 3.293906ms | 3.267387ms | 1.00× | 10.391171ms | 10.437871ms | 1.00× | Text: uncons walk over 100000 characters |
| 286.606µs | 287.127µs | 283.528µs | 0.99× | 1.223575ms | 1.238782ms | 1.01× | List: cons 10000 elements, then a pattern walk |
| 144.664µs | 148.052µs | 147.859µs | 1.02× | 1.37303ms | 1.374357ms | 1.00× | List: at, 10000 times |
| 707.832µs | 704.595µs | 709.547µs | 1.00× | 7.886315ms | 7.910312ms | 1.00× | Nat.toText and Nat.fromText, 10000 times |
| 1.004889ms | 1.001575ms | 1.002106ms | 1.00× | 30.218675ms | 30.10115ms | 1.00× | Float: sqrt, sin, cos, arithmetic and conversions, 100000 times |
| 486.917µs | 485.869µs | 485.806µs | 1.00× | 68.6044ms | 68.60235ms | 1.00× | MutableByteArray: 4 writes and 6 reads, both byte orders, 100000 times |
| 537.82µs | 538.083µs | 538.011µs | 1.00× | 33.7337ms | 33.698512ms | 1.00× | FFI.Ptr: 4 writes and 6 reads, 100000 times |
| 44.109µs | 44.889µs | 43.859µs | 0.99× | 4.031565ms | 3.961193ms | 0.98× | MutableArray: fill, freeze and sum 10000 elements |
| 126.747µs | 127.128µs | 128.155µs | 1.01× | 1.042386ms | 1.033864ms | 0.99× | Ref.cas loop, 10000 times |
| 277.608µs | 277.583µs | 275.717µs | 0.99× | 7.150296ms | 7.149296ms | 1.00× | murmurHashUntyped of Some (i, "x"), 10000 times |
| 7.474281ms | 7.412787ms | 7.396906ms | 0.99× | 94.12865ms | 93.5105ms | 0.99× | Records: a literal, an update, a pattern and two getters, 100000 times |
| 4.975712ms | 4.969465ms | 5.025356ms | 1.01× | 73.92315ms | 72.1313ms | 0.98× | Records: a 16-field literal and four getters, 100000 times |
| 2.575792ms | 2.596578ms | 2.606788ms | 1.01× | 15.273612ms | 15.078462ms | 0.99× | Counter: 100000 requests |
| 4.91015ms | 4.893693ms | 4.810621ms | 0.98× | 25.828412ms | 25.642912ms | 0.99× | Store: get and put, 100000 iterations |
| 4.213684ms | 4.198965ms | 4.210153ms | 1.00× | 6.026128ms | 6.050846ms | 1.00× | handle installed per iteration, 10000 iterations |
| 7.951003ms | 7.968584ms | 7.981518ms | 1.00× | 38.381175ms | 37.26085ms | 0.97× | Store whose put requests an outer Counter, 100000 iterations |
| 452.838µs | 453.093µs | 448.236µs | 0.99× | 1.539753ms | 1.552181ms | 1.01× | Bytes: decodeNat64be walk over 80000 bytes |
