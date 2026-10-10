# Where the backend's time goes (2026-10-10)

Code generation has been over half of a module's compile time through every step of the
compile-time work ([compile time](2026-10-09-compile-time.md), [O0](2026-10-09-o0.md),
[pipeline](2026-10-09-pipeline.md), [exit call](2026-10-09-exit-call.md)), and it barely
moved when the exit call took 40% of the optimized IR away. This is a sampling profile of
it: the scratch harness (a C program that links `jit_llvm.c` and compiles one dumped module
over and over, `development.md`) under macOS `sample` at 1 ms, with the default pipeline,
on three modules of the suite as dumped by `--jit on`. The "first lookup" phase of the
harness is what the stats line calls code generation: ORC materializes the module, runs the
codegen pass pipeline, writes the object, links it.

| module | optimized IR | parse and passes | code generation |
| --- | --- | --- | --- |
| `unison_u657_22_x7` (largest, rope and decode code) | 7,422 lines | 35 ms | 110 ms |
| `unison_765` (Collatz) | 653 lines | 4.7 ms | 6.3 ms |
| `unison_835` (murmur) | 421 lines | 3.7 ms | 4.7 ms |

## The split, by leaf sample under the first lookup

| | largest | Collatz | murmur |
| --- | --- | --- | --- |
| instruction selection (`SelectionDAGISel`) | 27% | 21% | 22% |
| register allocation (`RAGreedy`, coalescer, live intervals, rewriter) | 25% | 20% | 25% |
| the other machine passes | 24% | 34% | 29% |
| pass manager overhead (analysis bookkeeping between passes) | 14% | 7% | 6% |
| malloc and free | 9% | 15% | 13% |
| MC object emission | 1% | 2% | 1% |
| JITLink (allocate, relocate, protect) | 0.1% | 1% | 1% |

Among the other machine passes, nothing stands out: `AArch64LoadStoreOpt` is 5% of the
largest module, then the coalescer, live variables, copy propagation, sinking, branch
folding, prologue/epilogue insertion, at 1–2% each, and a long tail. Inside instruction
selection, `SelectionDAG::Combine` is a third, legalization and scheduling a tenth each.

## What this says

- **It is proportional to code, not fixed.** 110 ms for 7,400 lines and 4.7 ms for 420 is
  10–15 µs per optimized line in both. Linking, object writing and per-module setup are
  under 3% together, so there is no fixed per-module cost worth attacking (the ~2 ms
  measured in [O0](2026-10-09-o0.md) is real but small).
- **The passes are the ordinary ones, and each is a few percent.** No single pass is a
  target; `-O1` for the backend would skip some of the machine passes but the
  [O0](2026-10-09-o0.md) measurement already showed the generated code pays for that.
- **The remaining levers are upstream of the backend**: fewer lines reaching it (what the
  live sets, the pipeline and the exit call did) or fewer modules compiled (the batch rule,
  the exit rule). The hot functions' own code is what is left, and that is what the backend
  exists to compile well.
- **The write-back stores were never the backend's cost**, which closes the question of
  whether `llvm.experimental.deoptimize` with its assembly would have been worth it for
  compile time: the exit call removed them from the IR and the backend's time stayed.

## How to reproduce

`UNISON_JIT_LLVM_OPTS` passes further LLVM command line options at startup (`-time-passes`
prints nothing from ucm, since the process parks at exit before the timers report; the
sampling profile is the way). With a module dumped by `--jit-dump-ir`:

```
cd scratchpad/harness && cc -O1 -o harness harness.c $UNISON/unison-runtime/cbits/jit_llvm.c -I$UNISON/unison-runtime/cbits
./harness module.ll u 60 "$PIPELINE" & sample $! 8 -mayDie -file out.txt
python3 agg.py out.txt
```

`agg.py` (in the scratchpad) reads `sample`'s call tree and attributes each leaf sample to
the nearest matching ancestor: a legacy pass manager child for the per-pass table, or a
category for the split above.
