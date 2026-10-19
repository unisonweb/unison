# Working on the JIT

How to build, run, test and look inside the JIT. The [design](design.md) says what it is and how each part works, and [usage.md](usage.md) is the user's side (turning it on, installing LLVM, the settings); this is the developer's side. Numbers from past runs are in [benchmarks/](benchmarks/). [Part 1](#part-1-cheat-sheet) is the commands; [Part 2](#part-2-details) says why, and what goes wrong.

## Part 1: Cheat sheet

**Build.** Iterate with the fast tree; benchmark only with the optimized one, which lives in its own work dir so that switching never rebuilds:

```bash
stack build --fast                                  # iterate
stack build --work-dir .stack-work-opt              # benchmark
stack exec --work-dir .stack-work-opt unison -- ... # run the optimized binary
```

**Test**, from the repo root; the output must equal the file:

```bash
stack exec unison -- -C "$(scripts/testing-codebase.sh jit)" transcript.fork unison-src/transcripts/idempotent/jit-tests.md
```

Run it with `UNISON_JIT=off`, `on` and `eager` at least, and the [matrix](#the-test-matrix) before a commit that touches the generator or a helper:

```bash
scripts/jit-test-matrix.sh            # every row, fast build; --opt, --debug, --list, or row names
``` A matching output says nothing about whether the JIT was on: run with `UNISON_JIT_LOG=1` and look for the compile lines.

**Benchmark**, on the optimized build, on an idle machine, the two suites:

```bash
stack exec --work-dir .stack-work-opt unison -- -C "$(scripts/testing-codebase.sh jit)" transcript.fork unison-src/transcripts-manual/jit-benchmarks.md
stack exec --work-dir .stack-work-opt unison -- -C "$(scripts/testing-codebase.sh jit)" transcript.fork unison-src/transcripts-manual/jit-suite.md
```

Record a run as `benchmarks/<yyyy-mm-dd>-<what>.md`.

**Look inside.** `UNISON_JIT_LOG=1` (what compiled, what didn't and why), `UNISON_JIT_STATS=1` (exit counts per site), `--jit-dump-ir DIR` (the IR). The full list is in [Looking inside](#looking-inside).

**After editing `cbits/jit_helpers.c` or `cbits/jit_internal.h`:** run `unison-runtime/cbits/gen-helpers.sh` and commit the regenerated `.ll`; make some change to `jit_rt.c` too, or the C won't rebuild.

**Never run two Stack builds at once**, and check for a stuck one with `pgrep -fl "stack build|Cabal-simple"`.

**How long things take** (Paul's machine; the fast tree unless said):

| What | Time |
| --- | --- |
| `stack build --fast` after a change in the runtime | 2 to 5 min |
| the same after a change in `unison-util-rope` (every package depends on it) | about 5 min |
| optimized build | about 5 min; debug RTS build about 8 min |
| `jit-tests.md`, JIT on | about 20 s |
| `jit-tests.md`, JIT eager, or a stress row of the matrix | 60 to 85 s (everything is compiled before it runs) |
| `jit-tests.md`, JIT off | about 160 s (the tests are loops) |
| `jit-benchmarks.md`, JIT on | about 5 minutes (each row runs for several seconds); with stdout redirected to a file nothing appears until the end, since it is block-buffered |
| the whole test matrix (`scripts/jit-test-matrix.sh`), fast build | about 20 min |
| `jit-benchmarks.md` (`jitSuite`), optimized | about 1 min |
| `jit-suite.md` (`suite`), optimized | 5.5 to 7 min per mode |
| `jit-replacements.md`, optimized | about 10 min per mode |
| 300,000 random list operations through the helpers (`lists=300000`), optimized | about 35 s |
| a test transcript on the debug RTS (`+RTS -DS`) | 4 to 9 min |
| the rope library's tests; its benchmarks | about 1 min; about 6 min |

## Part 2: Details

### Building

- [The build trees](#the-build-trees): fast, optimized, and the two with the debug RTS.
- [The C side](#the-c-side): the embedded IR helpers, and why a header change alone doesn't rebuild.
- [Stack and Cabal gotchas](#stack-and-cabal-gotchas).

The JIT is always built in, and building needs no LLVM: nothing links against it, and [`jit_llvm.c`](../../../../cbits/jit_llvm.c) carries its own copies of the few C API declarations it uses. LLVM is loaded with `dlopen` when the JIT is turned on; where it is looked for and which versions are accepted is in [usage.md](usage.md#what-it-needs). macOS (arm64 and Intel), Linux (x86-64 and arm64) and Windows are built and tested, the Intel Mac, Linux x86-64 and Windows only in CI (`jit` job of test.yaml) and Linux arm64 also locally in an Ubuntu 24.04 container with gcc and Ubuntu's `llvm-20-dev`, where the whole matrix runs the same way; the test transcript truncates the transcendental functions to nine places because glibc's and macOS's libm differ in the last digit). Nothing in the code generator is written per platform; what does vary is finding LLVM, reading the thread's C stack bounds for the stack guard (`thread_stack_bounds` in `jit_rt.c`: macOS, glibc and Windows bodies), the symbols generated code resolves (none from the process: the runtime hands the JIT every helper's address, see [The IR helpers](design.md#the-ir-helpers)), and whether LLVM guarantees tail calls on the target (it does on arm64 and x86-64 except under the Win64 convention, where workers use a plain `tail call` and check the C stack at entry; `workerCC` in Codegen.hs). The C runtime compiles with any C compiler: the one routine that needs the `preserve_most` convention, the exit routine, is IR the JIT compiles at startup (2026-10-17).

#### The build trees

Stack does not rebuild when only the optimization level changes, so after a `--fast` build a plain `stack build` leaves the unoptimized binary in place. Each configuration therefore has its own work dir; dependencies are shared between them, only the local packages are built twice, and switching rebuilds nothing.

| Work dir | Build | Use |
| --- | --- | --- |
| `.stack-work` | `stack build --fast` | iterating; correctness tests |
| `.stack-work-opt` | `stack build --work-dir .stack-work-opt` | benchmarks, and the correctness tests before believing one |
| `.stack-work-debug` | `stack build --fast --ghc-options=-debug --work-dir .stack-work-debug` | the debug RTS: run with `+RTS -DS -RTS` at the end of the command line and it checks every heap object at every GC, which catches a bad info pointer, a wrong tag or a missed write barrier near its cause (about 10× slower; `unison +RTS --info` says `rts_thr_debug`) |
| `.stack-work-optdebug` | the same over the optimized build | bugs that only exist at `-O2`, where GHC's code trusts strict fields without checking tags |

The quickest way to tell a fast binary from an optimized one: "Count to 1 million" in `suite` takes about 40 ms optimized and about 3 s fast.

`Stack.hs`, `MCode.hs`, `Deque.Internal`, `Rope`, `Util.Text` and `Util.Bytes` carry `{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}` in every build, because the closure layouts the generator and the C helpers rely on only exist in optimized code. Without it a fast build makes the layout probe turn the JIT off, which is correct, and the probe's log line says so.

#### The C side

[`cbits/jit_helpers.c`](../../../../cbits/jit_helpers.c) is also compiled to [`cbits/jit_helpers.ll`](../../../../cbits/jit_helpers.ll), and [`cbits/jit_exit.c`](../../../../cbits/jit_exit.c) (the exit routine, which is never compiled into the library) to `jit_exit.ll`; both `.ll` files are committed and embedded ([The IR helpers](design.md#the-ir-helpers), [Write-back](design.md#write-back)). After editing either `.c`, `cbits/jit_internal.h` or `cbits/jit_rt.h`, run [`unison-runtime/cbits/gen-helpers.sh`](../../../../cbits/gen-helpers.sh) (needs clang; the GHC headers come from `stack exec ghc`) and commit the `.ll` files with the change; `gen-helpers.sh --check` says whether they are in step. It is a developer's step, not CI's (CI would need clang and the GHC headers for a file that rarely changes). The build itself needs no clang: it only embeds the file. The C copy of the hash stays: the startup self-check runs it, and it is the fallback under `UNISON_JIT_DISABLE=irhelpers`.

A change to `cbits/jit_rt.h` or `cbits/jit_internal.h` alone doesn't recompile `cbits/jit_rt.c`: Cabal rebuilds a C source when its own content changes and knows nothing of its includes, and `touch` doesn't count. The stale object then runs with the new Haskell, and a constant raised in the header and in Codegen.hs aborts at the old C bound. Make some change to `jit_rt.c` alongside; a comment will do.

#### Stack and Cabal gotchas

- Never run two Stack builds at once, and never leave a killed one behind: a build killed by a timeout leaves `Cabal-simple` and `ghc` processes holding the build lock, and later builds hang silently on it ("still blocking for directory lock"). Check with `pgrep -fl "stack build|Cabal-simple"`.
- A build can hang at "Configuring unison-runtime" when macOS is waiting for the user to approve a binary that the configure step runs (`pkg-config` was the case); answer the prompt.
- Stack ignores `lib/unison-util-rope/package.yaml` (the checked-in `.cabal` was generated by a newer hpack), so a new module, test or benchmark there goes into the `.cabal` file by hand. `unison-runtime.cabal` is regenerated by Stack's hpack and its version line changes; that is expected.
- Every package depends on `unison-util-rope`, so a change there rebuilds all the local packages in whichever tree is built next.

### Running

- [The transcripts](#the-transcripts): the four files and what each needs.
- [The codebase](#the-codebase): the `jit` testing codebase, where it comes from, and the differences between it and an empty one.

The modes (`--jit off|on|eager`), the settings and the replacements switch are described in [usage.md](usage.md#settings); every setting is an environment variable parsed in [`Config.hs`](Config.hs#:~:text=data%20Config). For tests, `eager` is deterministic and compiles code that never gets hot in a short run; `on` with `UNISON_JIT_THRESHOLD=1` is the most repeatable `on` configuration, since what plain `on` compiles depends on the compile thread racing the program.

#### The transcripts

All run from the repo root with `stack exec unison -- -C "$(scripts/testing-codebase.sh jit)" transcript.fork <file>` (the test transcript needs only builtins, so any up-to-date codebase does for it; without `-C` the home codebase is forked, which fails if that one is waiting to be migrated). `transcript.fork` runs on a throwaway copy of the codebase, so the downloaded one is never modified. Running one writes `<name>.output.md` next to it; for the idempotent one, copy the output over the `.md` when a change is intended, and don't commit stray `.output.md` files.

| File | What it is | Codebase it needs |
| --- | --- | --- |
| `unison-src/transcripts/idempotent/jit-tests.md` | correctness tests with known answers, builtins only, prompt `scratch/main>`; the file contains its own expected output | any, including empty |
| `unison-src/transcripts-manual/jit-benchmarks.md` | `jitSuite`, the benchmarks written for the JIT; prompt `jit-tests/main>`; timings print to the console | the `jit` testing codebase |
| `unison-src/transcripts-manual/jit-suite.md` | the older, broader `suite` (lists, maps, text, JSON, abilities); the test of whether the JIT is usable on library code | the `jit` testing codebase |
| `unison-src/transcripts-manual/jit-replacements.md` | `replacementSuite`, every replaced base operation at three sizes, meant to be run with `UNISON_JIT=off`, `UNISON_JIT=on UNISON_REPLACEMENTS=on` and `UNISON_JIT=on UNISON_REPLACEMENTS=off` (the `time` helper runs each row for several seconds whatever its work) | the `jit` testing codebase |

The base library transcripts (`unison-src/transcripts-using-base/`) run through the transcript runner, `stack exec transcripts -- <prefix>`, which runs every transcript whose file name starts with the prefix with the `_base.md` prelude and checks its `.output.md`, under `UNISON_JIT=off`, `on` and `eager`; the ones that use `Map`, `Value` and hashing are `replacements`, `serial-test`, `all-base-hashes`, `hashing`, `codeops`, `binary-encoding`. Known failure: `serial-test-03` under `eager` fails with "reflectValue: cannot prepare value for serialization: black hole", serializing a value that holds a continuation captured by a non-affine handler (`shift`); `on` and `off` pass it.

`run.compiled`: `unison-src/tests/jit-compiled.md` compiles a program (`jit-compiled.uc`, written to the current directory) whose loop matches on a type of its own; run the file with `unison run.compiled jit-compiled.uc` under `UNISON_JIT=eager UNISON_JIT_LOG=1` and expect the compile lines and `666668416667500000`. The CI `jit` job does this on every platform. The `.uc` format carries the data types' constructor arities since 2026-10-18 and a file is tied to the exact ucm version anyway, so compile and run with the same binary.

A test run can go on a copy of the transcript in another directory, which lets several run at once; when several start at the same moment one can fail at once with an error about `unison.sqlite3-wal` or `credentials.json.lock` (the codebase copy racing another run's), so run that one again.

#### The codebase

The `jit` testing codebase is a release of [unisonweb/testing-codebases](https://github.com/unisonweb/testing-codebases) (`jit-2026-10-19`, 17 MB gzipped). [`scripts/testing-codebase.sh jit`](../../../../../scripts/testing-codebase.sh) prints its directory, under `$TMPDIR/unison-testing-codebases/<sha256 of the archive>`, downloading and unpacking it only when that directory does not exist yet; `TESTING_CODEBASES_DIR` moves the cache. To publish a new version, pack `.unison/v2/unison.sqlite3` as a `.tar.gz`, make a release in that repository with it as the asset, and put the release name and the archive's SHA-256 in the script. It has a project `jit-tests`, branch `main`, with the `@pchiusano/misc-benchmarks` suite (`suite`, `fibsuite`, `fib1`, `time`, `printTime`, `repeat`, ...) and the libraries `unison_base_7_19_2`, `unison_json_1_3_4`, `unison_cloud_20_15_1` and `mitchellwrosen_benchmark_2_0_0`. It is on codebase schema 27; a codebase that needs migrating is migrated by opening it interactively (`UNISON_MIGRATION=auto` skips the prompt), not by `transcript.fork`.

The test transcript runs in an empty codebase after `builtins.mergeio`, where the builtins keep their own names (`Nat.fromText`, not `Text.toNat`); the testing codebase has base, whose wrappers of the same names raise exceptions, so a stanza checked there alone can behave differently from the same stanza in the transcript. Unison gotchas met while writing the tests: with builtins only there is no `Nat.-`, `Nat.==` and so on, so the test transcript defines them in a prelude stanza followed by `update`; bare `==` and `<` resolve to the polymorphic `Universal` versions unless the file starts with `use Nat + - * / == < > <= >=`, and with `use Nat ==` in scope, `==` on anything but Nats won't resolve (write `Universal.==`); definitions in one `unison` stanza are not visible in the next unless a `ucm` stanza runs `update`; a `##`-qualified name at the end of a nested binding doesn't parse (base's `Raw` namespaces name the same builtins).

### Testing

- [The test matrix](#the-test-matrix): the configurations a change to the generator or a helper should pass.
- [What else counts as a test](#what-else-counts-as-a-test).
- [Pitfalls](#pitfalls): the ways a passing test has lied before.

The interpreter is the specification: every test runs the same program with and without the JIT and the output must be identical. Most of the JIT's exit paths are rare in a normal run, so the stress modes force them, and the failure mode of most bugs here is silent heap corruption, so the debug RTS checks the heap.

#### The test matrix

[`scripts/jit-test-matrix.sh`](../../../../../scripts/jit-test-matrix.sh) runs the rows below on `jit-tests.md`, each on its own copy of the transcript, and reports a row as passed when the transcript completed, the output equals the input, and the log shows the JIT in the state the row asks for (compiling, or off). `--opt` and `--debug` select the optimized and debug-RTS trees, `--list` prints the rows, and row names run a subset. Logs of failed rows are kept. All on `jit-tests.md`, output identical each time. The stress modes: `poll=N` fires the preemption poll every N entries; `ustack=N` starts the Unison stack with N slots; `cstack=N` sets the C stack budget to N bytes; `callee=N` treats every N-th callee as not compiled; `alloc=N` makes the allocation budget N words; `pool=N` makes the constant pool's first array N entries so that it grows and functions check its size; `install=N` sleeps N ms before each function of a module is installed; `lists=N`, `texts=N`, `bytes=N` run N random operations through the C helpers at startup, each on the results of earlier ones, checking every result's structure and elements against Haskell.

| Configuration | Exercises |
| --- | --- |
| `UNISON_JIT=off`; `eager`; `on`; `on` with `UNISON_JIT_THRESHOLD=1` | the three modes; with threshold 1 every re-entry point is generated on first use |
| `eager` with `UNISON_JIT_STRESS=` each of `poll=3`, `ustack=4`, `cstack=2048`, `callee=3`, `alloc=64` | each exit path on its own |
| `eager` with `UNISON_JIT_STRESS=callee=2,cstack=4096,poll=5,ustack=4,alloc=64,pool=8` | all of them together ("the combination") |
| `on`, `THRESHOLD=1`, `install=5`, alone and with the combination | code installed while the program runs |
| the stress runs with `UNISON_JIT_EXIT_COST=0 UNISON_JIT_ENTRY_COST=0` | the estimate doesn't hide code from the tests |
| `UNISON_JIT_DISABLE=worker` | the uniform form everywhere |
| `UNISON_JIT_DISABLE=list,text,bytes,name` | the fallback paths of the C helpers |
| `UNISON_JIT_DISABLE=irhelpers`, `=requests`, `=records`, each under the combination | the C copies of the IR helpers; every request exits; every record instruction exits |
| `eager` with `UNISON_JIT_STRESS=lists=20000,texts=20000,bytes=200000` | the helper self-tests; after touching either side of a helper, run a few hundred thousand steps, on the debug RTS too (in `on` mode the checks run on the compile thread and may not finish before a short transcript does; use `eager`) |
| on the debug RTS with `-DS`: `on` with `THRESHOLD=1`; `eager`; `on` with `THRESHOLD=2`, both costs 0 and `install=5,pool=8,alloc=64,poll=7`; and `jit-benchmarks.md` with `on` and `THRESHOLD=1` | heap sanity under each |
| the first few rows on the optimized build | some failures only exist at `-O2`, and the startup checks can fail closed there for reasons the fast build never shows (a sample list GHC floated to a top-level constant) |

#### What else counts as a test

- The benchmark transcript, with the fast binary: base library code has shapes the builtins-only tests don't (a `Let` inside an inline binding was found by `printTime`, not by the tests).
- The rope library's own tests and benchmarks: `stack build --fast --test unison-util-rope` compares the Deque with `Data.Sequence` and the rope with a string model, checking the structures' invariants (`valid`) after every step; `stack build --work-dir .stack-work-opt --bench unison-util-rope` runs the benchmarks (`--ba "--csv FILE"` for the numbers).
- The interpreter's own speed: re-measure `off` whenever [`enter`](../Machine.hs#:~:text=enter%20%3A%3A), [`apply`](../Machine.hs#:~:text=apply%20%3A%3A) or [`yield`](../Machine.hs#:~:text=yield%20%3A%3A) change. The hot-count test is on the path of every call, and a version that compared with a configured threshold made the interpreter 5 to 20% slower with the JIT off.

#### Pitfalls

- **A matching transcript says nothing about whether the JIT was on; the log does.** Run with `UNISON_JIT_LOG=1` and look for "the JIT is off" and for the compile lines. When a run's numbers look like the other mode's, count the "JIT is off" lines before reading the table.
- **Check the executable is the one you think.** When `stack build` fails in the runtime, `stack exec unison` still runs the last executable that built, whose probes describe the old layout.
- **Check the run finished.** A run that dies (an `LLVM ERROR` at module compile, say) leaves the previous `.output.md` in place, and a diff against it passes by accident. Delete the output file before the run, or look for "Completed transcript" in the log.
- **A startup probe's sample values are built in an unoptimized module** ([`Layout.hs`](Layout.hs) under `--fast`), so a `Val`'s type tag bound by `let` is an untagged CAF pointer, a list literal's elements and a `fst` are thunks, and `writeArray arr i . any'` stores a thunk of the coercion; the C side then reports "untagged value" or a failed layout check. Build samples through the `-O2` modules' constructors (`NatVal`, `recordVals . segFromList`, `formData`) and write `evaluate (any' v) >>= writeArray arr i`, as the existing probes do.
- **A pointer tag written as a literal in C is a layout assumption the probes don't check.** When `GPAp` moved past the sixth constructor every data tag changed; the generator took the new tags from the layouts, but six C builders tagged their results with literals, and the list self-check caught it ("splitting a list of 0 elements differs from the interpreter") and turned the JIT off. The tags C writes now arrive from the inits (`LF.data1_tag`, `LF.data2_tag`, `PF_pap_tag`); keep it that way.
- **`LLVM ERROR: Instruction Combining on <helper> did not reach a fixpoint`** at a module compile is LLVM's self-check on the combiner, fatal in the LLVM in use, and it means the helper's IR got harder to optimize, not that the pipeline is wrong: the hash walker with a loop over a dynamic field count inlined into `mh_val` did it, and cost the hash row 40% besides; straight-line code for the common shapes with the loop out of line (`mh_data_wide`) fixed both. `instcombine<no-verify-fixpoint>` in `UNISON_JIT_PASSES` turns the check off if it is ever needed to get a run through.
- **Per-thread state made before the settings are read is a trap:** the self-tests once created the thread's JIT context before the stress settings were configured, and on that thread the stress poll fired on every entry forever. Under the stress modes the C entry now reports a function that leaves with the same exit a million times in a row without moving the stack.
- **A crash in the GC** is usually a bad object native code built or a pointer it stored untagged. The debug RTS over the optimized build puts the fault near its cause; macOS's crash reports (`~/Library/Logs/DiagnosticReports`) show which thread was where at exit time; `lldb` in batch mode needs `--source-on-crash` with the RTS's timer signals passed (`process handle SIGALRM SIGVTALRM SIGUSR2 -s false -n false -p true`), `PAGER=cat` and stdin closed, else it stalls on a prompt. A laziness leak on the compile thread shows in `+RTS -hT` as `:` cells.

### Looking inside

- [Logs and statistics](#logs-and-statistics)
- [Dumps](#dumps): the IR and the MCode.
- [LLVM knobs](#llvm-knobs)
- [Traces and switches](#traces-and-switches)
- [Measuring](#measuring): the techniques that chose things.

#### Logs and statistics

- `UNISON_JIT_LOG=1`: the compile log (per module: functions, compile time, IR size, and the time split into generating the IR, LLVM's parse and optimization, and LLVM's code generation), why a combinator or part of one was left to the interpreter ("partly interpreted"), the startup checks and how long they took, the thread's C stack size, and "the JIT is off" with the reason when a check failed.
- `UNISON_JIT_STATS=1` prints exit counts per site after each evaluation, with the compile totals first (modules, functions, auxiliary functions, re-entry functions generated on demand and never asked for, IR size, time) and how often the trampoline entered each native function; `UNISON_JIT_STATS=each` prints the exits taken since the program last wrote output, after each write, which gives per-benchmark exits for a suite that prints a line per benchmark; `UNISON_JIT_STATS_EVERY=N` also prints every N exits, for evaluations that never finish. A run with statistics is at most 7% slower on the entries that exit most; quote timings from runs without them. The counts for "depth 1000000" are the quickest way to see whether frames are unwinding more than they should.

#### Dumps

- `--jit-dump-ir DIR` (or `UNISON_JIT_DUMP_IR=DIR`) writes one `.ll` file per module into `DIR`, with the MCode of each function as a comment above its IR and a line for each combinator that wasn't compiled and why; `-` prints to stderr. Two versions of each module are written: `unison_<n>.ll` is the text handed to LLVM and `unison_<n>.opt.ll` the module after the optimization passes, which is what runs. For the arm64 assembly: `/opt/homebrew/opt/llvm/bin/llc -O2 unison_<n>.opt.ll -o out.s`. For example: `stack exec unison -- --jit eager --jit-dump-ir - transcript.fork t.md 2> t.ll`. Two dumps from different binaries differ in the info-pointer addresses baked into the IR; mask runs of ten or more digits before comparing them.
- `UNISON_JIT_DUMP_MCODE=1` dumps the MCode as it is loaded. To look at a site named in the statistics (`CIx ... <group> <n>`): run with `UNISON_JIT=off` and this, kill it once loading is done, and search the dump for `<group>:<n>:`.

#### LLVM knobs

- `UNISON_JIT_PASSES` is the LLVM pipeline a module is optimized with (the default is [`defaultPasses`](Config.hs#:~:text=defaultPasses%20%3A%3A), the parts of `default<O2>` our code uses, [2026-10-09 pipeline](benchmarks/2026-10-09-pipeline.md); `default<O2>` for comparison; empty runs none) and `UNISON_JIT_CODEGEN_LEVEL` (0 to 3) the level of the JIT's target machine, for measuring what the optimizer and the backend cost and buy ([2026-10-09 O0](benchmarks/2026-10-09-o0.md)); full O0 with workers crashes, see there.
- `UNISON_JIT_INLINE_THRESHOLD=N` sets LLVM's inliner threshold (default 225), for measurement; `UNISON_JIT_SCHED` (any value) keeps LLVM's machine schedulers on (off by default, [2026-10-09 compile time](benchmarks/2026-10-09-compile-time.md)); `UNISON_JIT_LLVM_OPTS` is any further LLVM command line options, space separated. All go through `LLVMParseCommandLineOptions` at startup, in `jit_llvm.c`. To see where the backend's time goes, sample the harness: [2026-10-10 backend profile](benchmarks/2026-10-10-backend-profile.md).

#### Traces and switches

- `UNISON_JIT_TRACE=1` prints every native entry and exit, on both the C and Haskell sides. Unusable on programs that load base (it exhausts memory during the load); fine on the test transcript. `UNISON_JIT_TRACE_YIELD=1` prints each value native code yields, its pointer tag and closure type with its fields two levels down; cheap, and what found the untagged-pointer crash. `UNISON_JIT_TRACE_TEST=1` prints each startup self-test operation as it runs, for finding one that crashes.
- `UNISON_JIT_EXITS=blocks` makes exits and unwinds write the frame back through generated blocks instead of the one call per site to `unison_jit_exit_frame` that is the default ([Write-back](design.md#write-back); [2026-10-09 exit call](benchmarks/2026-10-09-exit-call.md)). For comparison, and the fallback should the call form misbehave somewhere.
- `UNISON_JIT_BUMP=0` makes every allocation call `allocate` instead of bumping inline, for comparison.
- `UNISON_JIT_DISABLE=...` turns features off one at a time; the list is in [`Config.hs`](Config.hs#:~:text=disabled%20%3A%3A). Bisecting with it is how the exponential generator and the pool's lazy segment boxes were found.

#### Measuring

- **Benchmarking rules.** `suite` is the test of whether the JIT is usable on library code; `jitSuite` is the test of whether it is fast. Measure both, on the optimized build, on an idle machine, never while another job is running. One run of `suite` is not evidence for a 20% change in a 20 ms benchmark; its run-to-run noise is about 5% both ways, and its `Remote.Ref` row is a single run of 20 ms early in the suite that measures where a major GC lands, not the code. The first benchmark's label in the console output is overwritten by the transcript's progress line, so the first timing appears without a label. Record a run as a file in [benchmarks/](benchmarks/) named `<yyyy-mm-dd>-<what-it-measured>.md`: what the run was (build, mode, how many runs, which commit if it matters), the table, then what it showed. The [latest full run of both suites](benchmarks/2026-10-05-every-benchmark-end-of-m7.md) is what the next full one gets compared against; the later files measure one change each.
- **Counting what the interpreter dispatches on:** a `MutablePrimArray` indexed by `base + dataToTag# x` (the `DataToTag` class in `GHC.Exts`), bumped at the top of `eval`, in the `Ins` case and wherever a closure is scrutinized, and a thread that writes it to a file once a second; the indices map back to names through the declaration order. Two hooks and a rebuild of the optimized tree; what chose the constructor orders ([2026-10-15 constructor order](benchmarks/2026-10-15-constructor-order.md)).
- **Profiling the trampoline:** run the transcript in the background, `sample <pid> 5`, and resolve the addresses in the `unison` binary against `nm -n`. Haskell code shows up as offsets from the nearest exported symbol, which is coarse, but the C and runtime functions are named, and those were most of the waste when the round trip was cut in half.
