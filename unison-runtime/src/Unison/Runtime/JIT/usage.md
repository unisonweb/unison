# Using the JIT

The user-facing side of Unison's JIT: how to turn it on, what it needs, what the settings mean, and what to expect. The [design](design.md) says how it works; [development.md](development.md) is for people changing it.

The JIT is experimental. It is off unless asked for, and when it is on and something is missing (no LLVM, a failed startup check) ucm says so and runs everything on the interpreter, exactly as it would with the JIT off. A program's result never depends on whether the JIT is on: anything the JIT can't compile runs interpreted, and the generated code is checked against the interpreter's own implementations at startup.

## Turning it on

```bash
unison --jit on
```

or, equivalently, `UNISON_JIT=on unison`. The flag overrides the variable. The setting applies to everything that `unison` process evaluates through the codebase: `run`, transcripts, and evaluation from the editor. Compiled programs (`run.compiled`) are loaded by a separate path that doesn't give their definitions native code cells, so the JIT does nothing for them yet.

| Mode | What it does |
| --- | --- |
| `off` | The default. Nothing is compiled and LLVM is not loaded. |
| `on` | Compile what gets hot, on a background thread. A definition runs interpreted until it has been called 100 times, is then compiled together with the definitions it calls and is called by, and runs natively from the moment its code is installed. This is the mode meant for use. |
| `eager` | Compile every definition as it is loaded, synchronously, before it runs. Deterministic, and it compiles code that never gets hot in a short run, so it is the mode for testing; loading is slower. |

At startup, with `on` or `eager`, ucm loads LLVM and runs its checks, then prints one line:

```
JIT compilation activated ⚡🚀
```

or, if it can't, the reason, for example `no LLVM 20 or newer found; the JIT is off`, followed by the places it looked. The program runs either way.

## What it needs

**An LLVM shared library, version 20 or newer.** Nothing is linked at build time; `unison` loads `libLLVM` with `dlopen` when the JIT is turned on ([`jit_llvm.c`](../../../../cbits/jit_llvm.c)). The whole of LLVM isn't needed, only the one library (`libLLVM.dylib` on macOS, `libLLVM.so` on Linux), which the packages below provide. Development uses LLVM 23; 20 is the oldest accepted because it is the oldest tested (the CI smoke test installs it).

**A platform the runtime supports.** The CPU must be arm64 or x86-64. The JIT is built and tested on macOS (Apple silicon and Intel), Linux (x86-64 and arm64, Ubuntu 24.04 with gcc and the `llvm-20-dev` package) and Windows (x86-64, with the LLVM release's `LLVM-C.dll`); CI runs the smoke test on all five (2026-10-18). On Windows `UNISON_JIT_DISABLE=irhelpers` is ignored (the C copies of those helpers return their result differently from generated code there), and a tail call between compiled functions can use C stack where the arguments don't fit a sibling call, which the C stack guard bounds, so very deep mutual tail recursion runs partly interpreted there.

### Installing LLVM

| Platform | Install | Where `unison` finds it |
| --- | --- | --- |
| macOS, Homebrew | `brew install llvm` (the current version) or `brew install llvm@20` | `/opt/homebrew/opt/llvm/lib` (Apple silicon) or `/usr/local/opt/llvm/lib` (Intel), then every `llvm@N` keg, newest first |
| macOS, MacPorts | `sudo port install llvm-20` | `/opt/local/libexec/llvm-N/lib`, newest first |
| Debian, Ubuntu | `apt install llvm-20-dev` (or the [apt.llvm.org](https://apt.llvm.org) packages for a newer one; the `-dev` package is what provides the unversioned `libLLVM.so` link the lookup wants) | `/usr/lib/llvm-N/lib/libLLVM.so`, newest first, then the system library directories |
| Other Linux | the distribution's `llvm` or `llvm-libs` package | `/usr/lib64`, `/usr/lib`, `/usr/local/lib` and the multiarch directories, the first `libLLVM*.so` found |
| Windows | the [LLVM release installer](https://github.com/llvm/llvm-project/releases) or `choco install llvm` | `LLVM-C.dll` on the loader's search path, then `C:\Program Files\LLVM\bin\LLVM-C.dll` (the C API is in `LLVM-C.dll` there, not in a `libLLVM`) |
| Anything else | | `UNISON_LLVM_LIB=/path/to/libLLVM.so` |

The lookup order, in full: `UNISON_LLVM_LIB` if set (and only that; a path there that isn't a usable LLVM is an error, not a fallback), then the dynamic loader's own search path (`DYLD_LIBRARY_PATH`, `LD_LIBRARY_PATH`, the system defaults), then the directories in the table. When several versions are installed the newest is taken; set `UNISON_LLVM_LIB` to choose. `UNISON_JIT_LOG=1` prints the version and path that was loaded.

## Settings

Every setting is an environment variable, read once when `unison` starts ([`Config.hs`](Config.hs#:~:text=data%20Config) documents them all). Two also have command-line flags. The ones a user might want:

| Variable | Meaning |
| --- | --- |
| `UNISON_JIT=off\|on\|eager` | The mode, above. Flag: `--jit MODE`. |
| `UNISON_LLVM_LIB=PATH` | The LLVM library to load, instead of searching. |
| `UNISON_JIT_LOG=1` | Log to stderr: the LLVM loaded, the startup checks, and one line per compiled module (which definitions, how long, how much IR), plus what was left to the interpreter and why. The way to confirm the JIT is doing something. |
| `UNISON_JIT_STATS=1` | After each evaluation, print how often native code handed control back to the interpreter, per site, and the compile totals. `=each` prints the counts whenever the program writes output, which gives per-phase counts for a program that prints a line per phase. Costs up to 7% on exit-heavy code. |
| `UNISON_JIT_THRESHOLD=N` | How many interpreted calls make a definition hot (100). Lower it to compile sooner in a short run; `1` compiles everything on first use. |
| `UNISON_REPLACEMENTS=on\|off` | See [the `Map` caveat](#what-to-expect). |
| `--jit-dump-ir DIR` / `UNISON_JIT_DUMP_IR=DIR` | Write the LLVM IR of every compiled module into `DIR`, one `.ll` file per module before and after optimization, with each function's MCode as a comment; `-` prints to stderr. |

The rest are tuning and debugging knobs. They are documented in [`Config.hs`](Config.hs#:~:text=data%20Config) and their use in [development.md](development.md#looking-inside); the names, so that a stray setting can be recognized:

- *Compilation policy:* `UNISON_JIT_BATCH` (definitions per module, 32), `UNISON_JIT_BATCH_GATE` and `UNISON_JIT_SIZE_UNIT` (which neighbours join a batch), `UNISON_JIT_COPY` (inlining compiled callees), `UNISON_JIT_REENTRY_WAIT` (ms), `UNISON_JIT_EXIT_COST` and `UNISON_JIT_ENTRY_COST` (the estimate that leaves exit-heavy or tiny functions interpreted; `0` compiles everything).
- *LLVM:* `UNISON_JIT_PASSES` (the optimization pipeline), `UNISON_JIT_CODEGEN_LEVEL` (0 to 3), `UNISON_JIT_INLINE_THRESHOLD`, `UNISON_JIT_SCHED` (keep LLVM's machine schedulers on), `UNISON_JIT_LLVM_OPTS` (any further LLVM options).
- *Alternative mechanisms, for comparison:* `UNISON_JIT_EXITS=blocks`, `UNISON_JIT_BUMP=0`.
- *Debugging:* `UNISON_JIT_DUMP_MCODE=1`, `UNISON_JIT_TRACE=1`, `UNISON_JIT_TRACE_YIELD=1`, `UNISON_JIT_TRACE_TEST=1`, `UNISON_JIT_STATS_EVERY=N`, `UNISON_JIT_STRESS=...` (force the rare paths, for tests) and `UNISON_JIT_DISABLE=...` (turn features off one at a time, for bisecting).

## What to expect

**What runs natively.** Ordinary code: arithmetic, pattern matching, data construction, calls (known and through function values), tail-recursive loops, and the builtins over lists, text, bytes, arrays, refs, floats, hashing, universal comparison and records. Requests of *affine* handlers (handlers that resume at most once, in tail position: state, exceptions, streams and most others) and their handler code are native too. [builtins.md](builtins.md) lists every builtin and says which are native.

**What stays interpreted.** Delimited continuations (`Capture`, and handlers that resume more than once or out of tail position), `handle` itself, the `IO`, `STM`, `MVar`, `Tls`, `Code`/`Value` and crypto builtins, big integers, text patterns, and any definition the JIT decides isn't worth compiling because it would spend more time crossing back and forth than it saves. Native code hands control to the interpreter for these and takes it back afterwards, so a program that mixes them still gets native code for the rest.

**Speedups.** Loops and straight-line code over numbers, data and the builtins above run several times to tens of times faster than interpreted; code dominated by the interpreted features above runs about as before. The first 100 calls of anything run interpreted in `on` mode, and compilation happens in the background, so a program that runs for well under a second gains little. Compile time is roughly tens of milliseconds per module, and startup with the JIT on costs about a tenth of a second for loading LLVM and running the checks.

**The `Map` caveat.** The interpreter replaces base's `Map`, `Set` and `Multimap` with Haskell implementations. Under the JIT those replacements are switched off, because native Unison code beats the call-out at the sizes programs commonly use, but the Haskell maps win again above a few thousand keys. A program that was fast only because of a huge map can set `UNISON_REPLACEMENTS=on` to get the replacements back under the JIT. (`off` removes all of them, which is for measuring.) Serialized values and hashes are the same either way.

**Memory.** Native code allocates on the Haskell heap and is subject to GC like everything else; there is no separate heap to size. Compiled code lives for the life of the process.

## When something looks wrong

- **Is the JIT actually on?** Look for the "activated" line at startup, or run with `UNISON_JIT_LOG=1` and look for compile lines. A result that matches the interpreter says nothing about whether native code ran.
- **"the JIT is off" at startup** names the reason: no LLVM found (install one, or set `UNISON_LLVM_LIB`), a version older than 20, or a failed startup check. A failed check means the runtime's picture of GHC's data layout or of a builtin's behaviour differs from the running binary's, which is a bug to report, with the message.
- **A wrong result or a crash with the JIT on** that doesn't happen with `UNISON_JIT=off` is a JIT bug. The most useful report is the smallest program that shows it, with the mode, the `UNISON_JIT_LOG=1` output, and whether `UNISON_JIT=eager` shows it too (eager is deterministic; `on` depends on what got hot when).
- **Slower with the JIT than without.** `UNISON_JIT_STATS=1` shows which sites hand control back to the interpreter most; a loop whose every iteration exits is the usual cause, and the site it names says what the loop does that isn't native. Large maps are the other known case, above.
