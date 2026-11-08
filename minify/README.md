# minify

Removes the definitions a GHC-linked executable never uses and compacts the
file. `design.md` explains how; this file says how to run it. It handles
Mach-O (macOS) and ELF (Linux) executables for AArch64 and x86-64.

## Building and running

```
stack build minify
stack exec minify -- INPUT -o OUTPUT [--verify] [--report]
```

The input must be an unstripped executable (the symbol table is what
defines the definitions). A Mach-O output is re-signed ad hoc with
`codesign`. A Linux executable must be linked with its relocations kept,

```
-optl-fuse-ld=bfd -optl-Wl,--emit-relocs
```

(gold crashes on `--emit-relocs`), which gives the tool an exact list of
the pointers in data and of the 32-bit address fields in x86-64 code. A
PIE link also lists the pointers in data. Without either, the pointers
have to be guessed from the static closures, which can misread an integer
as an address or miss a closure shape; `--guess-pointers` allows that, for
debugging only.
Without `-o`, only the report is printed: definitions and bytes kept and
removed per section and per package.

| flag | meaning |
|---|---|
| `--remove PREFIX` | cut every definition whose name starts with PREFIX, then whatever only they referred to; repeatable |
| `--keep-dead` | compact without removing unreferenced definitions (only `--remove` applies) |
| `--verify` | analyze the output and check every reference against the plan |
| `--holes N` | drop at most N runs of dead pages from the file (default 200) |
| `--text-in-order` | keep the segments in address order in the load commands, which limits the holes to 11 |
| `--dump NAME` | print one definition's references and stop |
| `--top N` | packages listed in the report |
| `--keep-haskell-exports` | keep Haskell definitions in the export trie, the table `dlsym` searches; by default only C symbols stay exported |
| `--keep-con-desc` | keep constructor name strings (heap profiling, `GHC.Exts.Heap`); dropped by default |
| `--no-fold` | do not fold identical code and strings onto one copy |
| `--no-split SECTION` | do not split definitions at local closures (`text`), static closures (`data`) or constructor names (`strings`) |
| `--strip` | drop the symbol table and function starts; saves about 24 MB on unison, but the output cannot be minified again or symbolicated |
| `--force` | write the output even when the analysis reported anomalies (code it did not understand) |
| `--guess-pointers` | write an output for a Linux executable that has no relocation list (debugging only, see above) |
| `--disasm 0xLO-0xHI` | print the instruction boundaries the x86-64 decoder finds in a range and stop |

Names for `--remove` are the source names, `Module.binding` or
`package:Module.binding`, so `--remove Unison.Runtime.JIT` cuts the module
and its submodules and `--remove lsp-types:` cuts a package. A cut
definition that is still referred to is replaced by an abort that names it;
the report lists them:

```
unison: internal error: minify: unison-runtime:Unison.Runtime.JIT.Codegen.instrNativeIn was removed
```

Entering such a definition aborts. Code that inspects a cut data
constructor or CAF without entering it (a `case` on a static constructor
pointer, say) is undefined, so read the list in the report.

## Results

The `unison` executable from `stack build` on macOS arm64, 2026-11-07:

| | size |
|---|---|
| original | 235,761,896 |
| minified | 77,834,392 |
| minified, `--strip` | 64,190,371 |
| minified, `--no-fold` | 88,425,587 |

Of the compactable bytes 57.5% were unreachable once definitions were
split at local closures, static closures and constructor names: 85 MB of
code, 9.3 MB of static closures, 1.3 MB of strings, and with them 351,191
of the 463,853 symbols; the export trie drops from 203,338 entries to
2,923. The transcript suite (unison-src/transcripts and
unison-src/transcripts/idempotent) run with the minified binary gives the
same output as the original for every transcript, and jit-tests.md is
identical with the JIT on.

The macOS x86-64 release binary (trunk-build, 2026-11-07): 105,482,672 to
89,356,861 bytes. The linker already dead-strips at the symbol level on
that platform, so only 0.7% of the compactable bytes were unreachable; the
saving is folding (12.7 MB), constructor names and the export trie. Run
under Rosetta, the minified binary gives the same output as the native
arm64 build on every transcript of the suite.

A Linux arm64 build of a small test program: 20,183,768 to 5,836,672 bytes
as PIE, where the relocation list is exact.

## Limits

C code is never moved or removed. Dead bytes that do not fill a page stay
in the file as zeros (about 4 MB for unison on macOS arm64, 2 MB of it in
`__data`, which can only be cut into 12 pieces). A Mach-O output has one
segment per run of dropped pages (about 90 for unison); dyld and lldb are
fine with that, Apple's `dyld_info` tool is not. The extra `__TEXT` pieces
are named `__TEXT_1` and so on, so the unwinder does not find unwind
information for C code in them. An ELF output may have at most 73 program
headers, the one page the kernel reads them from, so at most about 64 runs
of dead pages leave the file. RELR relocations are not handled. A Linux
executable without a relocation list is handled heuristically, see above.
