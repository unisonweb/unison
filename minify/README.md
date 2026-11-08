# minify

Removes the definitions a GHC-linked executable never uses and compacts the
file. `design.md` explains how; this file says how to run it.

## Building and running

```
stack build minify
stack exec minify -- INPUT -o OUTPUT [--verify] [--report]
```

The input must be an unstripped executable (the symbol table is what
defines the definitions). The output is re-signed ad hoc with `codesign`.
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
| `--no-split SECTION` | do not split definitions at local closures (`text`), static closures (`data`) or constructor names (`strings`) |
| `--strip` | drop the symbol table and function starts; saves about 24 MB on unison, but the output cannot be minified again or symbolicated |
| `--force` | write the output even when the analysis reported anomalies (code it did not understand) |

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
| minified | 88,845,434 |
| minified, `--strip` | 75,167,187 |
| minified, `--strip --keep-con-desc --keep-haskell-exports` | 78,463,835 |

Of the compactable bytes 57.5% were unreachable once definitions were
split at local closures, static closures and constructor names: 85 MB of
code, 9.3 MB of static closures, 1.3 MB of strings, and with them 351,191
of the 463,853 symbols; the export trie drops from 203,338 entries to
2,923. The transcript suite (unison-src/transcripts and
unison-src/transcripts/idempotent) run with the minified binary gives the
same output as the original for every transcript, and jit-tests.md is
identical with the JIT on.

## Limits

Only AArch64 Mach-O. C code is never moved or removed. Dead bytes that do
not fill a page stay in the file as zeros (about 4 MB for unison, 2 MB of
it in `__data`, which can only be cut into 12 pieces). The output has one
segment per run of dropped pages (about 90 for unison); dyld and lldb are
fine with that, Apple's `dyld_info` tool is not. The extra `__TEXT` pieces
are named `__TEXT_1` and so on, so the unwinder does not find unwind
information for C code in them.
