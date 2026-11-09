# unisonrt

`unisonrt` runs a compiled Unison program, a `.uc` file, without a codebase.

`unisonrt`, its bundles and ucm's `bundle` command are experimental. Releases ship `unisonrt` minified ([minify](../../minify/README.md)), and minify is new: a bug in it could break a bundle or a `unisonrt` run, though never ucm itself, which ships as it is built. Run a bundle before relying on it. ucm's `compile` command writes the file:

```
scratch/main> compile main hello
```

and `unisonrt` runs it:

```
unisonrt hello.uc arg1 arg2
```

## Flags

The flags come before the file. Everything after the file is the program's, `+RTS` included. On Windows, the GHC runtime reads the process's command line from the system, so `+RTS ... -RTS` after the file is still read by it there (a bundle's arguments are the program's on every platform).

```
unisonrt [+RTS <GHC RTS flags> -RTS] [--jit MODE] [--jit-dump-ir DIR] FILE.uc [ARGS]
```

- `+RTS ... -RTS`: the GHC runtime's flags. The defaults are ucm's, `-I0 -N -qn4 -qg1`, without its statistics.
- `--jit MODE`: `off`, `on` or `eager`, as ucm's `--jit` and `UNISON_JIT`. With the JIT on, LLVM is loaded at startup. If it can't be, unisonrt says why on stderr and runs the program interpreted.
- `--jit-dump-ir DIR`: as ucm's flag of the same name.
- `--version`: the version of Unison whose programs this runs.

The other JIT settings are read from the environment, as in ucm ([`JIT.Config`](../src/Unison/Runtime/JIT/Config.hs)).

[`main.c`](main.c) is the program's entry point. It is GHC's own, except that it stops the RTS reading flags at the `.uc` file.

## Bundles

ucm's `bundle` command writes a self-contained executable that runs a definition, with a minified copy of unisonrt:

```
demo/main> bundle apps.hello.main hello
```

or from the shell, where the output is named after the definition unless given:

```
ucm bundle demo/main:apps.hello.main [hello]
```

ucm takes the unisonrt that `UNISONRT` names, else the one next to ucm, else the one on the `PATH`, checks that its version is ucm's, and copies it as it is ([`HandleInput/Bundle.hs`](../../unison-cli/src/Unison/Codebase/Editor/HandleInput/Bundle.hs)). Releases ship unisonrt next to ucm, minified when it is built ([`bundle-ucm.yaml`](../../.github/workflows/bundle-ucm.yaml)), so their bundles are small: a hello world is 23 MB. A development build's unisonrt is the whole runtime, and so its bundles are about 106 MB. Minify one by hand to get the release's size:

```
stack exec -- minify $(stack exec -- which unisonrt) -o unisonrt-min --strip --verify
UNISONRT=unisonrt-min ucm bundle demo/main:apps.hello.main
```

Minify doesn't read Windows executables, so on Windows unisonrt ships whole. On Linux, minify needs unisonrt linked with its relocations kept, which [`package.yaml`](../package.yaml) asks for.

`unisonrt bundle` makes a bundle from a `.uc` file, with the unisonrt running it, minified or not:

```
unisonrt bundle hello.uc            # writes hello
unisonrt bundle hello.uc -o greet   # writes greet
./hello arg1 arg2
```

A bundle is a copy of the `unisonrt` executable with the `.uc` file added ([`Bundle.hs`](Bundle.hs)):

- **macOS.** The program goes inside the Mach-O, as the section `__UNISON,__uc`. `unisonrt` is linked with that section holding 16 zero bytes ([`bundle-section`](bundle-section)), in a segment of its own just before `__LINKEDIT`. `bundle` fills the section with `UNISONRT`, the program's length as 8 little-endian bytes, and the program. It moves `__LINKEDIT` after it by whole pages, adjusts the load commands that point into `__LINKEDIT`, and signs the result again, ad hoc, with `codesign`. The program is covered by the signature like the rest of the image, so the bundle passes `codesign -v --strict` and can be signed again with a Developer ID and notarized. At startup the program reads the section from memory.
- **Elsewhere.** The `.uc` file follows the executable, then a 16-byte trailer: the offset of the `.uc` file as 8 little-endian bytes, and the text `UNISONRT`. At startup the program reads the trailer at the end of its own file, then the program at that offset.

A bundle's arguments are all the program's, so a bundle can't run `bundle` itself. GHC RTS flags can go in the `GHCRTS` environment variable, and the JIT settings in `UNISON_JIT` and the others. `main.c` detects a bundle the same way, so the RTS does not read `+RTS` from a bundle's arguments. `bundle` checks that this unisonrt can run the file before writing anything. Nothing is extracted to disk.

A minified `unisonrt` ([minify](../../minify/README.md) on its branch) keeps the section's segment just before `__LINKEDIT`, and bundles made from it work the same way.

## Versions

A `.uc` file holds MCode, whose encoding changes between versions of Unison, so `unisonrt` runs only files compiled by the same version of the runtime. The version is `git describe` at the time the runtime library was compiled ([`Unison.Runtime.Version`](../src/Unison/Runtime/Version.hs)). ucm stamps it into the files `compile` writes, and both `run.compiled` and unisonrt check it. ucm and unisonrt agree when they are built together. Rebuilding only one of them after the runtime library recompiles makes them disagree. A mismatch is reported with the hash of the program's main, to compile it again.

## Output

A program's output is its own. When the program fails, unisonrt prints the failure on stderr on one line, with the frames of the Unison stack, and exits with status 1:

```
unisonrt: bug: Some (Foo 1 "x")
  at #a8s7dd2m
```

Values print on one line with a small printer, not ucm's pretty-printer ([`Unison.Runtime.Render`](../src/Unison/Runtime/Render.hs)). It prints data in full: numbers, characters, text, bytes, lists, tuples, records and constructors. Anything else prints as a placeholder, such as `<function>`. `Debug.trace` and `Debug.toText` use the same printer.

Constructors print by the names the codebase gave them when the program was compiled. `compile` writes them into the `.uc` file, for every type the program's code mentions ([`constructorNames`](../ucm/Unison/Runtime/Interface.hs)). A constructor without a name there prints as its type's hash and its number.

## Testing

- **The bundle format.** `Unison.Test.Runtime.Bundle`, in the runtime's test suite, writes bundles from `unisonrt` and from each other, reads their programs back, runs one, and on macOS checks its signature with `codesign --verify --strict`.
- **Bundles end to end.** [`scripts/bundle-check.sh`](../../scripts/bundle-check.sh) compiles the programs of [`unison-src/tests/bundle/programs.md`](../../unison-src/tests/bundle/programs.md) and bundles them three ways: ucm's `bundle`, `ucm bundle`, and `unisonrt bundle`. It minifies `unisonrt` first as a release does, then runs the programs with the JIT off and on, and compares what they print and their exit codes with `expected.txt`. The programs cover arguments, value printing, traces, failures, and small versions of the JIT's tests, which get hot enough with the JIT on to be compiled. CI's `jit` job runs it on every platform, on the codebase the JIT's transcript made. It takes about half a minute:

```
scripts/bundle-check.sh --codebase jit_codebase $(stack exec -- which unison) $(stack exec -- which unisonrt) $(stack exec -- which minify)
```

`--update` rewrites `expected.txt` after an intended change.

## What it is built from

unison-runtime has two libraries ([`package.yaml`](../package.yaml)):

- `core` ([`src`](../src)) is what running compiled code needs: the machine, the builtins, the JIT, and the reader of `.uc` files ([`Unison.Runtime.Standalone`](../src/Unison/Runtime/Standalone.hs)). It does not depend on the parser, the typechecker, the codebase or the pretty-printer.
- The main library ([`ucm`](../ucm)) is what ucm adds: compiling terms from a codebase to ANF ([`Unison.Runtime.ANF.Convert`](../ucm/Unison/Runtime/ANF/Convert.hs)), decompiling values back to terms, and the `Runtime` interface.

`unisonrt` depends only on `core`.

The runtime needs the references of the builtin declarations (`Optional`, `Either`, the IO failures and so on). Those are hashes of declarations defined in parser-typechecker, so `core` holds them as constants ([`Unison.Runtime.Builtin.Decls`](../src/Unison/Runtime/Builtin/Decls.hs)), generated by [`scripts/GenBuiltinDecls.hs`](../scripts/GenBuiltinDecls.hs). The test `Unison.Test.Runtime.BuiltinDecls` checks them against their sources. Run the script again after changing a builtin declaration.
