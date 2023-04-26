This directory contains libraries necessary for building and running
unison programs via Racket Scheme.

## Prerequisites

You'll need to have a couple things installed on your system:

* [libcrypto](https://github.com/openssl/openssl) (you probably already have this installed)
* [Racket](https://racket-lang.org/), with the executable `racket` on your path somewhere
* [BLAKE2](https://github.com/BLAKE2/libb2) (you may need to install this manually)


In particular, our crypto functions require on both `libcrypto` (from openssl) and `libb2`. You may have to tell racket where to find `libb2`, by adding an entry to the hash table in your [`config.rktd` file](https://docs.racket-lang.org/raco/config-file.html). This is what I had, for an M1 mac w/ libb2 installed via Homebrew:
```
(lib-search-dirs . (#f "/opt/homebrew/Cellar/libb2/0.98.1/lib/"))
```
You'll also need to install `x509-lib` with `raco pkg install x509-lib`


## Running the unison test suite

To run the test suite, first `stack build` (or `stack build --fast`), then:

```
./unison-src/builtin-tests/jit-tests.sh
```

OR if you want to run the same tests in interpreted mode:

```
./unison-src/builtin-tests/interpreter-tests.sh
```

The above scripts fetch and cache a copy of base and the scheme-generating libraries, and copy this directory to `$XDG_DATA_DIRECTORY/unisonlanguage/scheme-libs`.

## Iterating more quickly

If running the above transcripts is too slow for you, here's a few things you can do instead:

### Run without needing to bounce ucm

First, tell UCM to load scheme files from this directory, by adding
a `SchemeLibs.Static` item to your `~/.unisonConfig`.

```
SchemeLibs.Static = "/path/to/unisoncode"
```

With this set, the compiler commands will look in `/path/to/somewhere/scheme-libs/` for the subdirectories containing the library files.

Once that's done, you can load the testing library and tests:

```
.jit> load unison-src/builtin-tests/testlib.u
.jit> add
.jit> load unison-src/builtin-tests/tests.u
.jit> add
```

And then, without needing to bounce `ucm` every time you edit your scheme files, you can do:

```
.jit> run.native tests
```

### Run without needing to regenerate the scheme

`run.native` produces a scheme file in `$XDG_CACHE_DIRECTORY/unisonlanguage/scheme-tmp`, so going one step further, you can grab these files and run them directly using Racket, bypassing `ucm` entirely.

```
~/unison » ls ~/.cache/unisonlanguage/scheme-tmp
testSuite.scm tests.scm
```

When running `tests.scm` directly with Racket, you'll need to add this `scheme-libs` directory and the generated builtins library to the path.

```
racket -S ~/.cache/unisonlanguage/scheme-libs/ -S ~/.local/share/unisonlanguage/scheme-libs/racket/ -S ~/.local/share/unisonlanguage/scheme-libs/common/  ~/.cache/unisonlanguage/scheme-tmp/tests.scm
``

## Loading in Racket

To load these libraries into a racket runtime, racket should be invoked like this:
```bash
$ racket -S scheme-libs/racket
Welcome to Racket v8.7 [cs].
> (require unison/core)
> ; now you can try out the definitions in core.ss!
```

You can then run racket tests with:

```bash
$ raco test scheme-libs/racket/unison/tests/your-test-file.rkt
```
