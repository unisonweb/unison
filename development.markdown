These are commands that will likely be useful during development.

__General:__ `./scripts/test.sh` compiles and builds the Haskell code and runs all tests. Recommended that you run this before pushing any code to a branch that others might be working on.

_Disclaimer_ If you have trouble getting started, please get in touch via [Slack](https://unison-lang.org/community) so we can help.  If you have any fixes to the process, please send us a PR!

## Running Unison

To get cracking with Unison:

1. [Install `stack`](https://docs.haskellstack.org/en/stable/README/#how-to-install).
2. Build the project with `stack build`. This builds all executables.
3. (Optional) Run `./dev-ui-install.hs` to fetch the latest release of the codebase UI. If you don't care about running the codebase UI locally you can ignore this step.
4. After building do `stack exec unison -- init` will initialize a codebase in your home directory (in `~/.unison`). This only needs to be done once.
5. `stack exec unison` starts Unison and watches for `.u` file changes in the current directory. If you want to run it in a different directory, just add `unison` to your `PATH`, after finding it with `stack exec which unison`.

On startup, Unison prints a url for the codebase UI. If you did step 3 above, then visiting that URL in a browser will give you a nice interface to your codebase.

## Running Tests

* `stack test --fast` builds and runs most test suites, see below for exceptions to this (e.g. transcript tests).

Most test suites support selecting a specific test to run by passing a prefix as a test argument:

* `stack test parser-typechecker --fast --test-arguments my-test-prefix` builds and runs most test suites, see below for exceptions to this (e.g. transcript tests).

Some tests are executables instead:

* `stack exec transcripts` runs the transcripts-related integration tests, found in `unison-src/transcripts`. You can add more tests to this directory.
* `stack exec transcripts -- prefix-of-filename` runs only transcript tests with a matching filename prefix.
* `stack exec unison -- transcript unison-src/transcripts-round-trip/main.md` runs the pretty-printing round trip tests


### What if you want a profiled build?

Do:

    stack build --executable-profiling --library-profiling --ghc-options="-fprof-auto -rtsopts" unison-parser-typechecker

Again you can leave off the flag. To run an executable with profiling enabled, do:

    stack exec -- <executable-name> +RTS -p

That will generate a `<executable-name>.prof` plain text file with profiling data. [More info on profiling](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html).

## Building with cabal

Unison can also be built/installed with cabal. You'll need the same ghc
used by `stack.yaml` to successfully build its dependencies.
The provided project file is also in contrib/ so you'll need to specify
its location on the command line.

* To build all projects use

    `cabal v2-build --project-file=contrib/cabal.project all`

* Tests can be run with e.g.

    `cabal v2-test --project-file=contrib/cabal.project all`

* The executable can be installed with

    `cabal v2-install --project-file=contrib/cabal.project unison`

* The install directory can be modified with the option `--installdir: ...`
