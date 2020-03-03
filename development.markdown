These are commands that will likely be useful during development.

__General:__ `./scripts/test.sh` compiles and builds the Haskell code and runs all tests. Recommended that you run this before pushing any code to a branch that others might be working on.

_Disclaimer_ If you have trouble getting started, please get in touch via [Slack](https://unisonweb.org/community) so we can help.  If you have any fixes to the process, please send us a PR!

## Running Unison

To get cracking with Unison:

* [Install `stack`](https://docs.haskellstack.org/en/stable/README/#how-to-install).
* Build the project with `stack build`. This builds all executables.
* After building, `stack exec unison` will fire up the codebase editor, create a codebase in the current directory, and watch for `.u` file changes.  If you want to run it in a different directory, just add `unison` to your `PATH`, after finding it with `find .stack-work -name unison -type f`.  (For me, this finds two, they both work, but have different contents.  ¯\\\_(ツ)\_/¯ )
* `stack exec tests` runs the tests
* `stack exec transcripts` runs all the integration tests, found in `unison-src/transcripts`. You can add more tests to this directory.

### What if you want a profiled build?

Do:

    stack build --executable-profiling --library-profiling --ghc-options="-fprof-auto -rtsopts" unison-parser-typechecker

Again you can leave off the flag. To run an executable with profiling enabled, do:

    stack exec -- <executable-name> +RTS -p

That will generate a `<executable-name>.prof` plain text file with profiling data. [More info on profiling](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html).

## Building with cabal

Unison can also be built/installed with cabal. You'll need ghc 8.6.x to
successfully build its dependencies. The provided project file is also in
contrib/ so you'll need to specify its location on the command line.

* To configure the build, you can use e.g. `cabal v2-configure
  --project-file=contrib/cabal.project --with-ghc=ghc-8.6.5` if you have
  multiple versions of GHC installed
* To build all projects use `cabal v2-build --project-file=contrib/cabal.project all`
* Tests can be run with e.g. `cabal v2-run --project-file=contrib/cabal.project
  parser-typechecker:tests`
* The executable can be installed with `cabal v2-install
  --project-file=contrib/cabal.project unison` the install directory can be
  modified by setting `installdir: ...` in `.cabal/config`
