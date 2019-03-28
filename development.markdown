These are commands that will likely be useful during development.

__General:__ `./scripts/test.sh` compiles and builds the Haskell code and runs all tests. Recommended that you run this before pushing any code to a branch that others might be working on.

_Disclaimer_ The getting-started instructions on this page have not yet been tested from a 'clean install'.  If they don't work for you, please get in touch via [gitter](https://gitter.im/unisonweb/unison) so we can help.  If you have any fixes to the process, please send us a PR!

## Running Unison

To get cracking with typechecking and running Unison functions,
* do the prerequisites mentioned in the list below
* run `./scripts/test.sh` if you haven't done it yet
* from the root directory of a clone of the Unison repo, run `./scripts/unisonloop.sh unison-src` - that fires up the codebase editor and watches for file changes in the `unison-src` directory
* once a file is typechecked, you can do `add` to add it to the codebase
* and then `view` to view a definition.

## Haskell

### Prerequisites
If you don't have haskell yet, visit [this page](https://docs.haskellstack.org/en/stable/README/#how-to-install) to install stack, which will then get it for you when you run the commands below.

### Working with the parser/typechecker

For doing compilation you can do:

    stack repl unison-parser-typechecker

You can also do:

    stack build unison-parser-typechecker && stack exec tests

If you want to run the tests outside the REPL. The `stack exec tests` takes an optional command line arg specifying the test prefix. In the REPL, you can also do:

    > import EasyTest as ET
    > ET.runOnly "typechecker" Main.test

What if you want a profiled build? Do:

    stack build --executable-profiling --library-profiling --ghc-options="-fprof-auto -rtsopts" unison-parser-typechecker

Again you can leave off the flag. To run an executable with profiling enabled, do:

    stack exec -- <executable-name> +RTS -p

That will generate a `<executable-name>.prof` plain text file with profiling data. [More info on profiling](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html).
