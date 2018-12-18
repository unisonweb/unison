These are commands that will likely be useful during development.

__General:__ `./scripts/test.sh` compiles and builds both the Haskell and Scala code and runs all tests. Recommended that you run this before pushing any code to a branch that others might be working on.

_Disclaimer_ The getting-started instructions on this page have not yet been tested from a 'clean install'.  If they don't work for you, please get in touch via [gitter](https://gitter.im/unisonweb/unison) so we can help.  If you have any fixes to the process, please send us a PR!

## Running Unison

To get cracking with typechecking and running Unison functions, 
* do the prerequisites mentioned for each side of the toolchain below (Scala and Haskell)
* from the root directory of a clone of the Unison repo, run `./scripts/unisonloop.sh` - that fires up the codebase editor, which watches for file changes in `unison-src`
* once a file is typechecked, you can do `add` to add it to the codebase
* and then `view` to view a definition.

## Scala

### Prerequisites

#### JDK

Install a JDK, for example OpenJDK 8.  

The runtime doesn't build with JDK 10 at the moment, but this will work:
```bash
JAVA_HOME=`/usr/libexec/java_home -v 9` sbt <commands ...>
```
#### Scala

A typical sbt-based scala install doesn't actually put scala in your system path.  You need the right scala version (see this [file](https://github.com/unisonweb/unison/blob/master/runtime-jvm/build.sbt)) in your path.

If you don't have scala already, then you can achieve that by following [these instructions](https://gist.github.com/Frozenfire92/3627e38dc47ca581d6d024c14c1cf4a9), taking care to replace the scala version where it appears with the one you want.  

Do `scala -version` to check it's worked.  

#### Bloop

Bloop is a scala build server, that keeps a scala compiler instance 'hot' and waiting to do its job as part of the unison compiler backend.  You need to have it installed and running.

* First install python 2 if you don't have it already, because otherwise bloop will fail to start.
* Then see the [bloop install doc](https://scalacenter.github.io/bloop/setup).  
  * Do the systemd automatic startup stuff which that page mentions.  
  * Then manually start the service, and check its status.
* If it's not already there, then add the bloop executable location (e.g. ~/.bloop) to your PATH.

### Diving in

Launch `sbt` in `runtime-jvm` directory, then here are various useful commands:

* `main/test:run` - runs all the tests
* `main/test:run compilation` - runs tests prefixed by `"compilation"`
* `main/test:run 102932 compilation.let3` - runs tests prefixed by `"compilation.let3"` with the random seed 102932
* `benchmark/run` - presents menu of benchmarks to run
* `;clean;coverage;main/test:run;coverageReport` followed optionally by `;coverageOff` - generates test coverage report
* `main/compile` - builds the interpreter
* `main/run` - builds and runs the interpreter

To run the built interpreter without booting sbt:
```bash
scala -cp main/target/scala-2.12/classes org.unisonweb.Bootstrap test.ub
```
or from the project root directory:
```bash
scala -cp runtime-jvm/main/target/scala-2.12/classes org.unisonweb.Bootstrap test.ub
```

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
