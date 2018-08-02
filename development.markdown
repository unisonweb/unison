These are commands that will likely be useful during development.


__General:__ `./test.sh` compiles and builds both the Haskell and Scala code and runs all tests. Recommended that you run this before pushing any code to a branch that others might be working on.

## Scala

Launch `sbt` in `runtime-jvm` directory, then here are various useful commands:

* `main/test:run` - runs all the tests
* `main/test:run compilation` - runs tests prefixed by `"compilation"`
* `main/test:run 102932 compilation.let3` - runs tests prefixed by `"compilation.let3"` with the random seed 102932
* `benchmark/run` - presents menu of benchmarks to run
* `;clean;coverage;main/test:run;coverageReport` followed optionally by `;coverageOff` - generates test coverage report
* `main/compile` - builds the interpreter
* `main/run` - builds and runs the interpreter

The runtime doesn't build with JDK 10 at the moment, but this will work:
```bash
JAVA_HOME=`/usr/libexec/java_home -v 9` sbt <commands ...>
```

To run the built interpreter without booting sbt:
```bash
scala -cp main/target/scala-2.12/classes org.unisonweb.Bootstrap test.ub
```
or from the project root directory:
```bash
scala -cp runtime-jvm/main/target/scala-2.12/classes org.unisonweb.Bootstrap test.ub
```

## Haskell

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
