These are commands that will likely be useful during development.

## Scala

Launch `sbt` in `runtime-jvm` directory, then here are various useful commands:

* `main/test:run` - runs all the tests
* `main/test:run compilation` - runs tests prefixed by `"compilation"`
* `main/test:run 102932 compilation.let3` - runs tests prefixed by `"compilation.let3"` with the random seed 102932
* `benchmark/run` - presents menu of benchmarks to run
* `;clean;coverage;main/test:run;coverageReport` followed optionally by `;coverageOff` - generates test coverage report
* `one-jar` - builds a single jar for a project
  * Then `java -jar runtime-jvm/main/target/scala-2.12/unison-runtime_2.12-0.1-SNAPSHOT-one-jar.jar` to run the interpreter.

The runtime doesn't build with JDK 10 at the moment, but this will work:
```bash
JAVA_HOME=`/usr/libexec/java_home -v 9` sbt <commands ...>
```

## Haskell

For doing compilation you can do:

    stack repl unison-parser-typechecker

From here, do `Main.main` to run the tests and `:r` for rapid recompile.

You can also do:

    stack build unison-parser-typechecker

If you want to run the tests outside the REPL.

What if you want a profiled build? Do:

    stack build --executable-profiling --library-profiling --ghc-options="-fprof-auto -rtsopts" unison-parser-typechecker

Again you can leave off the flag. To run an executable with profiling enabled, do:

    stack exec -- <executable-name> +RTS -p

That will generate a `<executable-name>.prof` plain text file with profiling data. [More info on profiling](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html).
