These are commands that will likely be useful during development.

For doing compilation you can do:

    stack repl unison-shared
    stack repl unison-node

and select the `node-tests` or `shared-tests` executable to launch a REPL with access to the tests in either the `shared` or `node` project. From here, do `Main.main` to run the tests and `:r` for rapid recompile.

To build/run the node container:

    stack build --flag unison-node:leveldb unison-node
    stack exec container

You can leave off the `--flag unison-node:leveldb` if you want, but it seems to be faster than the other backends.

What if you want a profiled build? Do:

    stack build --executable-profiling --library-profiling --ghc-options="-fprof-auto -rtsopts" --flag:leveldb unison-node

Again you can leave off the flag. To run the container with profiling enabled, you do:

    stack exec -- container +RTS -p

That will generate a `container.prof` plain text file with profiling data. [More info on profiling](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html).

To submit Unison programs to the container, do something like:

    curl -H "Content-Type: text/plain; charset=UTF-8" --data-binary @unison-src/searchengine.u http://localhost:8081/compute/dummynode909

You can use any name you want in place of `dummynode909`.

Lastly, for viewing the output of a Unison program, there's currently just one way - using the `Debug.watch` or `Debug.log` functions:

    -- Prints out labeled first argument then returns the second arg
    Debug.log : forall a b . Text -> b -> a -> a

    -- Prints out labeled version of its argument before returning
    Debug.watch : forall a . Text -> a -> a

Here's an example use:

    do Remote
      Remote.transfer alice
      result := ...
      pure (Debug.watch "got result" result)

If you think the runtime is busted and need to do debugging of message flows (hopefully never!), you can edit the file `$(HOME)/.unisonconfig` and add a single line like `logging = 3` (3 is 'info', 2 is 'warn', the default). This will generate lots of output for even simple programs though.
