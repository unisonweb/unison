# Test suite for builtins 

Edit `tests.u` in this directory to add to the test suite. The same test suite can be run using the JIT or the interpreter, using either of the two scripts:

```bash
$ ./unison-src/builtin-tests/jit-tests.sh
```

```bash
$ ./unison-src/builtin-tests/interpreter-tests.sh
```

The scripts will fetch a copy of base and the scheme codegen library and cache it for subsequent runs.