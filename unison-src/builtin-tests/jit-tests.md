
Note: This should be forked off of the codebase created by base.md

```ucm:hide
.> compile.native.fetch
.> compile.native.genlibs
.> load unison-src/builtin-tests/testlib.u
.> add
```

If you want to define more complex tests somewhere other than `tests.u`, just `load my-tests.u` then `add`,
then reference those tests (which should be of type `'{IO,Exception,Tests} ()`, written using calls
to `Tests.check` and `Tests.checkEqual`).

```ucm:hide
.> load unison-src/builtin-tests/concurrency-tests.u
.> add
```

```ucm:hide
.> load unison-src/builtin-tests/networking-tests.u
.> add
```

```ucm:hide
.> load unison-src/builtin-tests/tests.u
.> add
```

```ucm
.> run.native tests
```
