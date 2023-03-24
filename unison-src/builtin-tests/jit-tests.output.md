
Note: This should be forked off of the codebase created by base.md

If you want to define more complex tests somewhere other than `tests.u`, just `load my-tests.u` then `add`,
then reference those tests (which should be of type `'{IO,Exception,Tests} ()`, written using calls
to `Tests.check` and `Tests.checkEqual`).

TODO remember to re-add ucm to this snippet to run it
```
.> run.native tests

```

```ucm
.> run.native ex

  Scheme evaluation failed.

```
