Tests that use IO, such as reading a file or the clock, are not `test>` lines. They are definitions that start with `do`, and `io.test` runs them.

- An IO test looks like `foo.tests.name = do test.verify do ...`. The `do` before `test.verify` makes the whole test a definition whose type is `'{IO} [Result]`: a computation that hasn't run yet.
- Inside the `test.verify` block you can use IO as well as the checks, for example `printLine "checking"`.
- Run it with `io.test foo.tests.name`, or all of them with `io.test.all`. `io.test` needs `transcript-io` (see [transcripts](learn:ucm/transcript)).
- Name it like any other test (see [naming](learn:testing/naming)). Prefer a pure `test>` test unless the thing you are testing has to use IO.

````
``` unison
double.tests.ioExample = do test.verify do
  printLine "checking double"
  Each.repeat 5
  n = Random.natIn 0 100
  ensureEqual (double n) (n + n)
```
````

## Detail

**Why they differ.** A pure test's result is cached, since it can only depend on its inputs. A test that uses IO may give a different result each run, so it isn't a `test>` line and isn't cached.

**Saving and running.** `update` saves an IO test like any other definition (see [`update`](learn:ucm/update)). It doesn't run when typechecked.
