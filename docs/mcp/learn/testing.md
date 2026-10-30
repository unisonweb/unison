Tests in Unison are ordinary definitions that produce a list of results. You write them with `test>` lines and run them with [`test`](learn:ucm/run), using helper functions from the base library.

- A test is a definition of type `[Result]`. The usual way to write one is a `test>` line in a `unison` block (see [the scratch file](learn:ucm/scratch-file)):

  ````
  ``` unison
  double : Nat -> Nat
  double n = n * 2

  test> double.tests.examples = test.verify do
    labeled "zero" do ensureEqual (double 0) 0
    labeled "three" do ensureEqual (double 3) 6
  ```
  ````

- Typechecking the block runs the test, and the generated block after yours reports `✅ Passed`, or what failed. [`update`](learn:ucm/update) saves the test into the branch, and `test` runs the saved tests again (see [running tests](learn:testing/running)).
- **Test what could go wrong, not just the example in the task.** Cover the empty and single-element cases, each way a function can fail (an input that fails, the first and the last of several), every branch, and for anything recursive or structured, an input where every part is non-trivial at once (both recursive branches large, not just one). Write the expected values out; don't compute them by calling the function under test.
- Name the tests for `foo` as `foo.tests.<name>` (see [naming](learn:testing/naming)).
- `test.verify` runs a block of checks such as `ensureEqual` (see [the test functions](learn:testing/verify)). For checks over many generated inputs, add random generation (see [property-based tests](learn:testing/props)).
- A test that needs to use IO is written differently from a `test>` line (see [IO tests](learn:testing/io)).
- The test functions are in the base library (see [where they come from](learn:testing/libraries)). The blocks you pass them are written in a particular syntax (see [syntax](learn:testing/syntax)).
