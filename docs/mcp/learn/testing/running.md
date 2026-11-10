Typechecking a test runs it. After [`update`](learn:ucm/update) saves it, `test` runs the saved tests, and results are cached.

- In a [transcript](learn:ucm/transcript), a `unison` block with `test>` lines runs them when the block is typechecked, and the generated block after it reports each result.
- `test` runs the tests saved in the current [branch](learn:ucm/branches), and `test <namespace>` runs those under a namespace. `test.all` includes libraries.
- A test result is cached until the hash of something the test depends on changes (see [the codebase](learn:ucm/codebase)), so running a test again that hasn't changed is instant.
- Pure tests (`test>` lines) can't use IO. For tests that do, use `io.test` (see [IO tests](learn:testing/io)).

## Detail

**What a failure looks like.** The report names the scope of each `labeled` block, the values given to `label`, and the reason:

````
🚫 FAILED
  This will randomly fail:
    x: 3
    y: 67
    elements not equal
    (70, 100)
````

**Tools.** `test` works with every tool, even `transcript-read-only`, since it only reads the [branch](learn:ucm/branches). Saving a test needs [`update`](learn:ucm/update), which needs `transcript`.
