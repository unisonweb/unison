Running programs and tests.

- `test` runs the tests in the [branch](learn:ucm/branches) that don't use IO (see [testing](learn:testing)). `test.all` includes [libraries](learn:ucm/libraries).
- `run <name>` runs the program `name`, a [delayed computation](learn:lang/delayed) defined in the most recent `unison` block or in the branch. It needs `transcript-io` (see [transcripts](learn:ucm/transcript)), since the code can do anything IO can.
- `io.test <name>` runs a test that uses IO, and needs `transcript-io`.
- `add.run <name>` saves the result of the most recent `run` under a name.

## Detail

**Profiling.** `run.profiled` also prints profiling information.

**Compiling.** `compile <name> <file>` writes a standalone file that runs `name`. It works with `transcript`.

**Program output** from `run` appears under "Standard output of the transcript" in the reply.
