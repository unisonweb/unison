You are judging whether `slowSquares` in the Unison codebase you are connected to makes use of what the base library already offers. A previous agent wrote it. You may use any tool except `learn`. Don't change the project's definitions.

Look at the implementation (`view slowSquares`, then `dependencies slowSquares` for what it calls, following the project's own helper definitions). Use `find` and `docs` for `IO.Threads`.

Rate it:

- **GREAT**: it is built on a base structured-concurrency function (such as `IO.Threads.parMap`), with the sleep and squaring as the function mapped.
- **PASS**: it forks threads itself with `IO.Threads.fork`/`scope` and collects the results, correctly, instead of using `parMap`.
- **FAIL**: it is sequential, or uses unstructured forking (`IO.Threads.detach`, bare `fork`) so threads may outlive the call, or doesn't use `Threads` at all.

A solution that does the right thing but ignores a base function that does the job is not GREAT. Be strict about what the implementation actually calls.

Reply with a short paragraph of reasons, then a final line that is exactly GREAT, PASS or FAIL.
