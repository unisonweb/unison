You are judging whether `shuffleSeeded` in the Unison codebase you are connected to makes use of what the base library already offers. A previous agent wrote it. You may use any tool except `learn`. Don't change the project's definitions.

Look at the implementation (`view shuffleSeeded`, then `dependencies shuffleSeeded` for what it calls, following the project's own helper definitions). Use `find` and `docs` for the `Random` ability: its handlers and the functions built on it.

Rate it:

- **GREAT**: it uses base's `Random.shuffle` run under a seeded handler from base (such as `Random.splitmix` or `Random.lcg`), and nothing hand-written for the randomness.
- **PASS**: it uses the `Random` ability to produce random choices and a base seeded handler, but hand-rolls the shuffling algorithm.
- **FAIL**: it implements its own pseudo-random generator, or uses `Random.run`/IO, or otherwise doesn't use a seeded handler from base.

A solution that does the right thing but ignores a base function that does the job is not GREAT. Be strict about what the implementation actually calls.

Reply with a short paragraph of reasons, then a final line that is exactly GREAT, PASS or FAIL.
