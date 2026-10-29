You are judging how `shuffleSeeded` in the Unison codebase you are connected to uses abilities. A previous agent wrote it. You may use any tool except `learn`. Don't change the project's definitions.

View it with `view shuffleSeeded` and check each of the following:

1. It handles the `Random` ability locally with a seeded handler from base (look for `Random.splitmix`, `Random.lcg` or `RNG.run`), so the function's own type has no `Random` and no `IO`.
2. It does not use `Random.run` or any IO to get randomness.
3. It is not given the generator as an extra argument beyond the seed.

It passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
