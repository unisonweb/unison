You are judging how `shuffleSeeded` in the Unison codebase you are connected to gets its randomness and its shuffle. A previous agent wrote it. You may use any tool except `learn`. Don't change the project's definitions.

Look at the implementation (`view shuffleSeeded`, then `dependencies shuffleSeeded` for what it calls, following the project's own helper definitions). Use `find` and `docs` for the `Random` ability to see what base offers: its handlers and the functions built on it.

First check that its type is `Nat -> [a] -> [a]`, with no abilities (no `IO`, no `Random` left unhandled), and that it doesn't use `Random.run` or IO, and that it takes only the seed and the list. If any of that fails, the rating is FAIL.

Then rate it:

- **GREAT**: it uses base's `Random.shuffle` run under a seeded handler from base (such as `Random.splitmix` or `Random.lcg`), and nothing hand-written for the randomness or the shuffle.
- **PASS**: it uses the `Random` ability with a seeded handler from base, but hand-rolls the shuffling algorithm (for example Fisher-Yates over a mutable array).
- **FAIL**: it implements its own pseudo-random generator, or doesn't use a seeded handler from base, or fails the type checks above.

A solution that does the right thing but ignores a base function that does the job is not GREAT. Be strict about what the implementation actually calls.

Reply with a short paragraph of reasons, then a final line that is exactly GREAT, PASS or FAIL.
