You are judging whether `sampleSeeded` in the Unison codebase you are connected to makes use of what the base library already offers. A previous agent wrote it. You may use any tool except `learn`. Don't change the project's definitions.

Look at the implementation (`view sampleSeeded`, then `dependencies sampleSeeded` for what it calls, following the project's own helper definitions). Use `docs Random` and `find.all Random.` to see what base offers.

Rate it:

- **GREAT**: it runs base's `Random` ability from the seed with a base handler (`Random.splitmix seed do ...`) and chooses with base functions (`Random.shuffle` and `List.take`, `Bag.drawN`, or another base function that draws without repetition), with no generator or shuffle of its own, and its type has no abilities and takes only the seed, the count and the list.
- **PASS**: it uses base's `Random` from the seed but chooses the elements with its own loop (drawing indexes one at a time and skipping repeats, say).
- **FAIL**: it has its own random number generator (a linear congruential step, a hash of the seed), or its type has abilities, or it isn't deterministic for a seed.

A solution that does the right thing but ignores a base function that does the job is not GREAT. Be strict about what the implementation actually calls.

Reply with a short paragraph of reasons, then a final line that is exactly GREAT, PASS or FAIL.
