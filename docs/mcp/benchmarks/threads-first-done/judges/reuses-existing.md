You are judging whether `firstDone` in the Unison codebase you are connected to makes use of what the base library already offers. A previous agent wrote it. You may use any tool except `learn`. Don't change the project's definitions.

Look at the implementation (`view firstDone`, then `dependencies firstDone` for what it calls, following the project's own helper definitions). Use `find` and `docs` for `IO.Threads`.

Rate it:

- **GREAT**: it is built on a base racing function (`IO.Threads.multiRace`, or `IO.Threads.race` folded over the list).
- **PASS**: it forks the tasks itself with `IO.Threads.fork`/`scope` and a promise or signal, correctly cancelling the others.
- **FAIL**: it waits for all the tasks, or is sequential, or uses unstructured forking so threads may outlive the call.

A solution that does the right thing but ignores a base function that does the job is not GREAT. Be strict about what the implementation actually calls.

Reply with a short paragraph of reasons, then a final line that is exactly GREAT, PASS or FAIL.
