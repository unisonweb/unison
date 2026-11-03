You are judging the end state of a change in the Unison codebase you are connected to. A previous agent was asked to make two independent changes. First, add a `port : Nat` field, as the last field, to the type `Config` and pass the port, as one more last `Nat` argument, through `Config.parse`, `Server.connect` and `Server.status` (a chain: `status` calls `connect`, which calls `parse`, which builds a `Config`). Second, give `Backoff.delay`, `Job.wait`, `Job.total` and `Job.within` (a second chain: `within` calls `total`, which calls `wait`, which calls `delay`) the base delay, a `Nat`, as a new first argument. Both changes break every definition above them, so the agent had to fix all seven functions and their tests and finish the update. Before the change the project had `Config` (`host : Text`, `retries : Nat`), `Config.parse : Text -> Config`, `Server.connect : Text -> Text`, `Server.status : Text -> Text`, `Backoff.delay : Nat -> Nat`, `Job.wait : Nat -> Nat`, `Job.total : Nat -> Nat`, `Job.within : Nat -> Nat -> Boolean`, and a test for each function named `<function>.tests.example`. You may use any tool except `learn`. Don't change the project's definitions; use scratch code only for your own checks.

Run each of the following in project `scratch`, and check:

1. `branches`: only `main` exists (no `update-main` or other temporary branch left behind).
2. `view Config`: three fields, `host : Text`, `retries : Nat`, `port : Nat`, in that order.
3. `find` lists in the project, under the same names, `Config.parse`, `Server.connect`, `Server.status`, `Backoff.delay`, `Job.wait`, `Job.total`, `Job.within` and the seven tests `Config.parse.tests.example`, `Server.connect.tests.example`, `Server.status.tests.example`, `Backoff.delay.tests.example`, `Job.wait.tests.example`, `Job.total.tests.example`, `Job.within.tests.example`. None was lost on the way (an agent that saved only part of what it fixed can lose the rest silently: `Job.total`, `Job.within` and their tests are the likely ones), and there is no second copy under another name (`Config2`, `Job.total2`, ...).
4. The new signatures: `Config.parse : Text -> Nat -> Config`, `Server.connect : Text -> Nat -> Text`, `Server.status : Text -> Nat -> Text`, `Backoff.delay : Nat -> Nat -> Nat`, `Job.wait : Nat -> Nat -> Nat`, `Job.total : Nat -> Nat -> Nat`, `Job.within : Nat -> Nat -> Nat -> Boolean` (`find` or `view` shows them; no abilities in them).
5. `todo` has nothing pending.
6. `test` runs and all seven tests pass.
7. `dependencies Server.status` lists `Server.connect`, `dependencies Server.connect` lists `Config.parse`, `dependencies Job.within` lists `Job.total`, `dependencies Job.total` lists `Job.wait` and `dependencies Job.wait` lists `Backoff.delay`: the chains are kept, not collapsed.

It passes if every check holds. It fails if any one does not.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
