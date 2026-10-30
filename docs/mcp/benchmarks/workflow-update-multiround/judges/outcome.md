You are judging the end state of a change in the Unison codebase you are connected to. A previous agent was asked to add a `port : Nat` field, as the last field, to the type `Config`, and to pass the port, as one more last `Nat` argument, through `Config.parse`, `Server.connect`, `Server.status` and `Server.report`, which form a chain (`report` calls `status`, which calls `connect`, which calls `parse`, which builds a `Config`), and to update their tests. Changing the type and the first function breaks the whole chain, so the agent had to fix every layer and finish the update. Before the change the project had `Config` (`host : Text`, `retries : Nat`), `Config.parse : Text -> Config`, `Server.connect : Text -> Text`, `Server.status : Text -> Text`, `Server.report : [Text] -> Text`, and a test for each, named `<function>.tests.example`. You may use any tool except `learn`. Don't change the project's definitions; use scratch code only for your own checks.

Run each of the following in project `scratch`, and check:

1. `branches`: only `main` exists (no `update-main` or other temporary branch left behind).
2. `view Config`: three fields, `host : Text`, `retries : Nat`, `port : Nat`, in that order.
3. `find` lists in the project, under the same names, `Config.parse`, `Server.connect`, `Server.status`, `Server.report` and the four tests `Config.parse.tests.example`, `Server.connect.tests.example`, `Server.status.tests.example`, `Server.report.tests.example`. None was lost on the way (an agent that saved only part of what it fixed can lose the rest silently), and there is no second copy under another name (`Config2`, `Server.connect2`, ...).
4. The new signatures: `Config.parse : Text -> Nat -> Config`, `Server.connect : Text -> Nat -> Text`, `Server.status : Text -> Nat -> Text`, `Server.report : [Text] -> Nat -> Text` (`find` or `view` shows them; no abilities in them).
5. `todo` has nothing pending.
6. `test` runs and all four tests pass.
7. `dependencies Server.report` lists `Server.status`, `dependencies Server.status` lists `Server.connect`, and `dependencies Server.connect` lists `Config.parse`: the chain is kept, not collapsed.

It passes if every check holds. It fails if any one does not.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
