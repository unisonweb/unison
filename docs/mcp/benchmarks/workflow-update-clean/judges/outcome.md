You are judging the end state of a change in the Unison codebase you are connected to. A previous agent was asked to make `Invoice.tax` round up to the next whole cent instead of down, so that everything using it follows, and to keep its documentation right and add a test for the rounding. Before the change, `Invoice.tax amount` was 8% of `amount` rounded down; `Invoice.total` and `Invoice.describe` depend on it, and each of the three had a test. You may use any tool except `learn`. Don't change the project's definitions; use scratch code only for your own checks.

Run each of the following in project `scratch`, and check:

1. `branches`: only `main` exists (no `update-*` or other temporary branch), and `main` is the current branch (the prompt of `branches` and `project.current` or `ls` tell you).
2. `todo` has nothing pending.
3. `test` runs and every test passes (there should be at least the three original tests, `Invoice.tax.tests.example`, `Invoice.total.tests.example` and `Invoice.describe.tests.example`, and one or more new ones about rounding).
4. `find` lists `Invoice.tax`, `Invoice.total`, `Invoice.describe`, `Invoice.tax.doc` and the tests in the project (not only in a scratch file), and no extra copy of the function under another name (such as `Invoice.taxUp` or `Invoice.tax2`).
5. `docs Invoice.tax` still exists and says that the result is rounded up (it must not say "rounded down").
6. `dependencies Invoice.total` still lists `Invoice.tax`, and `dependencies Invoice.describe` lists `Invoice.total`: the dependents were not rewritten to avoid `Invoice.tax`.
7. The saved `Invoice.tax` has changed: in a `unison` block, `> Invoice.tax 1005` is `81`. (A change that was only typechecked and never saved still gives `80`.)

It passes if every check holds. It fails if any one does not.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
