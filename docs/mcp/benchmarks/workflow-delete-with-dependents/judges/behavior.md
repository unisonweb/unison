You are judging the behavior of the `Page` functions in the Unison codebase you are connected to. A previous agent removed the hardcoded page size and made `Page.first`, `Page.count`, `Page.summary` and `Page.report` take the page size as one more, last, `Nat` argument. You may use any tool except `learn`. Don't change the project's definitions; use scratch code only for your own checks.

Evaluate each of these in a `unison` block of watch expressions (`> Page.count (List.range 0 7) 3` and so on) and compare with the expected value. Use sizes other than 20, because an implementation could have the number written into it.

1. `Page.first (List.range 0 7) 3` is `[0, 1, 2]`, and `Page.first [1, 2] 5` is `[1, 2]`.
2. `Page.count (List.range 0 7) 3` is `3`, `Page.count (List.range 0 9) 3` is `3`, `Page.count (List.range 0 10) 3` is `4`, and `Page.count ([] : [Nat]) 5` is `0`.
3. `Page.summary (List.range 0 7) 3` is `"3 pages, first has 3"`, and `Page.summary (List.range 0 45) 20` is `"3 pages, first has 20"`.
4. `Page.report ["a", "b", "c", "d", "e"] 2` is `"report: 3 pages, first has 2"`, and `Page.report (List.map Nat.toText (List.range 0 45)) 20` is `"report: 3 pages, first has 20"`.

It passes only if every value matches.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
