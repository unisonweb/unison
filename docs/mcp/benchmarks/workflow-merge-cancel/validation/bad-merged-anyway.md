<!-- expected: outcome FAIL and no-collateral FAIL (the conflicts were resolved in Alice's favor and the merge was finished, so Bob's branch, with Debug.trace, is merged into alice) -->
``` unison resolved.u
Page.title : Text
Page.title = "Home page"

Page.render : Text -> Text
Page.render body = "== " ++ Page.title ++ " ==\n" ++ body

test> Page.render.tests.example = test.verify do
  ensureEqual (Page.render "hi") "== Home page ==\nhi"
```

``` ucm
scratch/merge-bob-into-alice> load resolved.u
scratch/merge-bob-into-alice> update
scratch/alice> test
```
