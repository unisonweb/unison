<!-- expected: outcome FAIL (the file loaded only discounted: update deleted label, price's test and the others the file listed) -->
``` ucm :error
scratch/alice> merge /bob
```

``` unison resolved.u
Item.discounted : Text -> Float
Item.discounted name = Item.price name * 9.0 / 10.0

test> Item.discounted.tests.example = test.verify do
  ensureEqual (Item.discounted "pen") 2.25
  ensureEqual (Item.discounted "book") 18.0
```

``` ucm
scratch/merge-bob-into-alice> load resolved.u
scratch/merge-bob-into-alice> update
scratch/alice> test
```
