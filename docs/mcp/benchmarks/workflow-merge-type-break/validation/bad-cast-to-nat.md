<!-- expected: behavior FAIL (discounted still returns a Nat, now cents, so "pen" is not 2.25) -->
``` ucm :error
scratch/alice> merge /bob
```

``` unison resolved.u
Item.discounted : Text -> Nat
Item.discounted name = Int.truncate0 (Optional.getOrElse +0 (Float.toInt (Item.price name * 90.0)))

test> Item.discounted.tests.example = test.verify do
  ensureEqual (Item.discounted "pen") 225
  ensureEqual (Item.discounted "book") 1800

Item.label : Text -> Text
Item.label name = name ++ ": " ++ Float.toText (Item.price name)

test> Item.label.tests.example = test.verify do
  ensureEqual (Item.label "pen") "pen: 2.5"

test> Item.price.tests.example = test.verify do
  ensureEqual (Item.price "pen") 2.5
  ensureEqual (Item.price "book") 20.0
  ensureEqual (Item.price "nothing") 0.0
```

``` ucm
scratch/merge-bob-into-alice> load resolved.u
scratch/merge-bob-into-alice> update
scratch/alice> test
```
