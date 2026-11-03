<!-- expected: approach PASS (outcome and behavior PASS: Bob's definitions were retyped on alice after cancelling the merge, with the same hashes, so bob is not in the history) -->
``` ucm :error
scratch/alice> merge /bob
```

``` ucm
scratch/merge-bob-into-alice> cancel
```

``` unison redo.u
Item.price : Text -> Float
Item.price = cases
  "pen" -> 2.5
  "book" -> 20.0
  _ -> 0.0

Item.label : Text -> Text
Item.label name = name ++ ": " ++ Float.toText (Item.price name)

Item.discounted : Text -> Float
Item.discounted name = Item.price name * 9.0 / 10.0

test> Item.price.tests.example = test.verify do
  ensureEqual (Item.price "pen") 2.5
  ensureEqual (Item.price "book") 20.0
  ensureEqual (Item.price "nothing") 0.0

test> Item.label.tests.example = test.verify do
  ensureEqual (Item.label "pen") "pen: 2.5"

test> Item.discounted.tests.example = test.verify do
  ensureEqual (Item.discounted "pen") 2.25
  ensureEqual (Item.discounted "book") 18.0
```

``` ucm
scratch/alice> load redo.u
scratch/alice> update
scratch/alice> test
```
