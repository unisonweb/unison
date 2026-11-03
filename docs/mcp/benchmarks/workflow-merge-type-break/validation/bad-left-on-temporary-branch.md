<!-- expected: outcome FAIL (loaded the fixed file, never ran update, so it ends on merge-bob-into-alice) -->
``` ucm :error
scratch/alice> merge /bob
```

``` unison resolved.u
Item.discounted : Text -> Float
Item.discounted name = Item.price name * 9.0 / 10.0

test> Item.discounted.tests.example = test.verify do
  ensureEqual (Item.discounted "pen") 2.25
  ensureEqual (Item.discounted "book") 18.0

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
```
