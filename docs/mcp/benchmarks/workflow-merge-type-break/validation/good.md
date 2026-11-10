<!-- The worker edits the file the merge wrote (merge-bob-into-alice-<hash>.u) and loads it; here the resolved definitions are written as a named file (resolved.u) and loaded, which is the same thing. -->
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
scratch/merge-bob-into-alice> update
scratch/alice> test
```
