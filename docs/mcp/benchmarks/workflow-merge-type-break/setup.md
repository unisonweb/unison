``` unison
Item.price : Text -> Nat
Item.price = cases
  "pen" -> 250
  "book" -> 2000
  _ -> 0

Item.label : Text -> Text
Item.label name = name ++ ": " ++ Nat.toText (Item.price name)

test> Item.price.tests.example = test.verify do
  ensureEqual (Item.price "pen") 250
  ensureEqual (Item.price "book") 2000
  ensureEqual (Item.price "nothing") 0

test> Item.label.tests.example = test.verify do
  ensureEqual (Item.label "pen") "pen: 250"
```

``` ucm
scratch/main> add
scratch/main> branch alice
scratch/main> branch bob
```

Bob moves prices from whole cents to dollars with a fraction.

``` unison
Item.price : Text -> Float
Item.price = cases
  "pen" -> 2.5
  "book" -> 20.0
  _ -> 0.0

Item.label : Text -> Text
Item.label name = name ++ ": " ++ Float.toText (Item.price name)

test> Item.price.tests.example = test.verify do
  ensureEqual (Item.price "pen") 2.5
  ensureEqual (Item.price "book") 20.0
  ensureEqual (Item.price "nothing") 0.0

test> Item.label.tests.example = test.verify do
  ensureEqual (Item.label "pen") "pen: 2.5"
```

``` ucm
scratch/bob> update
```

Alice adds a discounted price, computed from `Item.price` as it is on her branch.

``` ucm
scratch/alice> ls Item
```

``` unison
Item.discounted : Text -> Nat
Item.discounted name = Item.price name * 9 / 10

test> Item.discounted.tests.example = test.verify do
  ensureEqual (Item.discounted "pen") 225
  ensureEqual (Item.discounted "book") 1800
```

``` ucm
scratch/alice> add
scratch/alice> switch /alice
```
