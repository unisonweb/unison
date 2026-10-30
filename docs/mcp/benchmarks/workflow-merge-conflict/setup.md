``` unison
Order.total : Nat -> Nat
Order.total subtotal = subtotal

Order.summary : Nat -> Text
Order.summary subtotal = "Total: " ++ Nat.toText (Order.total subtotal)

test> Order.total.tests.example = test.verify do
  ensureEqual (Order.total 100) 100

test> Order.summary.tests.example = test.verify do
  ensureEqual (Order.summary 100) "Total: 100"
```

``` ucm
scratch/main> add
scratch/main> branch alice
scratch/main> branch bob
```

Alice adds a flat shipping fee.

``` unison
Order.total : Nat -> Nat
Order.total subtotal = subtotal + 5

Order.summary : Nat -> Text
Order.summary subtotal = "Total: " ++ Nat.toText (Order.total subtotal)

test> Order.total.tests.example = test.verify do
  ensureEqual (Order.total 100) 105

test> Order.summary.tests.example = test.verify do
  ensureEqual (Order.summary 100) "Total: 105"
```

``` ucm
scratch/alice> update
```

Bob takes a discount off.

``` ucm
scratch/bob> ls Order
```

``` unison
Order.total : Nat -> Nat
Order.total subtotal = subtotal - 10

Order.summary : Nat -> Text
Order.summary subtotal = "Total: " ++ Nat.toText (Order.total subtotal)

test> Order.total.tests.example = test.verify do
  ensureEqual (Order.total 100) 90

test> Order.summary.tests.example = test.verify do
  ensureEqual (Order.summary 100) "Total: 90"
```

``` ucm
scratch/bob> update
scratch/alice> switch /alice
```
