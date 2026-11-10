``` unison
Order.total : Nat -> Nat
Order.total subtotal = subtotal

Order.summary : Nat -> Text
Order.summary subtotal = "Total: " ++ Nat.toText (Order.total subtotal)

test> Order.total.tests.example = test.verify do
  ensureEqual (Order.total 100) 100

test> Order.summary.tests.example = test.verify do
  ensureEqual (Order.summary 100) "Total: 100"

Report.line01 : Nat -> Text
Report.line01 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 1
  label = "Line 01: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line02 : Nat -> Text
Report.line02 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 2
  label = "Line 02: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line03 : Nat -> Text
Report.line03 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 3
  label = "Line 03: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line04 : Nat -> Text
Report.line04 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 4
  label = "Line 04: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line05 : Nat -> Text
Report.line05 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 5
  label = "Line 05: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line06 : Nat -> Text
Report.line06 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 6
  label = "Line 06: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line07 : Nat -> Text
Report.line07 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 7
  label = "Line 07: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line08 : Nat -> Text
Report.line08 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 8
  label = "Line 08: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line09 : Nat -> Text
Report.line09 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 9
  label = "Line 09: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line10 : Nat -> Text
Report.line10 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 10
  label = "Line 10: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line11 : Nat -> Text
Report.line11 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 11
  label = "Line 11: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line12 : Nat -> Text
Report.line12 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 12
  label = "Line 12: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line13 : Nat -> Text
Report.line13 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 13
  label = "Line 13: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line14 : Nat -> Text
Report.line14 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 14
  label = "Line 14: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line15 : Nat -> Text
Report.line15 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 15
  label = "Line 15: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line16 : Nat -> Text
Report.line16 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 16
  label = "Line 16: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line17 : Nat -> Text
Report.line17 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 17
  label = "Line 17: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line18 : Nat -> Text
Report.line18 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 18
  label = "Line 18: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line19 : Nat -> Text
Report.line19 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 19
  label = "Line 19: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line20 : Nat -> Text
Report.line20 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 20
  label = "Line 20: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line21 : Nat -> Text
Report.line21 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 21
  label = "Line 21: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line22 : Nat -> Text
Report.line22 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 22
  label = "Line 22: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line23 : Nat -> Text
Report.line23 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 23
  label = "Line 23: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line24 : Nat -> Text
Report.line24 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 24
  label = "Line 24: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line25 : Nat -> Text
Report.line25 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 25
  label = "Line 25: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line26 : Nat -> Text
Report.line26 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 26
  label = "Line 26: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line27 : Nat -> Text
Report.line27 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 27
  label = "Line 27: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line28 : Nat -> Text
Report.line28 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 28
  label = "Line 28: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line29 : Nat -> Text
Report.line29 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 29
  label = "Line 29: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line30 : Nat -> Text
Report.line30 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 30
  label = "Line 30: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line31 : Nat -> Text
Report.line31 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 31
  label = "Line 31: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line32 : Nat -> Text
Report.line32 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 32
  label = "Line 32: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line33 : Nat -> Text
Report.line33 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 33
  label = "Line 33: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line34 : Nat -> Text
Report.line34 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 34
  label = "Line 34: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line35 : Nat -> Text
Report.line35 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 35
  label = "Line 35: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line36 : Nat -> Text
Report.line36 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 36
  label = "Line 36: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line37 : Nat -> Text
Report.line37 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 37
  label = "Line 37: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line38 : Nat -> Text
Report.line38 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 38
  label = "Line 38: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line39 : Nat -> Text
Report.line39 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 39
  label = "Line 39: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line40 : Nat -> Text
Report.line40 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 40
  label = "Line 40: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line41 : Nat -> Text
Report.line41 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 41
  label = "Line 41: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line42 : Nat -> Text
Report.line42 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 42
  label = "Line 42: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line43 : Nat -> Text
Report.line43 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 43
  label = "Line 43: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line44 : Nat -> Text
Report.line44 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 44
  label = "Line 44: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line45 : Nat -> Text
Report.line45 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 45
  label = "Line 45: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line46 : Nat -> Text
Report.line46 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 46
  label = "Line 46: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line47 : Nat -> Text
Report.line47 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 47
  label = "Line 47: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line48 : Nat -> Text
Report.line48 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 48
  label = "Line 48: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line49 : Nat -> Text
Report.line49 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 49
  label = "Line 49: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line50 : Nat -> Text
Report.line50 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 50
  label = "Line 50: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line51 : Nat -> Text
Report.line51 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 51
  label = "Line 51: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line52 : Nat -> Text
Report.line52 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 52
  label = "Line 52: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line53 : Nat -> Text
Report.line53 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 53
  label = "Line 53: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line54 : Nat -> Text
Report.line54 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 54
  label = "Line 54: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line55 : Nat -> Text
Report.line55 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 55
  label = "Line 55: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line56 : Nat -> Text
Report.line56 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 56
  label = "Line 56: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line57 : Nat -> Text
Report.line57 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 57
  label = "Line 57: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line58 : Nat -> Text
Report.line58 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 58
  label = "Line 58: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line59 : Nat -> Text
Report.line59 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 59
  label = "Line 59: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded

Report.line60 : Nat -> Text
Report.line60 subtotal =
  use Nat * +
  amount = Order.total subtotal
  doubled = amount * 2
  padded = doubled + 60
  label = "Line 60: "
  label ++ Nat.toText amount ++ " / " ++ Nat.toText padded
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
