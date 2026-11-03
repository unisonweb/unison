A stock-keeping namespace. `Stock.restock` has a doc and two tests in its own namespace (`Stock.restock.tests.*`), and `Stock.receiveAll` uses it. `Sku` is a type with one constructor, `Sku.Sku`, used by `Sku.code`, `Sku.parse` and a test. `Stock.report` and its test use neither and must not change.

```` unison
type Sku = Sku Text

Sku.code : Sku -> Text
Sku.code = cases Sku text -> text

Sku.parse : Text -> Optional Sku
Sku.parse text = if Text.size text == 6 then Some (Sku text) else None

Stock.restock : Text -> Nat -> Map Text Nat -> Map Text Nat
Stock.restock name qty stock =
  Map.alter (cases
    None -> Some qty
    Some n -> Some (n + qty)) name stock

Stock.restock.doc = {{
{{ docExample 3 (do (name qty stock -> Stock.restock name qty stock)) }} adds `qty` units of `name` to
the stock, creating the entry when there is none. For example:

@eval{ Map.toList (Stock.restock "fig" 2 (Map.fromList [("fig", 1)])) }
}}

Stock.receiveAll : [(Text, Nat)] -> Map Text Nat -> Map Text Nat
Stock.receiveAll items stock =
  List.foldLeft (acc item -> match item with (name, qty) -> Stock.restock name qty acc) stock items

Stock.report : Map Text Nat -> [Text]
Stock.report stock = List.map (cases (name, n) -> name ++ ": " ++ Nat.toText n) (Map.toList stock)

test> Stock.restock.tests.adds = test.verify do
  ensureEqual (Map.toList (Stock.restock "fig" 2 Map.empty)) [("fig", 2)]

test> Stock.restock.tests.existing = test.verify do
  ensureEqual (Map.toList (Stock.restock "fig" 2 (Map.fromList [("fig", 1)]))) [("fig", 3)]

test> Stock.tests.receiveAll = test.verify do
  ensureEqual (Map.toList (Stock.receiveAll [("fig", 1), ("pear", 2), ("fig", 1)] Map.empty)) [("fig", 2), ("pear", 2)]

test> Stock.tests.report = test.verify do
  ensureEqual (Stock.report (Map.fromList [("fig", 2)])) ["fig: 2"]

test> Sku.tests.parse = test.verify do
  ensureEqual (Optional.map Sku.code (Sku.parse "ABC123")) (Some "ABC123")
  ensureEqual (Sku.parse "AB") None
````

``` ucm
scratch/main> add
scratch/main> test
```
