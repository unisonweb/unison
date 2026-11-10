<!-- expected: library FAIL -->
<!-- The agent writes its own small scanner and installs nothing: the happy path on compact, ordered JSON works (and its tests only cover that), but no library is used and other key orders, white space and bad shapes are mishandled. -->

``` unison
-- the text after `"key":` up to the next `,` or `}` or `]`, without quotes
Order.fromJson.field : Text -> Text ->{Exception} Text
Order.fromJson.field key txt =
  use Text ++
  needle = "\"" ++ key ++ "\":"
  match Text.indexOf needle txt with
    None -> raise (Failure (typeLink Generic) ("missing " ++ key) (Any key))
    Some i ->
      rest = Text.drop (i + Text.size needle) txt
      Text.trim (Text.takeWhile (c -> c !== ?, && c !== ?} && c !== ?]) rest)
        |> Text.filter (c -> c !== ?")

Order.fromJson.nat : Text -> Text ->{Exception} Nat
Order.fromJson.nat key txt =
  match Nat.fromText (Order.fromJson.field key txt) with
    Some n -> n
    None -> raise (Failure (typeLink Generic) "not a number" (Any key))

Order.fromJson : Text ->{Exception} Order
Order.fromJson txt =
  compact = Text.filter (c -> c !== ?\s) txt
  itemsText = Text.takeWhile (c -> c !== ?]) (Text.dropWhile (c -> c !== ?[) compact)
  rawItems = List.drop 1 (Text.split ?{ itemsText)
  items = List.map (i -> LineItem (Order.fromJson.field "sku" i) (Order.fromJson.nat "quantity" i) (Order.fromJson.nat "priceCents" i)) rawItems
  note = Optional.map (_ -> Order.fromJson.field "note" compact) (Text.indexOf "\"note\":" compact)
  Order (Order.fromJson.field "customer" compact) items note

test> Order.fromJson.tests.full = test.verify do
  ensureEqual
    (Order.fromJson "{\"customer\":\"Ada\",\"items\":[{\"sku\":\"A-1\",\"quantity\":2,\"priceCents\":1250}],\"note\":\"gift\"}")
    (Order "Ada" [LineItem "A-1" 2 1250] (Some "gift"))
```

``` ucm
scratch/main> add
scratch/main> test
```
