<!-- Needs the network (a real install from Share). Run on the codebase prepare.sh makes from setup.md. -->

``` ucm
scratch/main> lib.install @unison/json/releases/latest
```

``` unison
Order.fromJson : Text ->{Exception} Order
Order.fromJson txt =
  lineItem = do
    LineItem
      (Decoder.object.at! "sku" Decoder.text)
      (Decoder.object.at! "quantity" Decoder.nat)
      (Decoder.object.at! "priceCents" Decoder.nat)
  Decoder.run
    (do
      Order
        (Decoder.object.at! "customer" Decoder.text)
        (Decoder.object.at! "items" (Decoder.array lineItem))
        (Decoder.object.optionalAt! "note" Decoder.text))
    txt

Order.fromJson.doc : Doc
Order.fromJson.doc =
  {{
  Reads an order from JSON text: an object with a `customer`, a list of `items`
  (each with a `sku`, a `quantity` and a `priceCents`) and an optional `note`.
  Raises an exception when the text is not valid JSON or does not have that shape.
  }}

test> Order.fromJson.tests.full = test.verify do
  ensureEqual
    (Order.fromJson "{\"customer\": \"Ada\", \"items\": [{\"sku\": \"A-1\", \"quantity\": 2, \"priceCents\": 1250}], \"note\": \"gift\"}")
    (Order "Ada" [LineItem "A-1" 2 1250] (Some "gift"))

test> Order.fromJson.tests.noNote = test.verify do
  ensureEqual
    (Order.fromJson "{\"items\": [], \"customer\": \"Bo\"}")
    (Order "Bo" [] None)

test> Order.fromJson.tests.failures = test.verify do
  ensure (Either.isLeft (catch do Order.fromJson "not json"))
  ensure (Either.isLeft (catch do Order.fromJson "[1, 2]"))
  ensure (Either.isLeft (catch do Order.fromJson "{\"items\": []}"))
  ensure (Either.isLeft (catch do Order.fromJson "{\"customer\": \"Ada\", \"items\": [{\"sku\": \"A\", \"quantity\": \"2\", \"priceCents\": 5}]}"))
```

``` ucm
scratch/main> add
scratch/main> test
scratch/main> list lib
```
