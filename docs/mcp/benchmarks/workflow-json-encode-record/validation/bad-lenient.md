<!-- expected: behavior FAIL (uses @unison/json, but a missing age silently becomes 0 and the tests never look) -->
``` ucm
scratch/main> lib.install @unison/json/releases/latest
```

``` unison
Person.toJson : Person -> Text
Person.toJson p =
  Json.toText
    (Json.object [("name", Json.text (Person.name p)), ("age", Json.nat (Person.age p))])

Person.fromJson : Text ->{Exception} Person
Person.fromJson txt =
  Decoder.run
    (do
      Person
        (Optional.getOrElse "" (Decoder.object.optionalAt! "name" Decoder.text))
        (Optional.getOrElse 0 (Decoder.object.optionalAt! "age" Decoder.nat)))
    txt

test> Person.tests.roundTrip = test.verify do
  ensureEqual (Person.fromJson (Person.toJson (Person "Ada" 36))) (Person "Ada" 36)

test> Person.tests.errors = test.verify do
  ensure (Either.isLeft (catch do Person.fromJson "not json"))
```

``` ucm
scratch/main> add
scratch/main> test
```
