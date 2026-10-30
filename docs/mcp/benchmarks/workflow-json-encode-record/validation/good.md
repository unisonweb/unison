<!-- Needs the network (the library install). Run on the codebase prepare.sh makes. -->
``` ucm
scratch/main> lib.install @unison/json/releases/latest
```

``` unison
Person.toJson : Person -> Text
Person.toJson p =
  Json.toText
    (Json.object [("name", Json.text (Person.name p)), ("age", Json.nat (Person.age p))])

Person.fromJson : Text -> Either Failure Person
Person.fromJson txt =
  catch do
    Decoder.run
      (do Person (Decoder.object.at! "name" Decoder.text) (Decoder.object.at! "age" Decoder.nat))
      txt

test> Person.tests.roundTrip = test.verify do
  ensureEqual (Person.fromJson (Person.toJson (Person "Ada" 36))) (Right (Person "Ada" 36))
  ensureEqual (Person.fromJson (Person.toJson (Person "" 0))) (Right (Person "" 0))
  ensureEqual
    (Person.fromJson (Person.toJson (Person "say \"hi\"\n" 7)))
    (Right (Person "say \"hi\"\n" 7))

test> Person.tests.errors = test.verify do
  ensure (Either.isLeft (Person.fromJson "not json"))
  ensure (Either.isLeft (Person.fromJson "{\"name\": \"Ada\"}"))
  ensure (Either.isLeft (Person.fromJson "{\"name\": \"Ada\", \"age\": \"x\"}"))
  ensureEqual (Person.fromJson "{\"age\": 3, \"name\": \"Bo\"}") (Right (Person "Bo" 3))
```

``` ucm
scratch/main> add
scratch/main> test
scratch/main> list lib
```
