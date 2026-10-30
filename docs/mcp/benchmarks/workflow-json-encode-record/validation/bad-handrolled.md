<!-- expected: library-choice FAIL (and behavior FAIL: other key order and missing fields are mishandled; JSON text built by hand, no library) -->
``` unison
Person.toJson : Person -> Text
Person.toJson p =
  "{\"name\": \"" ++ Person.name p ++ "\", \"age\": " ++ Nat.toText (Person.age p) ++ "}"

Person.fromJson : Text -> Either Failure Person
Person.fromJson txt =
  use Text ++
  match Text.split ?, (Text.drop 1 (Text.dropEnd 1 txt)) with
    [n, a] ->
      match (Text.split ?: n, Text.split ?: a) with
        ([_, nv], [_, av]) ->
          match Nat.fromText (Text.trim av) with
            Some age -> Right (Person (Text.trim nv |> Text.drop 1 |> Text.dropEnd 1) age)
            None -> Left (Failure (typeLink Generic) "bad age" (Any ()))
        _ -> Left (Failure (typeLink Generic) "bad fields" (Any ()))
    _ -> Left (Failure (typeLink Generic) "bad object" (Any ()))

test> Person.tests.roundTrip = test.verify do
  ensureEqual (Person.fromJson (Person.toJson (Person "Ada" 36))) (Right (Person "Ada" 36))

test> Person.tests.errors = test.verify do
  ensure (Either.isLeft (Person.fromJson "not json"))
```

``` ucm
scratch/main> add
scratch/main> test
```
