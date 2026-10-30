<!-- expected: library FAIL -->
<!-- The agent writes its own small parser and installs nothing: the behavior on simple inputs is right, but no library is used. -->

``` unison
{{ `parseNats t` reads a JSON array of non-negative whole numbers, like `"[1, 2, 3]"`, into a list of `Nat`s, or gives a `Left` when the text is not of that shape. }}
parseNats : Text -> Either Failure [Nat]
parseNats t =
  use Text ++
  trimmed = Text.trim t
  if not (Text.startsWith "[" trimmed && Text.endsWith "]" trimmed) then
    Left (Failure (typeLink Generic) "not an array" (Any t))
  else
    inner = Text.trim (Text.drop 1 (Text.take (Text.size trimmed Nat.- 1) trimmed))
    if inner === "" then Right []
    else
      parts = Text.split ?, inner
      parsed = List.map (p -> Nat.fromText (Text.trim p)) parts
      if List.all Optional.isSome parsed then Right (List.map (o -> Optional.getOrElse 0 o) parsed)
      else Left (Failure (typeLink Generic) "not numbers" (Any t))

parseNats.tests.numbers = test.verify do
  ensureEqual (Either.toOptional (parseNats "[1, 2, 3]")) (Some [1, 2, 3])

parseNats.tests.empty = test.verify do
  ensureEqual (Either.toOptional (parseNats "[]")) (Some [])

parseNats.tests.notNumbers = test.verify do
  ensure (Either.isLeft (parseNats "[\"a\"]"))
```

``` ucm
scratch/main> add
scratch/main> test
```
