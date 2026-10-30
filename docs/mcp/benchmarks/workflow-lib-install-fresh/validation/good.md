<!-- Needs the network (a real install from Share). -->

``` ucm
scratch/main> list lib
scratch/main> lib.install @unison/json/releases/latest
```

```` unison
{{ `parseNats t` reads the JSON text `t`, which must be an array of non-negative whole numbers, into a list of `Nat`s. It gives a `Left` when the text is not valid JSON or has other contents. For example:

```
parseNats "[1, 2, 3]"
```
}}
parseNats : Text -> Either Failure [Nat]
parseNats t = catch do Decoder.run (Decoder.array Decoder.nat) t

parseNats.tests.numbers = test.verify do
  ensureEqual (Either.toOptional (parseNats "[1, 2, 3]")) (Some [1, 2, 3])

parseNats.tests.empty = test.verify do
  ensureEqual (Either.toOptional (parseNats "[]")) (Some [])

parseNats.tests.notNumbers = test.verify do
  ensure (Either.isLeft (parseNats "[\"a\"]"))

parseNats.tests.negative = test.verify do
  ensure (Either.isLeft (parseNats "[-1]"))

parseNats.tests.notJson = test.verify do
  ensure (Either.isLeft (parseNats "[1, 2"))
````

``` ucm
scratch/main> add
scratch/main> test
scratch/main> list lib
scratch/main> docs parseNats
```
