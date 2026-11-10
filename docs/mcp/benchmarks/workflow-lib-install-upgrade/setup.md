This setup needs the network: it installs a real release of `@unison/json` from Unison Share. The release is pinned, so Share releases appearing later change nothing here.

``` ucm
scratch/main> lib.install @unison/json/releases/1.2.1
```

``` unison
{{ `parseNats t` reads the JSON text `t`, which must be an array of non-negative whole numbers, into a list of `Nat`s. It fails with an `Exception` (here caught into the `Either`) when the text is not valid JSON or has other contents. }}
parseNats : Text -> Either Failure [Nat]
parseNats t = catch do Decoder.run (Decoder.array Decoder.nat) t

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
scratch/main> list lib
scratch/main> dependencies parseNats
```
