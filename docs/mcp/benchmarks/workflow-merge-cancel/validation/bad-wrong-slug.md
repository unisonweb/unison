<!-- expected: behavior FAIL (a slug of its own that keeps capitals; outcome checks the hash against Bob's and fails too) -->
``` ucm
scratch/merge-bob-into-alice> cancel
scratch/alice> ls
```

``` unison
Text.slug : Text -> Text
Text.slug t = Text.replaceAll " " "-" t

test> Text.slug.tests.example = test.verify do
  ensureEqual (Text.slug "hello world") "hello-world"
```

``` ucm
scratch/alice> update
scratch/alice> test
```
