<!-- expected: outcome FAIL (Text.slug was brought over without its test) -->
``` ucm
scratch/merge-bob-into-alice> cancel
scratch/alice> ls
```

``` unison
Text.slug : Text -> Text
Text.slug t = Text.replaceAll " " "-" (Text.toLowercase t)
```

``` ucm
scratch/alice> update
scratch/alice> test
```
