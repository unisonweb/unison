<!-- The worker looks at Bob's function first (scratch/bob> view Text.slug Text.slug.tests.example) and retypes it; cancel is the first step. The last command before the unison block must be on alice, since a unison block is checked against the branch of the previous command. -->
``` ucm
scratch/merge-bob-into-alice> cancel
scratch/bob> view Text.slug Text.slug.tests.example
scratch/alice> ls
```

``` unison
Text.slug : Text -> Text
Text.slug t = Text.replaceAll " " "-" (Text.toLowercase t)

test> Text.slug.tests.example = test.verify do
  ensureEqual (Text.slug "Hello World") "hello-world"
  ensureEqual (Text.slug "Already-fine") "already-fine"
```

``` ucm
scratch/alice> update
scratch/alice> test
```
