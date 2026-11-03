<!-- expected: outcome FAIL (the work is merged into main, but the agent stays on shout and the branch is not deleted) -->
``` ucm
scratch/main> branch shout
```

``` unison
Text.shout : Text -> Text
Text.shout t = Text.toUppercase t ++ "!"

test> Text.shout.tests.basic = test.verify do
  ensureEqual (Text.shout "hey") "HEY!"
  ensureEqual (Text.shout "") "!"
```

``` ucm
scratch/shout> add
scratch/main> merge /shout
scratch/shout> test
```
