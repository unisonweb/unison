<!-- expected: behavior FAIL and outcome PASS (clean branch workflow, but the function forgets the exclamation mark for the empty text and the test never looks) -->
``` ucm
scratch/main> branch shout
```

``` unison
Text.shout : Text -> Text
Text.shout t = if Text.isEmpty t then t else Text.toUppercase t ++ "!"

test> Text.shout.tests.basic = test.verify do
  ensureEqual (Text.shout "hey") "HEY!"
```

``` ucm
scratch/shout> add
scratch/shout> test
scratch/main> merge /shout
scratch/main> delete.branch scratch/shout
scratch/main> test
```
