<!-- expected: outcome FAIL and used-branch FAIL (the work stays on shout, which is the current branch; main has no Text.shout) -->
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
scratch/shout> test
```
