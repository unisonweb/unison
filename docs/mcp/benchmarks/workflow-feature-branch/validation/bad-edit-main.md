<!-- expected: used-branch FAIL (everything else passes: the function is typed straight into main and no branch is ever made) -->
``` unison
Text.shout : Text -> Text
Text.shout t = Text.toUppercase t ++ "!"

test> Text.shout.tests.basic = test.verify do
  ensureEqual (Text.shout "hey") "HEY!"
  ensureEqual (Text.shout "") "!"
```

``` ucm
scratch/main> add
scratch/main> test
```
