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
scratch/main> merge /shout
scratch/main> delete.branch scratch/shout
scratch/main> test
scratch/main> branches
```
