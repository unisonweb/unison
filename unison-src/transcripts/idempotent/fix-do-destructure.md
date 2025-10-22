``` ucm :hide
> builtins.merge
```

``` unison
a zoink.++ b = ()

a foo.++ b = "abracadabra"

example = do 
  use foo ++
  (x, y) = ("hi", "there")
  x ++ y ++ "z" ++ "a" ++ "b" ++ "c"

example2 =
  use foo ++
  (x, y) = ("hi", "there")
  x ++ y ++ "z" ++ "a" ++ "b" ++ "c"

blah : [a] -> Text 
blah =
  use foo ++
  go = cases
    (a1, a2) ->
      cases
        [] -> "finished" 
        x +: xs -> "hi" ++ "bye" ++ "yay" ++ "nay"
  go ([], [])

blah2 : [a] -> Text 
blah2 =
  use foo ++
  go a1 a2 = cases
        [] -> "finished" 
        x +: xs -> "hi" ++ "bye" ++ "yay" ++ "nay"
  go [] []
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + blah       : [a] -> Text
  + blah2      : [a] -> Text
  + example    : 'Text
  + example2   : Text
  + (foo.++)   : a -> b -> Text
  + (zoink.++) : a -> b -> ()

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

> view example example2 blah blah2

  blah : [a] -> Text
  blah =
    use foo ++
    go = cases
      (a1, a2) ->
        cases
          []      -> "finished"
          x +: xs -> "hi" ++ "bye" ++ "yay" ++ "nay"
    go ([], [])

  blah2 : [a] -> Text
  blah2 =
    use foo ++
    go a1 a2 = cases
      []      -> "finished"
      x +: xs -> "hi" ++ "bye" ++ "yay" ++ "nay"
    go [] []

  example : 'Text
  example = do
    use foo ++
    (x, y) = ("hi", "there")
    x ++ y ++ "z" ++ "a" ++ "b" ++ "c"

  example2 : Text
  example2 =
    use foo ++
    (x, y) = ("hi", "there")
    x ++ y ++ "z" ++ "a" ++ "b" ++ "c"
```
