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
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + example    : 'Text
  + (foo.++)   : a -> b -> Text
  + (zoink.++) : a -> b -> ()

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

> view example

  example : 'Text
  example = do
    use foo ++
    (x, y) = ("hi", "there")
    x ++ y ++ "z" ++ "a" ++ "b" ++ "c"
```
