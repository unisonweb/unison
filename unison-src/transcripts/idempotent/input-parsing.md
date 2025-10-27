Should parse quoted arguments as expected.

``` ucm
scratch/main> builtins.mergeio

  Done.
```

``` unison
main = do
  getArgs.impl ()
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + main : '{IO} Either Failure [Text]

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> run main "all one arg" "contains escaped \" quote" second third

  Right
    [ "all one arg"
    , "contains escaped \" quote"
    , "second"
    , "third"
    ]
```
