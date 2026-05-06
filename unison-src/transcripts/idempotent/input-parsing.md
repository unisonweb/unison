Should parse quoted arguments as expected.

``` ucm
scratch/main> builtins.mergeio

  Done.
```

``` unison :hide
main = do
  getArgs.impl ()
```

``` ucm :hide
scratch/main> update
```

Quoting allows spaces in arguments.

``` ucm
scratch/main> run main "all one arg" "contains escaped \" quote" 'single quoted' second third

  Right
    [ "all one arg"
    , "contains escaped \" quote"
    , "single quoted"
    , "second"
    , "third"
    ]
```

Numbers are not expanded when used as non-structured arguments (this is command dependent)

``` ucm
scratch/main> ls

  1. builtin. (943 terms, 136 types)
  2. main     ('{IO} Either Failure [Text])

scratch/main> run main 1 2- 3-4

  Right ["1", "2-", "3-4"]
```

## Number expansion

Unquoted numbers are expanded to ranges.

``` unison :hide
a1 = "a1"
a2 = "a2"
a3 = "a3"
a4 = "a4"
a5 = "a5"
a6 = "a6"
a7 = "a7"
```

``` ucm :hide
scratch/numbers> update
```

``` ucm
scratch/numbers> ls

  1. a1 (##Text)
  2. a2 (##Text)
  3. a3 (##Text)
  4. a4 (##Text)
  5. a5 (##Text)
  6. a6 (##Text)
  7. a7 (##Text)

scratch/numbers> view 3

  a3 : ##Text
  a3 = "a3"

scratch/numbers> view -3

  a1 : ##Text
  a1 = "a1"

  a2 : ##Text
  a2 = "a2"

  a3 : ##Text
  a3 = "a3"

scratch/numbers> view 5-

  a5 : ##Text
  a5 = "a5"

  a6 : ##Text
  a6 = "a6"

  a7 : ##Text
  a7 = "a7"

scratch/numbers> view 3-5

  a3 : ##Text
  a3 = "a3"

  a4 : ##Text
  a4 = "a4"

  a5 : ##Text
  a5 = "a5"
```
