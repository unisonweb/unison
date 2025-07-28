``` unison

mutual0 = cases
  0 -> "okay"
  n ->
    _ = openFile
    mutual1 (drop n 1)

mutual1 n =
  mutual0 n

mkTestCase = do
  saveTestCase None "case-04" "v4" mutual1 5
  saveTestCase (Some 5) "case-04" "v5" mutual1 5
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + mkTestCase : '{IO, Exception} ()
  + mutual0    : Nat -> Text
  + mutual1    : Nat -> Text

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> run mkTestCase

  ()
```
