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

``` ucm
scratch/main> add
scratch/main> run mkTestCase
```
