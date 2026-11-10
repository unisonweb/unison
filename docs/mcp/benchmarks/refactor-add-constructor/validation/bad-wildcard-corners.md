<!-- expected: behavior FAIL, outcome FAIL -->
<!-- Every function that stopped typechecking got its case, but `Shape.corners`, which has a wildcard and so still typechecked, was left alone and says 4 for the triangle. Run with the directory of this file as the working directory. -->

``` unison
type Shape = Square Float | Rect Float Float | Circle Float | RightTriangle Float Float
```

``` ucm :error
scratch/main> update
```

``` ucm
scratch/update-main> load bad-wildcard-corners.u
scratch/update-main> update
scratch/main> test
```
