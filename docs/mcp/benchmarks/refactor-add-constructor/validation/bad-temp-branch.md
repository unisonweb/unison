<!-- expected: outcome FAIL -->
<!-- Stops after the first update could not finish: the worker is left on update-main. -->

``` unison
type Shape = Square Float | Rect Float Float | Circle Float | RightTriangle Float Float
```

``` ucm :error
scratch/main> update
```
