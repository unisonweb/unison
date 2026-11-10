<!-- expected: behavior FAIL -->
<!-- The area of the triangle is `a * b` (the division by 2 is missing) and the agent wrote its own test to match. Run with the directory of this file as the working directory. -->

``` unison
type Shape = Square Float | Rect Float Float | Circle Float | RightTriangle Float Float
```

``` ucm :error
scratch/main> update
```

``` ucm
scratch/update-main> load bad-wrong-area.u
scratch/update-main> update
scratch/main> test
```
