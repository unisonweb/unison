<!-- expected: outcome FAIL -->
<!-- The conflict flow was finished with `Shape.report` and the documentation of `Shape.area` deleted from the file, which deletes them from the branch. Run with the directory of this file as the working directory. -->

``` unison
type Shape = Square Float | Rect Float Float | Circle Float | RightTriangle Float Float
```

``` ucm :error
scratch/main> update
```

``` ucm
scratch/update-main> load bad-dropped-functions.u
scratch/update-main> update
scratch/main> test
```
