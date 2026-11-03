<!-- The update conflict flow. Adding a constructor makes the functions that match on `Shape` without a wildcard stop
     typechecking, so the first `update` can't finish: it leaves the temporary branch `update-main` and a file (named like
     `update-main-<hash>.u`) with the new type and every dependent, the ones that still typecheck (`Shape.corners`, which
     has a wildcard, and `Shape.isSquare`) included. `good.u` is that file after editing by hand: a case in each function,
     `Shape.corners` made explicit (its wildcard would have said 4 for the triangle, and it still typechecks), tests for
     the triangle. A worker edits the file in place; here a copy stands in. Run with the directory of this file as the
     working directory. -->

``` unison
type Shape = Square Float | Rect Float Float | Circle Float | RightTriangle Float Float
```

``` ucm :error
scratch/main> update
```

``` ucm
scratch/update-main> load good.u
scratch/update-main> update
scratch/main> test
scratch/main> branches
scratch/main> todo
```
