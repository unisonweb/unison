You are judging the behavior of the functions on `Shape` in the Unison codebase you are connected to. A previous agent added the constructor `Shape.RightTriangle Float Float` to `Shape` (the fields are the lengths of the two legs) and was expected to make every function on shapes give the right answer for it. The prompt did not list the answers, so here they are: for a right triangle with legs `a` and `b`, the area is `a * b / 2`, the perimeter is `a + b` plus the hypotenuse (the square root of `a * a + b * b`), it has 3 corners, it has no curve, it is not a square, and scaling by `k` multiplies both legs by `k`. You may use any tool except `learn`. Don't change the project's definitions; use a `unison` block of watch expressions (`> expr`) for your own checks and don't `update` it. Work in `scratch/main`.

Evaluate each of these and compare with the expected value:

1. `Shape.area (RightTriangle 3.0 4.0)` is `6.0`; `Shape.area (RightTriangle 6.0 8.0)` is `24.0`; `Shape.area (RightTriangle 1.0 1.0)` is `0.5`.
2. `Shape.perimeter (RightTriangle 3.0 4.0)` is `12.0` (the hypotenuse is `5.0`); `Shape.perimeter (RightTriangle 5.0 12.0)` is `30.0`; `Shape.perimeter (RightTriangle 6.0 8.0)` is `24.0`.
3. `Shape.scale 2.0 (RightTriangle 3.0 4.0)` is `RightTriangle 6.0 8.0` and `Shape.scale 0.5 (RightTriangle 3.0 4.0)` is `RightTriangle 1.5 2.0`.
4. `Shape.corners (RightTriangle 3.0 4.0)` is `3`, and the others are as before: `Shape.corners (Circle 1.0)` is `0`, `Shape.corners (Square 1.0)` and `Shape.corners (Rect 1.0 2.0)` are `4`.
5. `Shape.hasCurve (RightTriangle 3.0 4.0)` is `false`, `Shape.hasCurve (Circle 1.0)` is `true`, `Shape.hasCurve (Square 1.0)` is `false`.
6. `Shape.isSquare (RightTriangle 1.0 1.0)` is `false` and `Shape.isSquare (Square 1.0)` is `true`.
7. `Shape.totalArea [RightTriangle 3.0 4.0, Square 2.0]` is `10.0`.
8. The old shapes behave as before: `Shape.area (Circle 1.0)` is `3.141592653589793`, `Shape.perimeter (Circle 1.0)` is `6.283185307179586`, `Shape.area (Square 3.0)` is `9.0`, `Shape.area (Rect 2.0 5.0)` is `10.0`, `Shape.perimeter (Rect 2.0 5.0)` is `14.0`, `Shape.scale 2.0 (Rect 1.0 3.0)` is `Rect 2.0 6.0`.

It passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
