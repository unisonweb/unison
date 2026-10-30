You are judging the behavior of the functions on `Shape` in the Unison codebase you are connected to. A previous agent added the constructor `Shape.RightTriangle Float Float` (the lengths of the two legs). You may use any tool except `learn`. Don't change the project's definitions; use a `unison` block of watch expressions (`> expr`) for your own checks and don't `update` it. Work in `scratch/main`.

Evaluate each of these and compare with the expected value:

1. `Shape.area (RightTriangle 3.0 4.0)` is `6.0`; `Shape.area (RightTriangle 6.0 8.0)` is `24.0`; `Shape.area (RightTriangle 1.0 1.0)` is `0.5`.
2. `Shape.perimeter (RightTriangle 3.0 4.0)` is `12.0` (the hypotenuse is `5.0`); `Shape.perimeter (RightTriangle 5.0 12.0)` is `30.0`; `Shape.perimeter (RightTriangle 6.0 8.0)` is `24.0`.
3. `Shape.describe (RightTriangle 3.0 4.0)` is `"a right triangle with legs 3.0 and 4.0"` and `Shape.describe (RightTriangle 1.5 2.0)` is `"a right triangle with legs 1.5 and 2.0"`.
4. `Shape.name (RightTriangle 3.0 4.0)` is `"right triangle"`.
5. `Shape.scale 2.0 (RightTriangle 3.0 4.0)` is `RightTriangle 6.0 8.0` and `Shape.scale 0.5 (RightTriangle 3.0 4.0)` is `RightTriangle 1.5 2.0`.
6. `Shape.corners (RightTriangle 3.0 4.0)` is `3`, and the others are as before: `Shape.corners (Circle 1.0)` is `0`, `Shape.corners (Square 1.0)` and `Shape.corners (Rect 1.0 2.0)` are `4`.
7. `Shape.isSquare (RightTriangle 1.0 1.0)` is `false` and `Shape.isSquare (Square 1.0)` is `true`.
8. `Shape.totalArea [RightTriangle 3.0 4.0, Square 2.0]` is `10.0` and `Shape.report [RightTriangle 3.0 4.0]` is `["right triangle: 6.0"]`.
9. The old shapes behave as before: `Shape.area (Circle 1.0)` is `3.141592653589793`, `Shape.perimeter (Circle 1.0)` is `6.283185307179586`, `Shape.area (Square 3.0)` is `9.0`, `Shape.describe (Circle 1.0)` is `"a circle of radius 1.0"`, `Shape.describe (Rect 2.0 3.0)` is `"a 2.0 by 3.0 rectangle"`, `Shape.scale 2.0 (Rect 1.0 3.0)` is `Rect 2.0 6.0`, `Shape.name (Square 1.0)` is `"square"`.

It passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
