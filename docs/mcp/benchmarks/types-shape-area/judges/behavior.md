You are judging `Shape` in the Unison codebase you are connected to. A previous agent wrote it, with tests. You may use any tool except `learn`. Don't change the project's definitions; use scratch code only for your own checks.

Run each of the following, and note the result of each:

1. `view Shape` and `view Shape.area` and `view Shape.largest`: `Shape` is a type with three constructors (circle, rectangle, triangle), `Shape.area : Shape -> Float`, and `Shape.largest : [Shape] -> Optional Shape`.
2. Using the constructor names you find, evaluate the area of a rectangle `2.0` by `3.0`: it is `6.0`.
3. The area of a circle of radius `1.0` is within `0.0001` of `3.14159`, and of a right triangle with legs `3.0` and `4.0` is `6.0`.
4. `Shape.largest` on a list of a rectangle `2.0` by `3.0`, a circle of radius `1.0` and a triangle with legs `3.0` and `5.0` returns the triangle (area `7.5`).
5. `Shape.largest []` is `None`.

`Shape` passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
