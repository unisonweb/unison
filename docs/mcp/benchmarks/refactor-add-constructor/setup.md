A `Shape` type with three constructors and seven functions on it, all numeric or Boolean: `Shape.area` (with a doc), `Shape.perimeter`, `Shape.scale`, `Shape.hasCurve` (explicit cases), `Shape.corners` and `Shape.isSquare` (both end in a wildcard, so they still typecheck after a constructor is added, and `corners` then answers 4 for anything that is not a circle), and `Shape.totalArea` (uses `area`, needs no change).

```` unison
Geometry.pi : Float
Geometry.pi = 3.141592653589793

Geometry.square : Float -> Float
Geometry.square x = x * x

type Shape = Square Float | Rect Float Float | Circle Float

Shape.area : Shape -> Float
Shape.area = cases
  Square s -> Geometry.square s
  Rect w h -> w * h
  Circle r -> Geometry.pi * Geometry.square r

Shape.area.doc = {{
{{ docExample 1 (do (shape -> Shape.area shape)) }} is the area of `shape`. For example:

@eval{ Shape.area (Rect 2.0 3.0) }
}}

Shape.perimeter : Shape -> Float
Shape.perimeter = cases
  Square s -> 4.0 * s
  Rect w h -> 2.0 * (w + h)
  Circle r -> 2.0 * Geometry.pi * r

Shape.scale : Float -> Shape -> Shape
Shape.scale k = cases
  Square s -> Square (k * s)
  Rect w h -> Rect (k * w) (k * h)
  Circle r -> Circle (k * r)

Shape.hasCurve : Shape -> Boolean
Shape.hasCurve = cases
  Square _ -> false
  Rect _ _ -> false
  Circle _ -> true

Shape.corners : Shape -> Nat
Shape.corners = cases
  Circle _ -> 0
  _ -> 4

Shape.isSquare : Shape -> Boolean
Shape.isSquare = cases
  Square _ -> true
  _ -> false

Shape.totalArea : [Shape] -> Float
Shape.totalArea shapes = List.foldLeft (total shape -> total + Shape.area shape) 0.0 shapes

test> Shape.tests.area = test.verify do
  ensureEqual (Shape.area (Square 3.0)) 9.0
  ensureEqual (Shape.area (Rect 2.0 5.0)) 10.0
  ensureEqual (Shape.totalArea [Square 1.0, Rect 2.0 3.0]) 7.0

test> Shape.tests.perimeter = test.verify do
  ensureEqual (Shape.perimeter (Square 3.0)) 12.0
  ensureEqual (Shape.perimeter (Rect 2.0 5.0)) 14.0

test> Shape.tests.scale = test.verify do
  ensureEqual (Shape.scale 2.0 (Rect 1.0 3.0)) (Rect 2.0 6.0)
  ensureEqual (Shape.scale 3.0 (Square 1.0)) (Square 3.0)

test> Shape.tests.corners = test.verify do
  ensureEqual (Shape.corners (Rect 1.0 1.0)) 4
  ensureEqual (Shape.corners (Circle 1.0)) 0
  ensureEqual (Shape.hasCurve (Circle 1.0), Shape.hasCurve (Square 1.0)) (true, false)
  ensureEqual (Shape.isSquare (Square 1.0), Shape.isSquare (Rect 1.0 2.0)) (true, false)
````

``` ucm
scratch/main> add
scratch/main> test
```
