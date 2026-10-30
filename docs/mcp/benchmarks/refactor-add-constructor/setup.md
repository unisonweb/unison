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

Shape.describe : Shape -> Text
Shape.describe = cases
  Square s -> "a square of side " ++ Float.toText s
  Rect w h -> "a " ++ Float.toText w ++ " by " ++ Float.toText h ++ " rectangle"
  Circle r -> "a circle of radius " ++ Float.toText r

Shape.name : Shape -> Text
Shape.name = cases
  Square _ -> "square"
  Rect _ _ -> "rectangle"
  Circle _ -> "circle"

Shape.scale : Float -> Shape -> Shape
Shape.scale k = cases
  Square s -> Square (k * s)
  Rect w h -> Rect (k * w) (k * h)
  Circle r -> Circle (k * r)

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

Shape.report : [Shape] -> [Text]
Shape.report shapes = List.map (shape -> Shape.name shape ++ ": " ++ Float.toText (Shape.area shape)) shapes

test> Shape.tests.area = test.verify do
  ensureEqual (Shape.area (Square 3.0)) 9.0
  ensureEqual (Shape.area (Rect 2.0 5.0)) 10.0
  ensureEqual (Shape.totalArea [Square 1.0, Rect 2.0 3.0]) 7.0

test> Shape.tests.perimeter = test.verify do
  ensureEqual (Shape.perimeter (Square 3.0)) 12.0
  ensureEqual (Shape.perimeter (Rect 2.0 5.0)) 14.0

test> Shape.tests.describe = test.verify do
  ensureEqual (Shape.describe (Square 2.0)) "a square of side 2.0"
  ensureEqual (Shape.describe (Rect 2.0 3.0)) "a 2.0 by 3.0 rectangle"
  ensureEqual (Shape.describe (Circle 1.0)) "a circle of radius 1.0"

test> Shape.tests.scale = test.verify do
  ensureEqual (Shape.scale 2.0 (Rect 1.0 3.0)) (Rect 2.0 6.0)
  ensureEqual (Shape.name (Circle 1.0)) "circle"
  ensureEqual (Shape.corners (Rect 1.0 1.0)) 4
  ensureEqual (Shape.report [Square 2.0]) ["square: 4.0"]
````

``` ucm
scratch/main> add
scratch/main> test
```
