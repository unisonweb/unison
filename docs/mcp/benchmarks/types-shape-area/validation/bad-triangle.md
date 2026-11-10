``` unison
type Shape = Circle Float | Rectangle Float Float | RightTriangle Float Float

Shape.area : Shape -> Float
Shape.area = cases
  Circle r -> Float.pi * r * r
  Rectangle w h -> w * h
  RightTriangle a b -> a * b

Shape.largest : [Shape] -> Optional Shape
Shape.largest = List.maximumOn Universal.ordering Shape.area

test> Shape.area.tests.example = test.verify do
  ensureEqual (Shape.area (Rectangle 2.0 3.0)) 6.0
  ensure (Float.abs (Shape.area (Circle 1.0) - 3.14159) < 0.0001)
  ensureEqual (Shape.largest [Rectangle 2.0 3.0, Circle 1.0, RightTriangle 3.0 5.0]) (Some (RightTriangle 3.0 5.0))
  ensureEqual (Shape.largest []) None
```
``` ucm
scratch/main> add
scratch/main> test
```
