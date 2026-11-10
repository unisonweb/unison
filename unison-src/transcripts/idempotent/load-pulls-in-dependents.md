# A file that uses a definition which uses what the file redefines

`totalArea` uses `area`, which matches on `Shape`. A file that redefines `Shape` (a new constructor) and `area`, and has a
test that calls `totalArea`, would see two `Shape`s: its own, and the codebase's through `totalArea`. So `load` makes
`totalArea` part of the file, unchanged, and says so; `update` then saves it rebuilt on the new `Shape`.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

``` unison :hide
type Shape = Square Float | Circle Float

Shape.area : Shape -> Float
Shape.area = cases
  Square s -> s * s
  Circle r -> 3.0 * r * r

Shape.totalArea : [Shape] -> Float
Shape.totalArea = cases
  [] -> 0.0
  shape +: rest -> Shape.area shape + Shape.totalArea rest

Shape.isSquare : Shape -> Boolean
Shape.isSquare = cases
  Square _ -> true
  _ -> false
```

``` ucm :hide
scratch/main> add
```

``` unison
type Shape = Square Float | Circle Float | Triangle Float Float

Shape.area : Shape -> Float
Shape.area = cases
  Square s -> s * s
  Circle r -> 3.0 * r * r
  Triangle b h -> 0.5 * b * h

Shape.check : (Float, Boolean)
Shape.check = (Shape.totalArea [Square 2.0, Triangle 2.0 3.0], Shape.isSquare (Triangle 1.0 1.0))
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  Also typechecking, from the codebase:
  Shape.isSquare, Shape.totalArea. They use what this file
  redefines, so they are part of the file now, and `update`
  saves them rebuilt on the new definition.

  ~ type Shape

  + Shape.check : (Float, Boolean)
  ~ Shape.area      : Shape -> Float
  ~ Shape.isSquare  : Shape -> Boolean
  ~ Shape.totalArea : [Shape] -> Float

  + (added), ~ (modified)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> view Shape.totalArea Shape.isSquare

  Shape.isSquare : Shape -> Boolean
  Shape.isSquare = cases
    Square _ -> true
    _        -> false

  Shape.totalArea : [Shape] -> Float
  Shape.totalArea = cases
    []            -> 0.0
    shape +: rest -> area shape Float.+ Shape.totalArea rest
```

``` unison
> Shape.check
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.

    1 | > Shape.check
          ⧩
          (7.0, false)
```

A fold line (`---`) hides the rest of a file. The pulled-in definitions go above it, so they are still read, and what
is below the fold stays ignored:

``` unison
type Shape = Square Float | Circle Float | Triangle Float Float | Hexagon Float

Shape.area : Shape -> Float
Shape.area = cases
  Square s -> s * s
  Circle r -> 3.0 * r * r
  Triangle b h -> 0.5 * b * h
  Hexagon s -> 2.6 * s * s

Shape.check : (Float, Boolean)
Shape.check = (Shape.totalArea [Hexagon 1.0], Shape.isSquare (Hexagon 1.0))

---- Anything below this line is ignored by Unison.

Shape.ignored = "not read"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  Also typechecking, from the codebase:
  Shape.isSquare, Shape.totalArea. They use what this file
  redefines, so they are part of the file now, and `update`
  saves them rebuilt on the new definition.

  ~ type Shape

  ~ Shape.area      : Shape -> Float
  ~ Shape.check     : (Float, Boolean)
  ~ Shape.isSquare  : Shape -> Boolean
  ~ Shape.totalArea : [Shape] -> Float

  ~ (modified)

  Run `update` to apply these changes to your codebase.
```
