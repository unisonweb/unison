module Elmz.Movement where

import Keyboard
import Signal ((<~), (~), Signal)
import Signal
import Elmz.Signal as Signals
import Time

type Sign = Zero | Positive | Negative
-- A movement in one dimension
type D1 = D1 Sign
-- A movement in two dimensions
type D2 = D2 Sign Sign
-- A movement in three dimensions
type D3 = D3 Sign Sign Sign

sign : Int -> Sign
sign i = if | i == 0 -> Zero
            | i < 0 -> Negative
            | i > 0 -> Positive

inverseSign : Int -> Sign
inverseSign i = sign (-i)

d1 : Int -> D1
d1 = sign >> D1

{-| Up increments, down decremnts -}
upDown : Signal D1
upDown = Signal.map (.y >> d1) Keyboard.arrows

{-| Down increments, up decremnts -}
downUp : Signal D1
downUp = Signal.map (.y >> inverseSign >> D1) Keyboard.arrows

d1b : Bool -> D1
d1b t = D1 (movement1 t)

d2 : {x: Int, y: Int} -> D2
d2 {x,y} = D2 (sign x) (sign y)

movement1 : Bool -> Sign
movement1 t = if t then Positive else Negative

d3 : {x: Int, y: Int, z: Int} -> D3
d3 {x,y,z} = D3 (sign x) (sign y) (sign z)

d3mod : {x: Int, y: Int} -> Bool -> D3
d3mod {x,y} modifier = if modifier then D3 (sign x) Zero (sign y) else D3 (sign x) (sign y) Zero

-- lifted versions
d1' s = d1 <~ s
d1b' s = d1b <~ s
d2' s = d2 <~ s
d3mod' xy modifier = d3mod <~ xy ~ modifier

nonzeroD1 : D1 -> Bool
nonzeroD1 (D1 x) = x /= Zero

nonzeroD2 : D2 -> Bool
nonzeroD2 (D2 x y) = x /= Zero || y /= Zero

nonzeroD3 : D3 -> Bool
nonzeroD3 (D3 x y z) = x /= Zero || y /= Zero || z /= Zero

repeatD1 : Signal D1 -> Signal D1
repeatD1 d1 = Signals.repeatAfterIf (300 * Time.millisecond) 20 nonzeroD1 d1

repeatD2 : Signal D2 -> Signal D2
repeatD2 d2 = Signals.repeatAfterIf (300 * Time.millisecond) 20 nonzeroD2 d2

repeatD3 : Signal D3 -> Signal D3
repeatD3 d3 = Signals.repeatAfterIf (300 * Time.millisecond) 20 nonzeroD3 d3

moveD1 : { increment : s -> s, decrement : s -> s }
      -> Signal r -> Signal s -> Signal D1 -> Signal (Maybe s)
moveD1 mover reset base movements =
  let edit (D1 x) s = s |>
    (if | x == Positive -> mover.increment
        | x == Negative -> mover.decrement
        | otherwise -> identity)
  in Signals.foldpBetween'
    reset
    edit
    base
    movements

moveD2 : { left : s -> s, right : s -> s, up : s -> s, down : s -> s}
       -> Signal r -> Signal s -> Signal D2 -> Signal (Maybe s)
moveD2 mover reset base movements =
  let edit (D2 x y) s = s |>
    (if y == Positive then mover.up else identity) |>
    (if y == Negative then mover.down else identity) |>
    (if x == Positive then mover.right else identity) |>
    (if x == Negative then mover.left else identity)
  in Signals.foldpBetween'
    reset
    edit
    base
    movements
