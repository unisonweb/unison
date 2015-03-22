module Elmz.Mealy where

import Debug
import Elmz.Moore (Moore)
import Elmz.Moore as M

type alias Mealy i o = i -> Moore i o

ap : Mealy i (a -> b) -> Mealy i a -> Mealy i b
ap = map2 (<|)

delay : a -> Mealy a a
delay a0 a = moore a0 (delay a)

echo : Mealy a a
echo = lift identity

lift : (a -> b) -> Mealy a b
lift f a = moore (f a) (lift f)

map : (b -> c) -> Mealy a b -> Mealy a c
map f m a = M.map f (m a)

map2 : (a -> b -> c) -> Mealy i a -> Mealy i b -> Mealy i c
map2 f a b i =
  let (ar,br) = (a i, b i)
  in moore (f (M.extract ar) (M.extract br)) (map2 f (M.step ar) (M.step br))

mealy : (i -> Moore i o) -> Mealy i o
mealy = identity

moore : o -> Mealy i o -> Moore i o
moore = M.moore

pipe : Mealy a b -> Mealy b c -> Mealy a c
pipe ab bc a =
  let m1 = ab a
      m2 = bc (M.extract m1)
  in m1 `M.pipe` m2

-- first : Mealy a b -> Mealy (a,c) (b,c)
-- second : Mealy a b -> Mealy (c,a) (c,b)

pipe1 : Mealy a (b,c) -> Mealy b b2 -> Mealy a (b2,c)
pipe1 m1 m2 a =
  let m1' = m1 a
  in m1' `M.pipe1` (m2 (fst (M.extract m1')))

pipe2 : Mealy a (b,c) -> Mealy c c2 -> Mealy a (b,c2)
pipe2 i c =
  let swap (a,b) = (b,a)
  in map swap (pipe1 (map swap i) c)

split : Mealy a b -> Mealy a (b,b)
split = map (\b -> (b,b))

loop : c -> Mealy (a,c) (b,c) -> Mealy a b
loop c m a = M.loop (m (a,c))

-- withInput : Mealy i o -> Mealy i (i,o)
-- withInput m i = moore

