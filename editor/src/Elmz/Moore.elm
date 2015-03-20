module Elmz.Moore where

import Signal
import Signal ((<~), (~), foldp, Signal)
import List ((::))

type Moore i o = Moore (i -> Bool) o (i -> Moore i o)

transform : Moore i o -> Signal i -> Signal o
transform m i =
  let s i m = if steady m i then m else step m i
  in extract <~ foldp s m i

{-| Unlike `transform`, only emits events when the input transitions to a new state. -}
transitions : Moore i o -> Signal i -> Signal o
transitions m i =
  let s i (m,_) = if steady m i then (m,False) else (step m i,True)
      states = foldp s (m,True) i
      changes = Signal.map snd states
  in Signal.keepWhen changes (extract m) ((extract << fst) <~ states)

extract : Moore i o -> o
extract (Moore _ o _) = o

step : Moore i o -> (i -> Moore i o)
step (Moore _ _ k) = k

steady : Moore i o -> i -> Bool
steady (Moore same _ _) = same

duplicate : Moore i o -> Moore i (Moore i o)
duplicate m = Moore (steady m) m (step m >> duplicate)

moore : o -> (i -> Moore i o) -> Moore i o
moore o k = Moore (always False) o k

{-| A machine which stays in the same state whenever the input matches the predicate. -}
skips : (i -> Bool) -> Moore i o -> Moore i o
skips f (Moore same o k) = Moore (\i -> f i || same i) o (k >> skips f)

{-| Like `skips`, but only skips up to the first transition to a new state. -}
skip : (i -> Bool) -> Moore i o -> Moore i o
skip f (Moore same o k) = Moore (\i -> f i || same i) o k

contramap : (i0 -> i) -> Moore i o -> Moore i0 o
contramap f (Moore same o k) = Moore (f >> same) o (f >> k >> contramap f)

map : (o -> o2) -> Moore i o -> Moore i o2
map f (Moore same o k) = Moore same (f o) (k >> map f)

unit : o -> Moore i o
unit o = Moore (always True) o (always (unit o))

map2 : (o1 -> o2 -> o3) -> Moore i o1 -> Moore i o2 -> Moore i o3
map2 f ((Moore same1 o1 k1) as m1) ((Moore same2 o2 k2) as m2) =
  Moore (\i -> same1 i && same2 i)
        (f o1 o2)
        (\i -> map2 f (if same1 i then m1 else k1 i)
                      (if same2 i then m2 else k2 i))

ap : Moore i (a -> b) -> Moore i a -> Moore i b
ap = map2 (<|)

emit : o -> Moore i o -> Moore i o
emit oz (Moore same o k) = Moore same oz (k >> emit o)

echo : o -> Moore o o
echo o = moore o echo

echo' : Moore (Maybe a) (Maybe a)
echo' = echo Nothing

dropRepeats : o -> Moore o o
dropRepeats prev = Moore ((==) prev) prev dropRepeats

changesBy : (a -> a -> Maybe b) -> Moore a (Maybe b)
changesBy f =
  let prev = contramap Just (emit Nothing echo')
      cur = contramap Just echo'
      g prev cur = case (prev,cur) of
        (Just a, Just a2) -> f a a2
        _ -> Nothing
  in map2 g prev cur

pipe : Moore a b -> Moore b c -> Moore a c
pipe (Moore same1 b k1) (Moore same2 c k2) =
  let step a = k1 a `pipe` k2 b
      same a = same1 a && same2 b
  in Moore same c step

split : Moore a b -> Moore a (b,b)
split = map (\b -> (b,b))

pipe1 : Moore a (b,c) -> Moore b b2 -> Moore a (b2,c)
pipe1 (Moore same1 (b,c) k1) (Moore same2 b2 k2) =
  let step a = k1 a `pipe1` k2 b
      same a = same1 a && same2 b
  in Moore same (b2,c) step

pipe2 : Moore a (b,c) -> Moore c c2 -> Moore a (b,c2)
pipe2 i c =
  let swap (a,b) = (b,a)
  in map swap (pipe1 (map swap i) c)

loop : Moore (a,c) (b,c) -> Moore a b
loop (Moore s (b,c) k) =
  let same a = s (a,c)
      step a = loop (k (a,c))
  in Moore same b step

foldResult : (a -> r) -> (b -> r) -> Result a b -> r
foldResult f1 f2 e = case e of
  Err a -> f1 a
  Ok b -> f2 b

either : Moore a (Result x y) -> Moore x b -> Moore y b -> Moore a b
either (Moore samei xy ki) left right =
  let same a = samei a && foldResult (steady left) (steady right) xy
      st a = case xy of
        Err x -> either (ki a) (step left x) right
        Ok y -> either (ki a) left (step right y)
      o = case xy of
        Err x -> extract left
        Ok y -> extract right
  in Moore same o st

focus : (a -> Maybe b) -> Moore b c -> Moore a c
focus f ((Moore same o k) as m) =
  let same' a = case f a of
        Nothing -> True
        Just _ -> True
      k' a = case f a of
        Nothing -> focus f m
        Just b -> focus f (k b)
  in Moore same' o k'

{-| Run the first argument until it emits `Err s`, then switch permanently to `f s`. -}
bind : Moore a (Result s b) -> (s -> Moore a b) -> Moore a b
bind (Moore same sb k) f = case sb of
  Err s -> f s
  Ok b -> Moore same b (\a -> bind (k a) f)
