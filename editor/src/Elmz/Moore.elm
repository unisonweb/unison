module Elmz.Moore where

import Signal
import Signal ((<~), (~), foldp, Signal)
import List ((::))
import Maybe

type Moore i o = Moore o (i -> Maybe (Moore i o))

ap : Moore i (a -> b) -> Moore i a -> Moore i b
ap = map2 (<|)

changesBy : (a -> a -> Maybe b) -> Moore a (Maybe b)
changesBy f =
  let prev = contramap Just (emit Nothing echo')
      cur = contramap Just echo'
      g prev cur = case (prev,cur) of
        (Just a, Just a2) -> f a a2
        _ -> Nothing
  in map2 g prev cur

contramap : (i0 -> i) -> Moore i o -> Moore i0 o
contramap f (Moore o k) = Moore o (\i -> Maybe.map (contramap f) (k (f i)))

duplicate : Moore i o -> Moore i (Moore i o)
duplicate m = Moore m (\i -> Maybe.map duplicate (step m i))

extract : Moore i o -> o
extract (Moore o _) = o

feed : Moore i o -> i -> Moore i o
feed ((Moore _ k) as m) i = Maybe.withDefault m (k i)

focus : (a -> Maybe b) -> Moore b c -> Moore a c
focus f ((Moore o k) as m) =
  let k' a = f a `Maybe.andThen` \b -> Maybe.map (focus f) (k b)
  in Moore o k'

loop : Moore (a,c) (b,c) -> Moore a b
loop (Moore (b,c) k) =
  let step a = Maybe.map loop (k (a,c))
  in Moore b step

map2 : (o1 -> o2 -> o3) -> Moore i o1 -> Moore i o2 -> Moore i o3
map2 f ((Moore o1 k1) as m1) ((Moore o2 k2) as m2) =
  Moore (f o1 o2)
        (\i -> case (k1 i, k2 i) of
          (Nothing, Nothing) -> Nothing
          (m1', m2') -> Just <|
            map2 f (Maybe.withDefault m1 m1')
                   (Maybe.withDefault m2 m2'))

moore : o -> (i -> Moore i o) -> Moore i o
moore o k = Moore o (k >> Just)

map : (o -> o2) -> Moore i o -> Moore i o2
map f (Moore o k) = Moore (f o) (\i -> Maybe.map (map f) (k i))

echo : o -> Moore o o
echo o = moore o echo

echo' : Moore (Maybe a) (Maybe a)
echo' = echo Nothing

emit : o -> Moore i o -> Moore i o
emit oz (Moore o k) = Moore oz (\i -> Maybe.map (emit o) (k i))

pipe : Moore a b -> Moore b c -> Moore a c
pipe (Moore b k1) (Moore c k2) =
  let step a = k1 a `Maybe.andThen` \m1 -> Maybe.map (pipe m1) (k2 b)
  in Moore c step

pipe1 : Moore a (b,c) -> Moore b b2 -> Moore a (b2,c)
pipe1 (Moore (b,c) k1) (Moore b2 k2) =
  let step a = k1 a `Maybe.andThen` \m1 -> Maybe.map (pipe1 m1) (k2 b)
  in Moore (b2,c) step

pipe2 : Moore a (b,c) -> Moore c c2 -> Moore a (b,c2)
pipe2 i c =
  let swap (a,b) = (b,a)
  in map swap (pipe1 (map swap i) c)

split : Moore a b -> Moore a (b,b)
split = map (\b -> (b,b))

step : Moore i o -> i -> Maybe (Moore i o)
step (Moore _ k) = k

unit : o -> Moore i o
unit o = Moore o (always Nothing)

{-
withInput : i -> Moore i o -> Moore i (i,o)
withInput i0 m = map2 (,) (echo i0) m

dropRepeats : o -> Moore o o
dropRepeats prev = Moore ((==) prev) prev dropRepeats

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

{-| Run the first argument until it emits `Err s`, then switch permanently to `f s`. -}
bind : Moore a (Result s b) -> (s -> Moore a b) -> Moore a b
bind (Moore same sb k) f = case sb of
  Err s -> f s
  Ok b -> Moore same b (\a -> bind (k a) f)

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

-}
