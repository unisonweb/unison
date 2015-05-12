module Elmz.Moore where

import Signal
import Signal exposing ((<~), (~), foldp, Signal)
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
duplicate m = Moore m (\i -> Maybe.map duplicate (step i m))

echo : o -> Moore o o
echo o = moore o echo

echo' : Moore (Maybe a) (Maybe a)
echo' = echo Nothing

emit : o -> Moore i o -> Moore i o
emit oz (Moore o k) = Moore oz (\i -> Maybe.map (emit o) (k i))

extract : Moore i o -> o
extract (Moore o _) = o

feed : i -> Moore i o -> Moore i o
feed i ((Moore _ k) as m) = Maybe.withDefault m (k i)

feeds : List i -> Moore i o -> Moore i o
feeds i m = case i of
  [] -> m
  h :: t -> feeds t (feed h m)

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

pipe : Moore a b -> Moore b c -> Moore a c
pipe (Moore b k1) (Moore c k2) =
  let step a = k1 a `Maybe.andThen` \m1 -> Maybe.map (pipe m1) (k2 b)
  in Moore c step

pipe' : Moore a b -> (b -> Moore b c) -> Moore a c
pipe' m1 f = pipe m1 (f (extract m1))

pipe1 : Moore a (b,c) -> Moore b b2 -> Moore a (b2,c)
pipe1 (Moore (b,c) k1) (Moore b2 k2) =
  let step a = k1 a `Maybe.andThen` \m1 -> Maybe.map (pipe1 m1) (k2 b)
  in Moore (b2,c) step

pipe2 : Moore a (b,c) -> Moore c c2 -> Moore a (b,c2)
pipe2 i c =
  let swap (a,b) = (b,a)
  in map swap (pipe1 (map swap i) c)

spike : b -> b -> (a -> Maybe (Moore a b)) -> Moore a b
spike b bquiet k1 =
  let k a = Maybe.oneOf [k1 a, Just (Moore bquiet k1)]
  in Moore b k

split : Moore a b -> Moore a (b,b)
split = map (\b -> (b,b))

step : i -> Moore i o -> Maybe (Moore i o)
step i (Moore _ k) = k i

transform : Moore i o -> Signal i -> Signal o
transform m i =
  let s i m = feed i m
  in extract <~ foldp s m i

unit : o -> Moore i o
unit o = Moore o (always Nothing)
