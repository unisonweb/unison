module Elmz.Signal where

import Elmz.Maybe

{-| Delay the input `Signal` by one unit. -}
delay : a -> Signal a -> Signal a
delay h s =
  let go a {prev,cur} = { cur = prev, prev = a }
  in foldp go { prev = h, cur = h } s
  |> lift .cur

{-| Statefully transform the `a` signal, using `f`. -}
loop : (a -> s -> (b, s))
    -> s
    -> Signal a
    -> Signal (Maybe b)
loop f s a =
  let go a (_,s) = case f a s of (b,s) -> (Just b, s)
  in fst <~ foldp go (Nothing,s) a

{-| Spikes `True` when the present value differs from the previous value using `==`,
    otherwise is `False`. -}
changed : Signal a -> Signal Bool
changed a =
  lift (\_ -> True) (dropRepeats a) `merge` constant False

{-| Spikes `False` when the value of the input signal changes, otherwise is `True`. -}
unchanged : Signal a -> Signal Bool
unchanged a = lift not (changed a)

{-| Accumulates using `foldp` during regions where `cond` is `True`,
    starting with the value `z`, otherwise emits `z`. -}
foldpWhen : Signal Bool -> (a -> b -> b) -> b -> Signal a -> Signal b
foldpWhen cond f z a =
  let go (cond,a) b = if cond then f a b else z
  in foldp go z ((,) <~ cond ~ a)

{-| Like `foldpWhen`, but uses `z` as the starting value during 'live'
    regions, and emits `Nothing` when `cond` is `False`. -}
foldpWhen' : Signal Bool -> (a -> b -> b) -> Signal b -> Signal a -> Signal (Maybe b)
foldpWhen' cond f z a =
  let go (a,z) b = case b of Nothing -> Just (f a z)
                             Just b  -> Just (f a b)
  in foldpWhen cond go Nothing ((,) <~ a ~ z)

{-| Accumulates into a list using `foldp` during regions where `cond`
    is `True`, otherwise emits the empty list. -}
accumulateWhen : Signal Bool -> Signal a -> Signal [a]
accumulateWhen cond a = foldpWhen cond (::) [] a |> lift reverse

choose : Signal Bool -> Signal a -> Signal a -> Signal a
choose cond a a2 =
  let go cond a a2 = if cond then a else a2
  in go <~ cond ~ a ~ a2

fromMaybe : Signal a -> Signal (Maybe a) -> Signal a
fromMaybe = lift2 Elmz.Maybe.fromMaybe
