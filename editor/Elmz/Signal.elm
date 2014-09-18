module Elmz.Signal where

{-| Delay the input `Signal` by one unit. -}
delay : a -> Signal a -> Signal a
delay h s =
  let go a {prev,cur} = { cur = prev, prev = a }
  in foldp go { prev = h, cur = h } s
  |> lift .cur

{-| True if the present value differs from the previous value
    according to `f`. -}
changedBy : (a -> a -> Bool) -> Signal a -> Signal Bool
changedBy f s =
  let go a b = case a of
        Nothing -> True
        Just a -> f a b
  in go <~ delay Nothing (lift Just s) ~ s

{-| True if the present value differs from the previous value
    according to `/=`. -}
changed : Signal a -> Signal Bool
changed = changedBy (/=)

{-| Accumulates using `foldp` during regions where `cond` is `True`,
    otherwise emits `z`. -}
foldpWhen : Signal Bool -> (a -> b -> b) -> b -> Signal a -> Signal b
foldpWhen cond f z a =
  let go (cond,a) b = if cond then f a b else z
  in foldp go z ((,) <~ cond ~ a)

{-| Accumulates into a list using `foldp` during regions where `cond`
    is `True`, otherwise emits the empty list. -}
accumulateWhen : Signal Bool -> Signal a -> Signal [a]
accumulateWhen cond a = foldpWhen cond (::) [] a |> lift reverse
