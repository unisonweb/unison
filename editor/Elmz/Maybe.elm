module Elmz.Maybe where

import List

pure : a -> Maybe a
pure = Just

ap : Maybe (a -> b) -> Maybe a -> Maybe b
ap f a = case f of
  Just f -> case a of
    Just a -> Just (f a)
    _ -> Nothing
  _ -> Nothing

map : (a -> b) -> Maybe a -> Maybe b
map f m = case m of
  Nothing -> Nothing
  Just a -> Just (f a)

sequence : [Maybe a] -> Maybe [a]
sequence ms =
  let j = justs ms
  in if length j == length ms then Just j else Nothing

traverse : (a -> Maybe b) -> [a] -> Maybe [b]
traverse f a = sequence (List.map f a)
