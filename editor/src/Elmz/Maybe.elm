module Elmz.Maybe where

import List
import Maybe as M

pure : a -> Maybe a
pure = Just

maybe : r -> (a -> r) -> Maybe a -> r
maybe nothing just a = case a of
  Nothing -> nothing
  Just a -> just a

fromMaybe : a -> Maybe a -> a
fromMaybe a m = case m of
  Nothing -> a
  Just a -> a

ap : Maybe (a -> b) -> Maybe a -> Maybe b
ap f a = case f of
  Just f -> case a of
    Just a -> Just (f a)
    _ -> Nothing
  _ -> Nothing

join : Maybe (Maybe a) -> Maybe a
join a = case a of
  Nothing -> Nothing
  Just a -> a

map2 : (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
map2 f a b = pure f `ap` a `ap` b

map3 : (a -> b -> c -> d) -> Maybe a -> Maybe b -> Maybe c -> Maybe d
map3 f a b c = pure f `ap` a `ap` b `ap` c

map : (a -> b) -> Maybe a -> Maybe b
map f m = case m of
  Nothing -> Nothing
  Just a -> Just (f a)

sequence : List (Maybe a) -> Maybe (List a)
sequence ms =
  let j = List.filterMap identity ms
  in if List.length j == List.length ms then Just j else Nothing

traverse : (a -> Maybe b) -> List a -> Maybe (List b)
traverse f a = sequence (List.map f a)
