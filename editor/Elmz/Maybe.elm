module Elmz.Maybe where

import List
import Maybe as M

pure : a -> Maybe a
pure = Just

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
