module Unison.Trie where

import List

data Trie k v = Trie (Maybe v) (k -> Maybe (Trie k v))

empty : Trie k v
empty = Trie Nothing (\_ -> Nothing)

unit : v -> Trie k v
unit v = Trie (Just v) (\_ -> Nothing)

trie : (k -> Maybe v) -> Trie k v
trie subs = Trie Nothing (mapMaybe unit . subs)

fromList : [([k], v)] -> Trie k v
fromList kvs = fromList kvs

mapMaybe : (a -> b) -> Maybe a -> Maybe b
mapMaybe f = maybe Nothing (Just . f)

map : (a -> b) -> Trie k a -> Trie k b
map f (Trie here subs) =
  Trie (mapMaybe f here) (\k -> mapMaybe (map f) (subs k))

nest : k -> Trie k v -> Trie k v
nest k t = Trie Nothing (\k' -> if k == k' then Just t else Nothing)

merge : Trie k v -> Trie k v -> Trie k v
merge (Trie here1 subs1) (Trie here2 subs2) =
  let here = maybe here2 Just here1
      subs k = case (subs1 k, subs2 k) of
        (Nothing, o) -> o
        (o, Nothing) -> o
        (Just t1, Just t2) -> Just (merge t1 t2)
  in Trie here subs

lookup : [k] -> Trie k v -> Maybe v
lookup ks t =
  let go k ot = maybe Nothing (\(Trie _ subs) -> subs k) ot
  in case List.foldl go (Just t) ks of
    Just (Trie here _) -> here
    Nothing -> Nothing
