module Unison.Trie where

import List

data Trie k v = Trie (Maybe v) [(k, Trie k v)]

unit : v -> Trie k v
unit v = Trie (Just v) []

map : (a -> b) -> Trie k a -> Trie k b
map f (Trie here subs) =
  let here' = maybe Nothing (Just . f) here
  in Trie here' (List.map (\(k,t) -> (k, map f t)) subs)

modify : [k] -> (Maybe v -> v) -> Trie k v -> Trie k v
modify k e (Trie here subs) = case k of
  [] -> Trie (Just (e here)) subs
  k :: kt ->
    let f (ksub,t) = if ksub == k then (ksub, modify kt e t) else (ksub, t)
        subs' = List.map f subs
    in Trie here subs'

-- Only replace value if it did not already exist
insert : [k] -> v -> Trie k v -> Trie k v
insert k v = modify k (maybe v id)

-- Replace the value regardless
reinsert : [k] -> v -> Trie k v -> Trie k v
reinsert k v = modify k (\_ -> v)

isEmpty : Trie k v -> Bool
isEmpty (Trie here subs) = isNothing here && List.isEmpty subs

mergeBy : (k -> comparable) -> Trie k v -> Trie k v -> Trie k v
mergeBy f (Trie here1 subs1) (Trie here2 subs2) =
  let here = maybe here2 Just here1
      merge subs1 subs2 = case (subs1, subs2) of
        ([], subs2) -> subs2
        (subs1, []) -> subs1
        ((k1,sub1) :: t1, (k2,sub2) :: t2) ->
          let (fk1, fk2) = (f k1, f k2)
          in if | fk1 < fk2 -> (k1,sub1) :: merge t1 ((k2,sub2) :: t2)
                | fk1 > fk2 -> (k2,sub2) :: merge ((k1,sub1) :: t1) t2
                | otherwise -> (k1,mergeBy f sub1 sub2) :: merge t1 t2
  in Trie here (merge (subs1 |> sortBy (f . fst)) (subs2 |> sortBy (f . fst)))

lookup : [k] -> Trie k v -> Maybe v
lookup k (Trie here subs) = case k of
  [] -> here
  k :: kt -> case List.filter (\(k',t) -> k' == k) subs of
    [] -> Nothing
    (_,t) :: _ -> lookup kt t

