module Elmz.Trie where

import List
import List ((::))

type Trie k v = Trie (Maybe v) (List (k, Trie k v))

empty : Trie k v
empty = Trie Nothing []

insert : List k -> v -> Trie k v -> Trie k v
insert k v (Trie v0 children) = case k of
  [] -> Trie (Just v) children
  h :: t -> let (yes,no) = List.partition (\(k2,_) -> k2 == h) children
            in case yes of
                 [] -> Trie v0 ((h, insert t v empty) :: no)
                 (_,child) :: _ -> Trie v0 ((h, insert t v child) :: no)

lookup : List k -> Trie k v -> Maybe v
lookup k (Trie v children) = case k of
  [] -> v
  h :: t -> let f (k,v) = if k == h then Just v else Nothing
            in case List.filterMap f children of
                 child :: _ -> lookup t child
                 []     -> Nothing

delete : List k -> Trie k v -> Trie k v
delete k (Trie v children) = case k of
  [] -> Trie Nothing children
  h :: t -> let f2 (k,child) = if k == h then (k, delete t child) else (k, child)
            in Trie v (List.map f2 children)

contains : List k -> Trie k v -> Bool
contains k t = case lookup k t of
  Nothing -> False
  Just _ -> True
