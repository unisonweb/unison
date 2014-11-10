module Elmz.Pattern where

import Maybe

type Pattern ctx focus = ctx -> Maybe focus

infixl 4 **

(**) : Pattern a b -> Pattern b c -> Pattern a c
(**) p1 p2 a = case p1 a of
  Nothing -> Nothing
  Just b -> p2 b

empty : Pattern a b
empty a = Nothing

or : Pattern a b -> Pattern a b -> Pattern a b
or p1 p2 a = case p1 a of
  Nothing -> p2 a
  Just a -> Just a
