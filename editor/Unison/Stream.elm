module Unison.Stream where

data Stream a = Empty | Cons (() -> a) (() -> Stream a)

fromList : [a] -> Stream a
fromList a = case a of
  [] -> Empty
  h :: t -> Cons (\_ -> h) (\_ -> fromList t)

unfold : (s -> (a,s)) -> s -> Stream a
unfold f s =
  let (h,s') = f s
  in Cons (\_ -> h) (\_ -> unfold f s')

uncons : Stream a -> Maybe (a, () -> Stream a)
uncons s = case s of
  Empty -> Nothing
  Cons h t -> Just (h (), t)

take : Int -> Stream a -> Stream a
take n s =
  if n <= 0 then Empty
  else case s of
    Empty -> Empty
    Cons h t -> Cons h (\_ -> take (n-1) (t ()))

maybeHead : Stream a -> Maybe a
maybeHead s = case s of
  Empty -> Nothing
  Cons h _ -> Just (h ())

append : Stream a -> Stream a -> Stream a
append l r = case l of
  Empty -> r
  Cons h t -> Cons h (\_ -> t () `append` r)

