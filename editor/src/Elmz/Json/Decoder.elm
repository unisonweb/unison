module Elmz.Json.Decoder where

import Dict as M
import Json.Decode as J
import Json.Decode (Decoder, andThen)
import List
import Set
import Set (Set)
import String as S

type alias Decoder a = J.Decoder a

int = J.int
string = J.string
float = J.float
bool = J.bool
list = J.list
array = J.array
map = J.map
map2 = J.object2
map3 = J.object3
map4 = J.object4
map5 = J.object5
at = J.at
fail = J.fail
unit = J.succeed
andThen = J.andThen
product2 = J.tuple2
product3 = J.tuple3
product4 = J.tuple4
product5 = J.tuple5
product6 = J.tuple6
tuple2 = J.tuple2 (,)
tuple3 = J.tuple3 (,,)
tuple4 = J.tuple4 (,,,)
tuple5 = J.tuple5 (,,,,)
maybe = J.maybe
decodeString = J.decodeString
object = J.dict

lazy : (() -> Decoder a) -> Decoder a
lazy a = J.succeed () `J.andThen` \_ -> a ()

apply : Decoder (a -> b) -> Decoder a -> Decoder b
apply f a = J.object2 (<|) f a

infixl 4 #
(#) : Decoder (a -> b) -> Decoder a -> Decoder b
f # a = apply f a

set : Decoder comparable -> Decoder (Set comparable)
set p = J.map Set.fromList (J.list p)

union : String -> String -> (String -> Decoder a) -> Decoder a
union tag contents f =
  J.at [tag] J.string `andThen` \t -> J.at [contents] (f t)

union' : (String -> Decoder a) -> Decoder a
union' = union "tag" "contents"

-- Extracts a tag from the 0th index of the input, assumed to be an array,
-- and uses this to pick a decoder to run on the 1st index of the input.
arrayUnion : (String -> Decoder a) -> Decoder a
arrayUnion f =
  product2 always string (unit ()) `andThen` \t ->
  product2 (\_ a -> a) (unit ()) (f t)

-- Like `arrayUnion`, but for just a single decoder. The tag must match `expected`.
arrayNewtyped : String -> Decoder a -> Decoder a
arrayNewtyped expected d =
  let ok t = if t == expected then d else fail ("expected " ++ expected ++ " got :" ++ t)
  in arrayUnion ok

newtyped' : (a -> b) -> Decoder a -> Decoder b
newtyped' f p = union' (\_ -> J.map f p)

safeIndex : Int -> List a -> Maybe a
safeIndex i xs = case List.drop i xs of
  [] -> Nothing
  h :: t -> Just h
