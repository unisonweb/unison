module Elmz.Json.Encoder where

import Maybe (maybe)
import Json as J
import Dict as M
import Set as S

type Encoder a = a -> J.Value

render : Encoder a -> a -> String
render ja a = J.toString "" (ja a)

contramap : (a -> b) -> Encoder b -> Encoder a
contramap f j = j << f

unit : J.Value -> Encoder a
unit v _ = v

string : Encoder String
string = J.String

null : Encoder a
null _ = J.Null

number : Encoder Float
number = J.Number

int : Encoder Int
int n = J.Number (toFloat n)

optional : Encoder a -> Encoder (Maybe a)
optional ja oa = maybe J.Null ja oa

boolean : Encoder Bool
boolean = J.Boolean

array : Encoder a -> Encoder [a]
array f vs = J.Array (map f vs)

emptyArray : Encoder a
emptyArray _ = J.Array []

set : Encoder comparable -> Encoder (S.Set comparable)
set a = array a << S.toList

dict : Encoder comparable -> Encoder v -> Encoder (M.Dict comparable v)
dict k v = array (tuple2 k v) << M.toList

object : Encoder v -> Encoder (M.Dict String v)
object v = J.Object << M.map v

tuple2 : Encoder a -> Encoder b -> Encoder (a,b)
tuple2 a b p = J.Array [a (fst p), b (snd p)]

tuple3 : Encoder a -> Encoder b -> Encoder c -> Encoder (a,b,c)
tuple3 ja jb jc p = case p of
  (a,b,c) -> J.Array [ja a, jb b, jc c]

tuple4 : Encoder a -> Encoder b -> Encoder c -> Encoder d -> Encoder (a,b,c,d)
tuple4 ja jb jc jd p = case p of
  (a,b,c,d) -> J.Array [ja a, jb b, jc c, jd d]

tuple5 : Encoder a
      -> Encoder b
      -> Encoder c
      -> Encoder d
      -> Encoder e
      -> Encoder (a,b,c,d,e)
tuple5 ja jb jc jd je p = case p of
  (a,b,c,d,e) -> J.Array [ja a, jb b, jc c, jd d, je e]

tag : String -> String -> String -> Encoder a -> Encoder a
tag tagKey contentsKey tagValue j a =
  J.Object (M.fromList [(tagKey, J.String tagValue), (contentsKey, j a)])

tag' : String -> Encoder a -> Encoder a
tag' = tag "tag" "contents"

product0 : Encoder ()
product0 _ = J.Array []
