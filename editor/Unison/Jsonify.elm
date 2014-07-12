module Unison.Jsonify where

import Json as J
import Dict as M

type Jsonify a = a -> J.Value

render : Jsonify a -> a -> String
render ja a = J.toString "" (ja a)

unit : J.Value -> Jsonify a
unit v _ = v

string : Jsonify String
string = J.String

null : Jsonify a
null _ = J.Null

number : Jsonify Float
number = J.Number

optional : Jsonify a -> Jsonify (Maybe a)
optional ja oa = maybe J.Null ja oa

boolean : Jsonify Bool
boolean = J.Boolean

array : Jsonify a -> Jsonify [a]
array f vs = J.Array (map f vs)

tuple2 : Jsonify a -> Jsonify b -> Jsonify (a,b)
tuple2 a b p = J.Array [a (fst p), b (snd p)]

tuple5 : Jsonify a
      -> Jsonify b
      -> Jsonify c
      -> Jsonify d
      -> Jsonify e
      -> Jsonify (a,b,c,d,e)
tuple5 ja jb jc jd je p = case p of
  (a,b,c,d,e) -> J.Array [ja a, jb b, jc c, jd d, je e]

tag : String -> String -> String -> Jsonify a -> Jsonify a
tag tagKey contentsKey tagValue j a =
  J.Object (M.fromList [(tagKey, J.String tagValue), (contentsKey, j a)])

tag' : String -> Jsonify a -> Jsonify a
tag' = tag "tag" "contents"

product0 : Jsonify ()
product0 _ = J.Array []
