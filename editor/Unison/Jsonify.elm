module Unison.Jsonify where

import Json as J
import Dict as M

type Jsonify a = a -> J.Value

unit : J.Value -> Jsonify a
unit v _ = v

string : Jsonify String
string = J.String

null : Jsonify a
null _ = J.Null

number : Jsonify Float
number = J.Number

boolean : Jsonify Bool
boolean = J.Boolean

array : Jsonify a -> Jsonify [a]
array f vs = J.Array (map f vs)

product2 : (p -> a) -> (p -> b)
        -> Jsonify a
        -> Jsonify b
        -> Jsonify p
product2 f1 f2 a b r = J.Array [a (f1 r), b (f2 r)]

tag : String -> String -> String -> Jsonify a -> Jsonify a
tag tagKey contentsKey tagValue j a =
  J.Object (M.fromList [(tagKey, J.String tagValue), (contentsKey, j a)])

tag' : String -> Jsonify a -> Jsonify a
tag' = tag "tag" "contents"
