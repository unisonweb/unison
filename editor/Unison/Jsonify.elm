module Unison.Jsonify where

import Json as J

type Jsonify a = a -> J.Value

unit : J.Value -> Jsonify a
unit v _ = v

array : Jsonifier a -> Jsonify [a]
array f vs = J.Array (map f v)

product2 : (p -> a) -> (p -> b)
        -> Jsonify a
        -> Jsonify b
        -> Jsonify p
product2 f1 f2 a b r = J.Array [a (f1 r), b (f2 r)]
