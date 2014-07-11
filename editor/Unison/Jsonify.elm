module Unison.Jsonify where

import Json as J

type Jsonify a = a -> J.Value

unit : J.Value -> Jsonify a
unit v _ = v

array : Jsonifier a -> Jsonify [a]
array f vs = J.Array (map f v)
