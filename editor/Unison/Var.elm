module Unison.Var where

import Unison.Jsonify as J
import Unison.Parser as P

type I = Int

z : I
z = 0

succ : I -> I
succ i = i + 1

parse : P.Parser I
parse = P.int

jsonify : J.Jsonify I
jsonify i = J.number (toFloat i)
