module Unison.Hash where

import Unison.Parser as P
import Unison.Jsonify as J

type Hash = String

base64 : Hash -> String
base64 h = h

parse : P.Parser Hash
parse = P.string

jsonify : J.Jsonify Hash
jsonify = J.string
