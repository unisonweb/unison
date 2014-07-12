module Unison.Path where

import Unison.Jsonify as J
import Unison.Jsonify (Jsonify)

data E
  = Fn -- ^ Points at function in a function application
  | Arg -- ^ Points at the argument of a function application
  | Body -- ^ Points at the body of a lambda

type Path = [E]

jsonifyE : Jsonify E
jsonifyE e = case e of
  Fn -> J.string "Fn"
  Arg -> J.string "Arg"
  Body -> J.string "Body"

jsonify : Jsonify Path
jsonify = J.array jsonifyE
