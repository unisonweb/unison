module Unison.Path where

import Array (Array)
import Array
import Unison.Parser (Parser)
import Unison.Parser as P
import Unison.Jsonify as J
import Unison.Jsonify (Jsonify)

data E
  = Fn -- ^ Points at function in a function application
  | Arg -- ^ Points at the argument of a function application
  | Body -- ^ Points at the body of a lambda
  | Index Int -- ^ Points into a `Vector` literal

type Path = Array E

parseE : Parser E
parseE = P.union' <| \t ->
  if | t == "Fn" -> P.unit Fn
     | t == "Arg" -> P.unit Arg
     | t == "Body" -> P.unit Body
     | t == "Index" -> P.map Index P.int

jsonifyE : Jsonify E
jsonifyE e = case e of
  Fn -> J.tag' "Fn" J.emptyArray ()
  Arg -> J.tag' "Arg" J.emptyArray ()
  Body -> J.tag' "Body" J.emptyArray ()
  Index i -> J.tag' "Index" J.int i

parsePath : Parser Path
parsePath = P.map Array.fromList (P.array parseE)

jsonifyPath : Jsonify Path
jsonifyPath = J.contramap Array.toList (J.array jsonifyE)
