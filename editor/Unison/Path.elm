module Unison.Path where

import Array (Array)
import Array as A
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

push : Path -> E -> Path
push p e = A.push e p

append : Path -> [E] -> Path
append p es = A.append p (A.fromList es)

-- Trim from the right of this path until hitting a `Body` path element.
-- This is used to normalize paths
trimToScope : Path -> Path
trimToScope p =
  if | A.length p == 0 -> p
     | A.getOrFail (A.length p - 1) p == Body -> p
     | otherwise -> trimToScope (A.slice 0 -1 p)

startsWith : Array a -> Array a -> Bool
startsWith prefix overall =
  A.length prefix == 0 ||
  A.length prefix <= A.length overall &&
  A.get 0 prefix == A.get 0 overall &&
  startsWith (A.slice 1 (A.length prefix) prefix)
             (A.slice 1 (A.length overall) overall)

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
parsePath = P.map A.fromList (P.array parseE)

jsonifyPath : Jsonify Path
jsonifyPath = J.contramap A.toList (J.array jsonifyE)
