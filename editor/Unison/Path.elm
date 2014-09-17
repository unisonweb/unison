module Unison.Path where

import Array (Array)
import Array as A
import Unison.Parser (Parser)
import Unison.Parser as P
import Unison.Jsonify as J
import Unison.Jsonify (Jsonify)
import Unison.Stream (Stream)
import Unison.Stream as Stream

data E
  = Fn -- ^ Points at function in a function application
  | Arg -- ^ Points at the argument of a function application
  | Body -- ^ Points at the body of a lambda
  | Index Int -- ^ Points into a `Vector` literal

type Path = [E]

snoc : Path -> E -> Path
snoc p e = p ++ [e]

append : Path -> [E] -> Path
append p es = p ++ es

-- Trim from the right of this path until hitting a `Body` path element.
-- This is used to normalize paths
trimToScope : Path -> Path
trimToScope p =
  let go p = case p of
        [] -> p
        Body :: t -> reverse p
        h :: t -> go t
  in go (reverse p)

startsWith : [a] -> [a] -> Bool
startsWith prefix overall =
  length prefix == 0 ||
  length prefix <= length overall &&
  (zip prefix overall |> all (\(a,a2) -> a == a2))

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
parsePath = P.array parseE

jsonifyPath : Jsonify Path
jsonifyPath = J.array jsonifyE
