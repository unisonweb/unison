module Unison.Path where

import Unison.Parser (Parser)
import Unison.Parser as P
import Unison.Jsonify as J
import Unison.Jsonify (Jsonify)

data E
  = Fn -- ^ Points at function in a function application
  | Arg -- ^ Points at the argument of a function application
  | Body -- ^ Points at the body of a lambda
  | Index Int -- ^ Points into a `Vector` literal

type Path = [E]

type ComparablePath = [Int]

toComparablePath : Path -> ComparablePath
toComparablePath xs =
  let f x = case x of
    Fn -> -3
    Arg -> -2
    Body -> -1
    Index i -> i
  in map f xs

fromComparablePath : ComparablePath -> Path
fromComparablePath xs =
  let f x = if | x == -1 -> Fn
               | x == -2 -> Arg
               | x == -3 -> Body
               | otherwise -> Index x
  in map f xs

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

parseComparablePath : Parser ComparablePath
parseComparablePath = P.map toComparablePath parsePath

jsonifyComparablePath : Jsonify ComparablePath
jsonifyComparablePath = J.array J.int

