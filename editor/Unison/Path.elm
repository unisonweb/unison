module Unison.Path where

import Array (Array)
import Array as A
import Elmz.Json.Encoder as Encoder
import Elmz.Json.Encoder (Encoder)
import Elmz.Json.Decoder as Decoder
import Elmz.Json.Decoder (Decoder)
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

increment : (Path -> Bool) -> Path -> Path
increment valid p =
  let go p = case p of
    Fn :: tl -> reverse (Arg :: tl)
    Index i :: tl -> let p' = reverse (Index (i+1) :: tl)
                     in if valid p' then p' else go tl
    _ :: tl -> go tl
    [] -> []
  in go (reverse p)

decrement : (Path -> Bool) -> Path -> Path
decrement valid p =
  let go p = case p of
    Arg :: tl -> let p1 = reverse (Arg :: Fn :: tl)
                     p2 = reverse (Fn :: tl)
                 in if valid p1 then p1 else p2
    Index i :: tl -> let p' = reverse (Index (i-1) :: tl)
                     in if valid p' then p' else go tl
    _ :: tl -> go tl
    [] -> []
  in go (reverse p)

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

decodeE : Decoder E
decodeE = Decoder.union' <| \t ->
  if | t == "Fn" -> Decoder.unit Fn
     | t == "Arg" -> Decoder.unit Arg
     | t == "Body" -> Decoder.unit Body
     | t == "Index" -> Decoder.map Index Decoder.int

encodeE : Encoder E
encodeE e = case e of
  Fn -> Encoder.tag' "Fn" Encoder.emptyArray ()
  Arg -> Encoder.tag' "Arg" Encoder.emptyArray ()
  Body -> Encoder.tag' "Body" Encoder.emptyArray ()
  Index i -> Encoder.tag' "Index" Encoder.int i

decodePath : Decoder Path
decodePath = Decoder.array decodeE

encodePath : Encoder Path
encodePath = Encoder.array encodeE
