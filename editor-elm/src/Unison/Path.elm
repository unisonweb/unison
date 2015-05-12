module Unison.Path where

import Array exposing (Array)
import Array as A
import Elmz.Json.Decoder as Decoder
import Elmz.Json.Encoder exposing (Encoder)
import Elmz.Json.Encoder as Encoder
import Json.Decode exposing (Decoder)
import Json.Decode as Decode
import List
import String

type E
  = Fn -- ^ Points at function in a function application
  | Arg -- ^ Points at the argument of a function application
  | Body -- ^ Points at the body of a lambda
  | Index Int -- ^ Points into a `Vector` literal

type alias Path = List E
type alias Key = String

key : Path -> Key
key path =
  let f e = case e of
        Fn -> "Fn"
        Arg -> "Arg"
        Body -> "Body"
        Index i -> toString i
  in String.join "," (List.map f path)

snoc : Path -> E -> Path
snoc p e = p ++ [e]

append : Path -> List E -> Path
append p es = p ++ es

increment : (Path -> Bool) -> Path -> Path
increment valid p =
  let go p = case p of
    Fn :: tl -> List.reverse (Arg :: tl)
    Index i :: tl -> let p' = List.reverse (Index (i+1) :: tl)
                     in if valid p' then p' else go tl
    _ :: tl -> go tl
    [] -> []
  in go (List.reverse p)

decrement : (Path -> Bool) -> Path -> Path
decrement valid p =
  let go p = case p of
    Arg :: tl -> let p1 = List.reverse (Arg :: Fn :: tl)
                     p2 = List.reverse (Fn :: tl)
                 in if valid p1 then p1 else p2
    Index i :: tl -> let p' = List.reverse (Index (i-1) :: tl)
                     in if valid p' then p' else go tl
    _ :: tl -> go tl
    [] -> []
  in go (List.reverse p)

isRightmostArg : Path -> Bool
isRightmostArg p = case List.reverse p of
  Fn :: _ -> False
  Arg :: Fn :: _ -> False
  _ -> True

trimArg : Path -> Path
trimArg p = case List.reverse p of
  Arg :: tl -> List.reverse tl
  _ -> p

lambdaDepth : Path -> Int
lambdaDepth p = case p of
  [] -> 0
  Body :: tl -> 1 + lambdaDepth tl
  _ :: tl -> lambdaDepth tl

-- Trim from the right of this path until hitting a `Body` path element.
-- This is used to normalize paths
trimToScope : Path -> Path
trimToScope p =
  let go p = case p of
        [] -> p
        Body :: t -> List.reverse p
        h :: t -> go t
  in go (List.reverse p)

trimThroughScope : Path -> Path
trimThroughScope p =
  let go p = case p of
        [] -> p
        Body :: t -> List.reverse t
        h :: t -> go t
  in go (List.reverse p)

startsWith : List a -> List a -> Bool
startsWith prefix overall =
  List.length prefix == 0 ||
  List.length prefix <= List.length overall &&
  (List.map2 (,) prefix overall |> List.all (\(a,a2) -> a == a2))

decodeE : Decoder E
decodeE = Decoder.union' <| \t ->
  if | t == "Fn" -> Decode.succeed Fn
     | t == "Arg" -> Decode.succeed Arg
     | t == "Body" -> Decode.succeed Body
     | t == "Index" -> Decode.map Index Decode.int
     | otherwise -> Decode.fail ("unrecognized element tag: " ++ t)

encodeE : Encoder E
encodeE e = case e of
  Fn -> Encoder.tag' "Fn" Encoder.emptyArray ()
  Arg -> Encoder.tag' "Arg" Encoder.emptyArray ()
  Body -> Encoder.tag' "Body" Encoder.emptyArray ()
  Index i -> Encoder.tag' "Index" Encoder.int i

decodePath : Decoder Path
decodePath = Decode.list decodeE

encodePath : Encoder Path
encodePath = Encoder.list encodeE
