module Unison.Metadata where

import Array
import Dict as M
import Elmz.Json.Encoder as Encoder
import Elmz.Json.Encoder (Encoder)
import Elmz.Json.Decoder as Decoder
import Elmz.Json.Decoder (Decoder, (#))
import List
import Maybe
import Unison.Reference as R
import Unison.Path as Path
import Unison.Path (Path)
import Unison.Hash as H
import Unison.Var (I)
import Unison.Var as V
type alias E = Path.E
type alias Path = Path.Path -- to avoid conflict with Graphics.Collage.Path

type Sort = Type | Term

type alias Metadata = {
  sort : Sort,
  names : Names,
  -- for each var, and each scope (which points to a lambda body), what are the names of that var w/in that scope
  locals : M.Dict I (List (Path,Names)),
  description : Maybe R.Reference,
  annotation : R.Reference
}

anonymousSymbol : Symbol
anonymousSymbol = Symbol "anonymousSymbol" Prefix 9

anonymousTerm : Metadata
anonymousTerm = Metadata Term [] M.empty Nothing (R.Builtin "unknown type")

firstSymbol : String -> Metadata -> Symbol
firstSymbol defaultName md = case md.names of
  [] -> { name = defaultName, fixity = Prefix, precedence = 9 }
  h :: _ -> h

firstName : String -> Metadata -> String
firstName ifEmpty md =
  if List.isEmpty md.names
  then ifEmpty
  else (List.head md.names).name

resolveLocal : Metadata -> Path -> I -> Symbol
resolveLocal md p v =
  let ns = localNames md p v
  in if List.isEmpty ns then { name = "v"++toString v, fixity = Prefix, precedence = 9 }
     else List.head ns

localNames : Metadata -> Path -> I -> Names
localNames env p v =
  let trimmed = Path.trimToScope p
  in case M.get v env.locals of
    Nothing -> []
    Just psns -> let go (p,ns) acc = case acc of
                    Nothing -> if p == trimmed then Just ns else Nothing
                    Just acc -> Just acc
                 in Maybe.withDefault [] (List.foldl go Nothing psns)

type Fixity = InfixL | InfixR | Infix | Prefix

type alias Symbol = { name : String, fixity : Fixity, precedence : Int }

type alias Names = List Symbol

type Query = Query String

decodeFixity : Decoder Fixity
decodeFixity = Decoder.andThen Decoder.string <| \t ->
  if | t == "InfixL" -> Decoder.unit InfixL
     | t == "InfixR" -> Decoder.unit InfixR
     | t == "Infix"  -> Decoder.unit Infix
     | t == "Prefix" -> Decoder.unit Prefix
     | otherwise -> Decoder.fail ("expected {InfixL, InfixR, Infix, Prefix}, got : " ++ t)

encodeFixity : Encoder Fixity
encodeFixity f = case f of
  InfixL -> Encoder.string "InfixL"
  InfixR -> Encoder.string "InfixR"
  Infix -> Encoder.string "Infix"
  Prefix -> Encoder.string "Prefix"

decodeSymbol : Decoder Symbol
decodeSymbol =
  let symbol n f p = { name = n, fixity = f, precedence = p }
  in Decoder.newtyped' identity <| Decoder.product3 symbol Decoder.string decodeFixity Decoder.int

encodeSymbol : Encoder Symbol
encodeSymbol s =
  Encoder.tag' "Symbol" (Encoder.tuple3 Encoder.string encodeFixity Encoder.int) (s.name, s.fixity, s.precedence)

decodeSort : Decoder Sort
decodeSort = Decoder.andThen Decoder.string <| \t ->
  if | t == "Type" -> Decoder.unit Type
     | t == "Term" -> Decoder.unit Term
     | otherwise -> Decoder.fail ("expected {Type, Term}, got : " ++ t)

encodeSort : Encoder Sort
encodeSort s = case s of
  Type -> Encoder.string "Type"
  Term -> Encoder.string "Term"

decodeQuery : Decoder Query
decodeQuery = Decoder.newtyped' Query Decoder.string

encodeQuery : Encoder Query
encodeQuery (Query q) = Encoder.tag' "Query" Encoder.string q

decodeNames : Decoder Names
decodeNames = Decoder.newtyped' identity (Decoder.list decodeSymbol)

encodeNames : Encoder Names
encodeNames = Encoder.tag' "Names" (Encoder.list encodeSymbol)

decodeMetadata : Decoder Metadata
decodeMetadata =
  Decoder.newtyped' identity <| Decoder.product5
    Metadata
    decodeSort
    decodeNames
    decodeLocals
    (Decoder.maybe R.decode)
    R.decode

decodeLocals : Decoder (M.Dict I (List (Path,Names)))
decodeLocals =
  Decoder.map M.fromList (Decoder.list (Decoder.tuple2 V.decode (Decoder.list (Decoder.tuple2 Path.decodePath decodeNames))))

encodeLocals : Encoder (M.Dict I (List (Path,Names)))
encodeLocals m = Encoder.list (Encoder.tuple2 V.encode (Encoder.list (Encoder.tuple2 Path.encodePath encodeNames))) (M.toList m)

encodeMetadata : Encoder Metadata
encodeMetadata md = Encoder.tag' "Metadata"
  (Encoder.tuple5
    encodeSort
    encodeNames
    encodeLocals
    (Encoder.optional R.encode)
    R.encode)
  (md.sort, md.names, md.locals, md.description, md.annotation)

