module Unison.Metadata where

import Array
import Dict as M
import Elmz.Json.Encoder as Encoder
import Elmz.Json.Encoder (Encoder)
import Elmz.Json.Decoder as Decoder
import Elmz.Json.Decoder (Decoder, (#))
import Maybe (maybe)
import Unison.Path as Path
import Unison.Path (Path)
import Unison.Hash as H
import Unison.Var (I)
import Unison.Var as V
type Path = Path.Path -- to avoid conflict with Graphics.Collage.Path

data Sort = Type | Term

type Metadata = {
  sort : Sort,
  names : Names,
  -- for each var, and each scope (which points to a lambda body), what are the names of that var w/in that scope
  locals : M.Dict I [(Path,Names)],
  description : Maybe H.Hash,
  annotation : H.Hash
}

anonymousSymbol : Symbol
anonymousSymbol = Symbol "anonymousSymbol" Prefix 9

anonymousTerm : Metadata
anonymousTerm = Metadata Term [] M.empty Nothing "unknown"

firstSymbol : String -> Metadata -> Symbol
firstSymbol defaultName md = case md.names of
  [] -> { name = defaultName, fixity = Prefix, precedence = 9 }
  h :: _ -> h

firstName : String -> Metadata -> String
firstName ifEmpty md =
  if isEmpty md.names
  then ifEmpty
  else (head md.names).name

resolveLocal : Metadata -> Path -> I -> Symbol
resolveLocal md p v =
  let ns = localNames md p v
  in if isEmpty ns then { name = "v"++show v, fixity = Prefix, precedence = 9 }
     else head ns

localNames : Metadata -> Path -> I -> Names
localNames env p v =
  let trimmed = Path.trimToScope p
  in case M.get v env.locals of
    Nothing -> []
    Just psns -> let go (p,ns) acc = case acc of
                    Nothing -> if p == trimmed then Just ns else Nothing
                    Just acc -> Just acc
                 in maybe [] identity (foldl go Nothing psns)

data Fixity = InfixL | InfixR | Infix | Prefix

type Symbol = { name : String, fixity : Fixity, precedence : Int }

type Names = [Symbol]

data Query = Query String

decodeFixity : Decoder Fixity
decodeFixity = Decoder.bind Decoder.string <| \t ->
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
decodeSort = Decoder.bind Decoder.string <| \t ->
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
decodeNames = Decoder.newtyped' identity (Decoder.array decodeSymbol)

encodeNames : Encoder Names
encodeNames = Encoder.tag' "Names" (Encoder.array encodeSymbol)

decodeMetadata : Decoder Metadata
decodeMetadata =
  Decoder.newtyped' identity <| Decoder.product5
    Metadata
    decodeSort
    decodeNames
    decodeLocals
    (Decoder.optional H.decode)
    H.decode

decodeLocals : Decoder (M.Dict I [(Path,Names)])
decodeLocals =
  Decoder.map M.fromList (Decoder.array (Decoder.tuple2 V.decode (Decoder.array (Decoder.tuple2 Path.decodePath decodeNames))))

encodeLocals : Encoder (M.Dict I [(Path,Names)])
encodeLocals m = Encoder.array (Encoder.tuple2 V.encode (Encoder.array (Encoder.tuple2 Path.encodePath encodeNames))) (M.toList m)

encodeMetadata : Encoder Metadata
encodeMetadata md = Encoder.tag' "Metadata"
  (Encoder.tuple5
    encodeSort
    encodeNames
    encodeLocals
    (Encoder.optional H.encode)
    H.encode)
  (md.sort, md.names, md.locals, md.description, md.annotation)

