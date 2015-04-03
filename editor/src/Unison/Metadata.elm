module Unison.Metadata where

import Array
import Dict
import Elmz.Json.Encoder as Encoder
import Elmz.Json.Encoder (Encoder)
import Elmz.Json.Decoder as Decoder
import Elmz.Json.Decoder (Decoder, (#))
import Elmz.Moore (Moore(..))
import Elmz.Moore as Moore
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
  locals : List (Path, Symbol),
  description : Maybe R.Reference
}

type alias Event = List (R.Key, Metadata)

cache : Moore Event (R.Reference -> Metadata)
cache =
  let go acc entries = let acc' = Dict.fromList entries `Dict.union` acc
                       in Just <| Moore acc' (go acc')
  in Moore Dict.empty (go Dict.empty)
     |> Moore.map (\dict r -> Maybe.withDefault (defaultMetadata r) (Dict.get (R.toKey r) dict))

anonymousSymbol : Symbol
anonymousSymbol = Symbol "anonymousSymbol" Prefix 9

prefixSymbol : String -> Symbol
prefixSymbol name = Symbol name Prefix 9

anonymousTerm : Metadata
anonymousTerm = Metadata Term [] [] Nothing

defaultMetadata : R.Reference -> Metadata
defaultMetadata s =
  Metadata Term [Symbol (R.toKey s) Prefix 9] [] Nothing

firstSymbol : String -> Metadata -> Symbol
firstSymbol defaultName md = case md.names of
  [] -> { name = defaultName, fixity = Prefix, precedence = 9 }
  h :: _ -> h

firstName : String -> Metadata -> String
firstName ifEmpty md =
  if List.isEmpty md.names
  then ifEmpty
  else (List.head md.names).name

localSymbol : Metadata -> Path -> Maybe Symbol
localSymbol env p =
  let trimmed = Path.trimToScope p
  in case List.filter (\(p',sym) -> p == p') env.locals of
    [] -> Nothing
    (_,s) :: _ -> Just s

type Fixity = InfixL | InfixR | Infix | Prefix

type alias Symbol = { name : String, fixity : Fixity, precedence : Int }

type alias Names = List Symbol

type alias Query = String

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
  in Decoder.map3 Symbol
       (Decoder.at ["name"] Decoder.string)
       (Decoder.at ["fixity"] decodeFixity)
       (Decoder.at ["precedence"] Decoder.int)

encodeSymbol : Encoder Symbol
encodeSymbol s =
  Encoder.object3 ("name", Encoder.string)
                  ("fixity", encodeFixity)
                  ("precedence", Encoder.int)
                  (s.name, s.fixity, s.precedence)

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
decodeQuery = Decoder.string

encodeQuery : Encoder Query
encodeQuery = Encoder.string

decodeNames : Decoder Names
decodeNames = Decoder.list decodeSymbol

encodeNames : Encoder Names
encodeNames = Encoder.list encodeSymbol

decodeMetadata : Decoder Metadata
decodeMetadata =
  Decoder.map4 Metadata
    (Decoder.at ["sort"] decodeSort)
    (Decoder.at ["names"] decodeNames)
    (Decoder.at ["locals"] decodeLocals)
    (Decoder.at ["description"] (Decoder.maybe R.decode))

decodeLocals : Decoder (List (Path,Symbol))
decodeLocals =
  Decoder.list (Decoder.tuple2 Path.decodePath decodeSymbol)

encodeLocals : Encoder (List (Path,Symbol))
encodeLocals = Encoder.list (Encoder.tuple2 Path.encodePath encodeSymbol)

encodeMetadata : Encoder Metadata
encodeMetadata md = Encoder.object4
  ("sort", encodeSort)
  ("names", encodeNames)
  ("locals", encodeLocals)
  ("description", (Encoder.optional R.encode))
  (md.sort, md.names, md.locals, md.description)
