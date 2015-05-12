module Unison.Metadata where

import Array
import Dict
import Elmz.Json.Decoder exposing (Decoder, (#))
import Elmz.Json.Decoder as Decoder
import Elmz.Json.Encoder exposing (Encoder)
import Elmz.Json.Encoder as Encoder
import Elmz.Moore exposing (Moore(..))
import Elmz.Moore as Moore
import List
import Maybe
import Unison.Hash as H
import Unison.Path exposing (Path)
import Unison.Path as Path
import Unison.Reference as R
import Unison.Symbol exposing (Symbol)
import Unison.Symbol as Symbol
type alias E = Path.E
type alias Path = Path.Path -- to avoid conflict with Graphics.Collage.Path

type Sort = Type | Term

type alias Metadata =
  { sort : Sort
  , names : Names
  , description : Maybe R.Reference }

type alias Event = List (R.Key, Metadata)

cache : Moore Event (R.Reference -> Metadata)
cache =
  let go acc entries = let acc' = Dict.fromList entries `Dict.union` acc
                       in Just <| Moore acc' (go acc')
  in Moore Dict.empty (go Dict.empty)
     |> Moore.map (\dict r -> Maybe.withDefault (defaultMetadata r) (Dict.get (R.toKey r) dict))

anonymousTerm : Metadata
anonymousTerm = Metadata Term [] Nothing

defaultMetadata : R.Reference -> Metadata
defaultMetadata s =
  Metadata Term [Symbol.prefix (R.toKey s)] Nothing

firstSymbol : String -> Metadata -> Symbol
firstSymbol defaultName md = case md.names of
  [] -> { freshId = 0, name = defaultName, fixity = Symbol.Prefix, precedence = 9 }
  h :: _ -> h

firstName : String -> Metadata -> String
firstName ifEmpty md =
  Maybe.withDefault ifEmpty (Maybe.map .name (List.head md.names))

type alias Names = List Symbol

type alias Query = String

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
decodeNames = Decoder.list Symbol.decodeSymbol

encodeNames : Encoder Names
encodeNames = Encoder.list Symbol.encodeSymbol

decodeMetadata : Decoder Metadata
decodeMetadata =
  Decoder.map3 Metadata
    (Decoder.at ["sort"] decodeSort)
    (Decoder.at ["names"] decodeNames)
    (Decoder.at ["description"] (Decoder.maybe R.decode))

encodeMetadata : Encoder Metadata
encodeMetadata md = Encoder.object3
  ("sort", encodeSort)
  ("names", encodeNames)
  ("description", (Encoder.optional R.encode))
  (md.sort, md.names, md.description)
