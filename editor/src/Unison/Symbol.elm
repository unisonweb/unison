module Unison.Symbol where

import Elmz.Json.Encoder as Encoder
import Elmz.Json.Encoder (Encoder)
import Elmz.Json.Decoder as Decoder
import Elmz.Json.Decoder (Decoder)

type Fixity = InfixL | InfixR | Infix | Prefix

type alias Symbol = { freshId : Int, name : String, fixity : Fixity, precedence : Int }

type alias Key = String

toKey : Symbol -> Key
toKey s = if s.freshId == 0 then s.name
          else s.name ++ toString s.freshId

anonymous : Symbol
anonymous = Symbol 0 "anonymous" Prefix 9

prefix : String -> Symbol
prefix name = Symbol 0 name Prefix 9

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
  in Decoder.map4 Symbol
       (Decoder.at ["freshId"] Decoder.int)
       (Decoder.at ["name"] Decoder.string)
       (Decoder.at ["fixity"] decodeFixity)
       (Decoder.at ["precedence"] Decoder.int)

encodeSymbol : Encoder Symbol
encodeSymbol s =
  Encoder.object4 ("freshId", Encoder.int)
                  ("name", Encoder.string)
                  ("fixity", encodeFixity)
                  ("precedence", Encoder.int)
                  (s.freshId, s.name, s.fixity, s.precedence)
