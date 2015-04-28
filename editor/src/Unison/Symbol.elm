module Unison.Symbol where

import Elmz.Json.Encoder as Encoder
import Elmz.Json.Encoder (Encoder)
import Elmz.Json.Decoder as Decoder
import Elmz.Json.Decoder (Decoder)

type Fixity = InfixL | InfixR | Infix | Prefix

type alias Symbol = { name : String, fixity : Fixity, precedence : Int }

anonymous : Symbol
anonymous = Symbol "anonymousSymbol" Prefix 9

prefix : String -> Symbol
prefix name = Symbol name Prefix 9

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
