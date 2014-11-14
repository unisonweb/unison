module Unison.Type where

import Elmz.Json.Encoder as Encoder
import Elmz.Json.Encoder (Encoder)
import Elmz.Json.Decoder as Decoder
import Elmz.Json.Decoder (Decoder)
import Unison.Hash as H
import Unison.Var (I)
import Unison.Var as V

data Literal
  = Number
  | String
  | Vector
  | Hash H.Hash

data Type
  = Unit Literal
  | Arrow Type Type
  | Universal I
  | Existential I
  | Ann Type Kind
  | Constrain Type ()
  | Forall I Type

data Kind = Star | KArrow Kind Kind

decodeKind : Decoder Kind
decodeKind = Decoder.union' <| \t ->
  if | t == "Star" -> Decoder.unit Star
     | t == "Arrow" -> Decoder.lift2 KArrow decodeKind decodeKind

decodeLiteral : Decoder Literal
decodeLiteral = Decoder.union' <| \t ->
  if | t == "Number" -> Decoder.unit Number
     | t == "String" -> Decoder.unit String
     | t == "Vector" -> Decoder.unit Vector
     | t == "Hash" -> Decoder.map Hash H.decode

decodeType : Decoder Type
decodeType = Decoder.union' <| \t ->
  if | t == "Unit" -> Decoder.map Unit decodeLiteral
     | t == "Arrow" -> Decoder.lift2 Arrow decodeType decodeType
     | t == "Universal" -> Decoder.map Universal V.decode
     | t == "Existential" -> Decoder.map Existential V.decode
     | t == "Kind" -> Decoder.lift2 Ann decodeType decodeKind
     | t == "Constrain" -> Decoder.lift2 Constrain decodeType (Decoder.unit ())
     | t == "Forall" -> Decoder.lift2 Forall V.decode decodeType

encodeKind : Encoder Kind
encodeKind k = case k of
  Star -> Encoder.tag' "Star" Encoder.product0 ()
  KArrow k k2 -> Encoder.tag' "Arrow" (Encoder.array encodeKind) [k, k2]

encodeLiteral : Encoder Literal
encodeLiteral l = case l of
  Number -> Encoder.tag' "Number" Encoder.product0 ()
  String -> Encoder.tag' "String" Encoder.product0 ()
  Vector -> Encoder.tag' "Vector" Encoder.product0 ()
  Hash h -> Encoder.tag' "Hash" H.encode h

encodeType : Encoder Type
encodeType t = case t of
  Unit l -> Encoder.tag' "Unit" encodeLiteral l
  Arrow i o -> Encoder.tag' "Arrow" (Encoder.array encodeType) [i, o]
  Universal v -> Encoder.tag' "Universal" V.encode v
  Existential v -> Encoder.tag' "Existential" V.encode v
  Ann t k -> Encoder.tag' "Ann" (Encoder.tuple2 encodeType encodeKind) (t,k)
  Constrain t c -> Encoder.tag' "Constrain" (Encoder.tuple2 encodeType Encoder.product0) (t, ())
  Forall n t -> Encoder.tag' "Forall" (Encoder.tuple2 V.encode encodeType) (n, t)
