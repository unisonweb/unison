module Unison.Type where

import Elmz.Json.Decoder (Decoder)
import Elmz.Json.Decoder as Decoder
import Elmz.Json.Encoder (Encoder)
import Elmz.Json.Encoder as Encoder
import List
import String
import Unison.Metadata (Metadata)
import Unison.Metadata as Metadata
import Unison.Reference (Reference)
import Unison.Reference as Reference
import Unison.Var (I)
import Unison.Var as V

type Literal
  = Number
  | String
  | Vector
  | Distance
  | Ref Reference

type Type
  = Unit Literal
  | Arrow Type Type
  | Universal I
  | Existential I
  | Ann Type Kind
  | Constrain Type ()
  | Forall I Type

all : Type
all = Forall 0 (Universal 0)

type Kind = Star | KArrow Kind Kind

-- todo: beef up to actually get proper type variable names
key : { tl | metadata : Reference -> Metadata }
   -> Type
   -> String
key env cur = case cur of
  Unit lit -> case lit of
    Ref r -> Metadata.firstName (Reference.toKey r) (env.metadata r)
    _ -> toString lit
  Universal v -> "t"++toString v
  Existential v -> "t"++toString v++"'"
  Arrow i o -> case i of
    Arrow _ _ -> "(" ++ key env i ++ ") → " ++ key env o
    _ -> key env i ++ " → " ++ key env o
  Forall v body ->
    let go v = case v of
          Forall v body -> let (vs,inner) = go body in (v :: vs, inner)
          _ -> ([], v)
    in case go cur of
         (vs,body) -> "∀ " ++ String.join " " (List.map (key env << Universal) vs)
                   ++ ". " ++ key env body

isFunction : Type -> Bool
isFunction t = case t of
  Arrow _ _ -> True
  Forall _ body -> isFunction body
  Ann t _ -> isFunction t
  Constrain t _ -> isFunction t
  _ -> False

decodeKind : Decoder Kind
decodeKind = Decoder.union' <| \t ->
  if | t == "Star" -> Decoder.unit Star
     | t == "Arrow" -> Decoder.product2 KArrow decodeKind decodeKind

decodeLiteral : Decoder Literal
decodeLiteral = Decoder.union' <| \t ->
  if | t == "Number" -> Decoder.unit Number
     | t == "String" -> Decoder.unit String
     | t == "Vector" -> Decoder.unit Vector
     | t == "Distance" -> Decoder.unit Distance
     | t == "Ref" -> Decoder.map Ref Reference.decode

decodeType : Decoder Type
decodeType = Decoder.union' <| \t ->
  if | t == "Unit" -> Decoder.map Unit decodeLiteral
     | t == "Arrow" -> Decoder.product2 Arrow decodeType decodeType
     | t == "Universal" -> Decoder.map Universal V.decode
     | t == "Existential" -> Decoder.map Existential V.decode
     | t == "Kind" -> Decoder.product2 Ann decodeType decodeKind
     | t == "Constrain" -> Decoder.product2 Constrain decodeType (Decoder.unit ())
     | t == "Forall" -> Decoder.product2 Forall V.decode decodeType

encodeKind : Encoder Kind
encodeKind k = case k of
  Star -> Encoder.tag' "Star" Encoder.product0 ()
  KArrow k k2 -> Encoder.tag' "Arrow" (Encoder.list encodeKind) [k, k2]

encodeLiteral : Encoder Literal
encodeLiteral l = case l of
  Number -> Encoder.tag' "Number" Encoder.product0 ()
  String -> Encoder.tag' "String" Encoder.product0 ()
  Vector -> Encoder.tag' "Vector" Encoder.product0 ()
  Ref r -> Encoder.tag' "Ref" Reference.encode r

encodeType : Encoder Type
encodeType t = case t of
  Unit l -> Encoder.tag' "Unit" encodeLiteral l
  Arrow i o -> Encoder.tag' "Arrow" (Encoder.list encodeType) [i, o]
  Universal v -> Encoder.tag' "Universal" V.encode v
  Existential v -> Encoder.tag' "Existential" V.encode v
  Ann t k -> Encoder.tag' "Ann" (Encoder.tuple2 encodeType encodeKind) (t,k)
  Constrain t c -> Encoder.tag' "Constrain" (Encoder.tuple2 encodeType Encoder.product0) (t, ())
  Forall n t -> Encoder.tag' "Forall" (Encoder.tuple2 V.encode encodeType) (n, t)
