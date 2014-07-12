module Unison.Type where

import Unison.Hash as H
import Unison.Parser as P
import Unison.Parser (Parser)
import Unison.Jsonify as J
import Unison.Jsonify (Jsonify)
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

parseKind : Parser Kind
parseKind = P.union' <| \t ->
  if | t == "Star" -> P.unit Star
     | t == "Arrow" -> P.lift2 KArrow parseKind parseKind

parseLiteral : Parser Literal
parseLiteral = P.union' <| \t ->
  if | t == "Number" -> P.unit Number
     | t == "String" -> P.unit String
     | t == "Vector" -> P.unit Vector
     | t == "Hash" -> P.map Hash H.parse

parseType : Parser Type
parseType = P.union' <| \t ->
  if | t == "Unit" -> P.map Unit parseLiteral
     | t == "Arrow" -> P.lift2 Arrow parseType parseType
     | t == "Universal" -> P.map Universal V.parse
     | t == "Existential" -> P.map Existential V.parse
     | t == "Kind" -> P.lift2 Ann parseType parseKind
     | t == "Constrain" -> P.lift2 Constrain parseType (P.unit ())
     | t == "Forall" -> P.lift2 Forall V.parse parseType

jsonifyKind : Jsonify Kind
jsonifyKind k = case k of
  Star -> J.tag' "Star" J.product0 ()
  KArrow k k2 -> J.tag' "Arrow" (J.array jsonifyKind) [k, k2]

jsonifyLiteral : Jsonify Literal
jsonifyLiteral l = case l of
  Number -> J.tag' "Number" J.product0 ()
  String -> J.tag' "String" J.product0 ()
  Vector -> J.tag' "Vector" J.product0 ()
  Hash h -> J.tag' "Hash" H.jsonify h

jsonifyType : Jsonify Type
jsonifyType t = case t of
  Unit l -> J.tag' "Unit" jsonifyLiteral l
  Arrow i o -> J.tag' "Arrow" (J.array jsonifyType) [i, o]
  Universal v -> J.tag' "Universal" V.jsonify v
  Existential v -> J.tag' "Existential" V.jsonify v
  Ann t k -> J.tag' "Ann" (J.tuple2 jsonifyType jsonifyKind) (t,k)
  Constrain t c -> J.tag' "Constrain" (J.tuple2 jsonifyType J.product0) (t, ())
  Forall n t -> J.tag' "Forall" (J.tuple2 V.jsonify jsonifyType) (n, t)
