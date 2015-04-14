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
  | Text
  | Vector
  | Distance
  | Ref Reference

type Type
  = Unit Literal
  | Arrow Type Type
  | App Type Type
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
key env cur =
  let
    paren : Int -> Int -> String -> String
    paren cur ambient s = if cur < ambient then "(" ++ s ++ ")" else s
    go top prec cur = case cur of
      Unit lit -> case lit of
        Ref r -> Metadata.firstName (Reference.toKey r) (env.metadata r)
        _ -> toString lit
      Universal v -> "t"++toString v
      Existential v -> "t"++toString v++"'"
      Arrow i o -> paren 0 prec (go False (prec+1) i ++ " → " ++ go top prec o)
      App x y -> paren 9 prec (go top 9 x ++ " " ++ go top 10 y)
      Forall v body ->
        if top then go True prec body
        else let extract v = case v of -- higher rank, show the quantifier introduction
               Forall v body -> let (vs,inner) = extract body in (v :: vs, inner)
               _ -> ([], v)
             in case extract cur of
               (vs,body) -> "∀ " ++ String.join " " (List.map (go False 0 << Universal) vs)
                         ++ ". " ++ go False prec body
                         |> paren 9 prec
  in go True 0 cur

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
     | t == "Text" -> Decoder.unit Text
     | t == "Vector" -> Decoder.unit Vector
     | t == "Distance" -> Decoder.unit Distance
     | t == "Ref" -> Decoder.map Ref Reference.decode

decodeType : Decoder Type
decodeType = Decoder.union' <| \t ->
  if | t == "Unit" -> Decoder.map Unit decodeLiteral
     | t == "Arrow" -> Decoder.product2 Arrow decodeType decodeType
     | t == "App" -> Decoder.product2 App decodeType decodeType
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
  Text -> Encoder.tag' "Text" Encoder.product0 ()
  Vector -> Encoder.tag' "Vector" Encoder.product0 ()
  Distance -> Encoder.tag' "Distance" Encoder.product0 ()
  Ref r -> Encoder.tag' "Ref" Reference.encode r

encodeType : Encoder Type
encodeType t = case t of
  Unit l -> Encoder.tag' "Unit" encodeLiteral l
  Arrow i o -> Encoder.tag' "Arrow" (Encoder.list encodeType) [i, o]
  App x y -> Encoder.tag' "App" (Encoder.list encodeType) [x, y]
  Universal v -> Encoder.tag' "Universal" V.encode v
  Existential v -> Encoder.tag' "Existential" V.encode v
  Ann t k -> Encoder.tag' "Ann" (Encoder.tuple2 encodeType encodeKind) (t,k)
  Constrain t c -> Encoder.tag' "Constrain" (Encoder.tuple2 encodeType Encoder.product0) (t, ())
  Forall n t -> Encoder.tag' "Forall" (Encoder.tuple2 V.encode encodeType) (n, t)
