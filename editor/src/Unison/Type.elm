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
import Unison.Symbol (Symbol)
import Unison.Symbol as Symbol

type Literal
  = Number
  | Text
  | Vector
  | Distance
  | Ref Reference

type Type
  = Lit Literal
  | Arrow Type Type
  | App Type Type
  | Universal Symbol
  | Existential Symbol
  | Ann Type Kind
  | Constrain Type ()
  | Forall Symbol Type

all : Type
all = Forall Symbol.anonymous (Universal Symbol.anonymous)

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
      Lit lit -> case lit of
        Ref r -> Metadata.firstName (Reference.toKey r) (env.metadata r)
        _ -> toString lit
      Universal v -> Symbol.toKey v
      Existential v -> "'" ++ Symbol.toKey v
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

encodeKind : Encoder Kind
encodeKind k = case k of
  Star -> Encoder.tag' "Star" Encoder.product0 ()
  KArrow k k2 -> Encoder.tag' "Arrow" (Encoder.list encodeKind) [k, k2]

decodeLiteral : Decoder Literal
decodeLiteral = Decoder.union' <| \t ->
  if | t == "Number" -> Decoder.unit Number
     | t == "Text" -> Decoder.unit Text
     | t == "Vector" -> Decoder.unit Vector
     | t == "Distance" -> Decoder.unit Distance
     | t == "Ref" -> Decoder.map Ref Reference.decode

encodeLiteral : Encoder Literal
encodeLiteral l = case l of
  Number -> Encoder.tag' "Number" Encoder.product0 ()
  Text -> Encoder.tag' "Text" Encoder.product0 ()
  Vector -> Encoder.tag' "Vector" Encoder.product0 ()
  Distance -> Encoder.tag' "Distance" Encoder.product0 ()
  Ref r -> Encoder.tag' "Ref" Reference.encode r

decodeType : Decoder Type
decodeType =
  Decoder.arrayUnion <| \t ->
  if t /= "Tm" then Decoder.fail ("decodeType.ABT unknown tag: " ++ t)
  else Decoder.union' <| \t ->
    if | t == "Lit" -> Decoder.map Lit decodeLiteral
       | t == "Arrow" -> Decoder.product2 Arrow decodeType decodeType
       | t == "App" -> Decoder.product2 App decodeType decodeType
       | t == "Universal" -> Decoder.arrayNewtyped "Var" (Decoder.map Universal Symbol.decodeSymbol)
       | t == "Existential" -> Decoder.arrayNewtyped "Var" (Decoder.map Existential Symbol.decodeSymbol)
       | t == "Kind" -> Decoder.product2 Ann decodeType decodeKind
       | t == "Constrain" -> Decoder.product2 Constrain decodeType (Decoder.unit ())
       | t == "Forall" -> Decoder.arrayNewtyped "Abs" (Decoder.product2 Forall Symbol.decodeSymbol decodeType)
       | otherwise -> Decoder.fail ("decodeType.F unknown tag: " ++ t)

encodeType : Encoder Type
encodeType t = case t of
  Forall n t ->
    Encoder.tagProduct
      "Tm"
      (Encoder.tag' "Forall" (Encoder.tagProduct "Abs" (Encoder.tuple2 Symbol.encodeSymbol encodeType)))
      (n, t)
  _ -> Encoder.tagProduct "Tm" (\t -> case t of
    Lit l -> Encoder.tag' "Lit" encodeLiteral l
    Arrow i o -> Encoder.tag' "Arrow" (Encoder.list encodeType) [i, o]
    App x y -> Encoder.tag' "App" (Encoder.list encodeType) [x, y]
    Universal v -> Encoder.tag' "Universal" (Encoder.tagProduct "Var" Symbol.encodeSymbol) v
    Existential v -> Encoder.tag' "Existential" (Encoder.tagProduct "Var" Symbol.encodeSymbol) v
    Ann t k -> Encoder.tag' "Ann" (Encoder.tuple2 encodeType encodeKind) (t,k)
    Constrain t c -> Encoder.tag' "Constrain" (Encoder.tuple2 encodeType Encoder.product0) (t, ())
  ) t

