module Unison.Type where

import Unison.Hash as H
import Unison.Parser as P
import Unison.Parser (Parser)
import Unison.Var (I)

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
  | Kind Type Kind
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

parseI : Parser I
parseI = P.int 

parseType : Parser Type
parseType = P.union' <| \t -> 
  if | t == "Unit" -> P.map Unit parseLiteral
     | t == "Arrow" -> P.lift2 Arrow parseType parseType  
     | t == "Universal" -> P.map Universal parseI 
     | t == "Existential" -> P.map Existential parseI 
     | t == "Kind" -> P.lift2 Kind parseType parseKind 
     | t == "Constrain" -> P.lift2 Constrain parseType (P.unit ())
     | t == "Forall" -> P.lift2 Forall parseI parseType
