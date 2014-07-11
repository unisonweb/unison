module Unison.Term where

import Unison.Hash (Hash)
import Unison.Hash as H
import Unison.Parser as P
import Unison.Parser (Parser)
import Unison.Var (I)
import Unison.Var as V
import Unison.Type as T

data Literal
  = N Float
  | S String
  | V [Term]

data Term
  = Var I
  | Lit Literal
  | Con Hash
  | Ref Hash
  | App Term Term
  | Type Term T.Type
  | Lam I Term

parseLiteral : Parser Literal
parseLiteral = P.union' <| \t ->
  if | t == "Number" -> P.map N P.number
     | t == "String" -> P.map S P.string
     | t == "Vector" -> P.map V (P.array parseTerm)

parseTerm : Parser Term
parseTerm = P.union' <| \t ->
  if | t == "Var" -> P.map Var V.parse
     | t == "Lit" -> P.map Lit parseLiteral
     | t == "Con" -> P.map Con H.parse
     | t == "Ref" -> P.map Ref H.parse
     | t == "App" -> P.lift2 App parseTerm parseTerm
     | t == "Ann" -> P.lift2 Type parseTerm T.parseType
     | t == "Lam" -> P.lift2 Lam V.parse parseTerm
