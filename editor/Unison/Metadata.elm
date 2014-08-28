module Unison.Metadata where

import Array
import Dict as M
import Unison.Jsonify as J
import Unison.Jsonify (Jsonify)
import Unison.Path as Path
import Unison.Path (Path)
import Unison.Parser as P
import Unison.Parser (Parser, (#))
import Unison.Hash as H
import Unison.Var (I)
import Unison.Var as V

data Sort = Type | Term

data Metadata = Metadata {
  sort : Sort,
  names : Names,
  -- for each var, and each scope (which points to a lambda body), what are the names of that var w/in that scope
  locals : M.Dict I [(Path,Names)],
  description : Maybe H.Hash,
  annotation : H.Hash
}

firstName : String -> Metadata -> String
firstName ifEmpty (Metadata md) =
  if isEmpty md.names
  then ifEmpty
  else (head md.names).name

resolveLocal : Metadata -> Path -> I -> Symbol
resolveLocal md p v =
  let ns = localNames md p v
  in if isEmpty ns then { name = show v, fixity = Prefix, precedence = 9 }
     else head ns

localNames : Metadata -> Path -> I -> Names
localNames (Metadata env) p v =
  let trimmed = Path.trimToScope p
  in case M.get v env.locals of
    Nothing -> []
    Just psns -> let go (p,ns) acc = case acc of
                    Nothing -> if p == trimmed then Just ns else Nothing
                    Just acc -> Just acc
                 in maybe [] id (foldl go Nothing psns)

data Fixity = InfixL | InfixR | Infix | Prefix

type Symbol = { name : String, fixity : Fixity, precedence : Int }

type Names = [Symbol]

data Query = Query String

parseFixity : Parser Fixity
parseFixity = P.bind P.string <| \t ->
  if | t == "InfixL" -> P.unit InfixL
     | t == "InfixR" -> P.unit InfixR
     | t == "Infix"  -> P.unit Infix
     | t == "Prefix" -> P.unit Prefix
     | otherwise -> P.fail ("expected {InfixL, InfixR, Infix, Prefix}, got : " ++ t)

jsonifyFixity : Jsonify Fixity
jsonifyFixity f = case f of
  InfixL -> J.string "InfixL"
  InfixR -> J.string "InfixR"
  Infix -> J.string "Infix"
  Prefix -> J.string "Prefix"

parseSymbol : Parser Symbol
parseSymbol =
  let symbol n f p = { name = n, fixity = f, precedence = p }
  in P.newtyped' id <| P.product3 symbol P.string parseFixity P.int

jsonifySymbol : Jsonify Symbol
jsonifySymbol s =
  J.tag' "Symbol" (J.tuple3 J.string jsonifyFixity J.int) (s.name, s.fixity, s.precedence)

parseSort : Parser Sort
parseSort = P.bind P.string <| \t ->
  if | t == "Type" -> P.unit Type
     | t == "Term" -> P.unit Term
     | otherwise -> P.fail ("expected {Type, Term}, got : " ++ t)

jsonifySort : Jsonify Sort
jsonifySort s = case s of
  Type -> J.string "Type"
  Term -> J.string "Term"

parseQuery : Parser Query
parseQuery = P.newtyped' Query P.string

jsonifyQuery : Jsonify Query
jsonifyQuery (Query q) = J.tag' "Query" J.string q

parseNames : Parser Names
parseNames = P.newtyped' id (P.array parseSymbol)

jsonifyNames : Jsonify Names
jsonifyNames = J.tag' "Names" (J.array jsonifySymbol)

parseMetadata : Parser Metadata
parseMetadata =
  let md s ns ls d a =
    { sort = s, names = ns, locals = ls,
      description = d, annotation = a }
  in P.newtyped' Metadata <| P.product5
    md
    parseSort
    parseNames
    parseLocals
    (P.optional H.parse)
    H.parse

parseLocals : Parser (M.Dict I [(Path,Names)])
parseLocals =
  P.map M.fromList (P.array (P.tuple2 V.parse (P.array (P.tuple2 Path.parsePath parseNames))))

jsonifyLocals : Jsonify (M.Dict I [(Path,Names)])
jsonifyLocals m = J.array (J.tuple2 V.jsonify (J.array (J.tuple2 Path.jsonifyPath jsonifyNames))) (M.toList m)

jsonifyMetadata : Jsonify Metadata
jsonifyMetadata (Metadata md) = J.tag' "Metadata"
  (J.tuple5
    jsonifySort
    jsonifyNames
    jsonifyLocals
    (J.optional H.jsonify)
    H.jsonify)
  (md.sort, md.names, md.locals, md.description, md.annotation)

