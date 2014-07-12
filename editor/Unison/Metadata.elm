module Unison.Metadata where

import Dict as M
import Unison.Jsonify as J
import Unison.Jsonify (Jsonify)
import Unison.Parser as P
import Unison.Parser (Parser, (#))
import Unison.Hash as H
import Unison.Var (I)
import Unison.Var as V

data Sort = Type | Term

data Metadata = Metadata {
  sort : Sort,
  names : Names,
  locals : M.Dict I Names,
  description : Maybe H.Hash,
  annotation : H.Hash
}

data Names = Names [String]

data Query = Query String

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
parseNames = P.newtyped' Names (P.array P.string)

jsonifyNames : Jsonify Names
jsonifyNames (Names ns) = J.tag' "Names" (J.array J.string) ns

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

parseLocals : Parser (M.Dict I Names)
parseLocals = P.map M.fromList (P.array (P.tuple2 V.parse parseNames))

jsonifyLocals : Jsonify (M.Dict I Names)
jsonifyLocals m = J.array (J.tuple2 V.jsonify jsonifyNames) (M.toList m)

jsonifyMetadata : Jsonify Metadata
jsonifyMetadata (Metadata md) = J.tag' "Metadata"
  (J.tuple5
    jsonifySort
    jsonifyNames
    jsonifyLocals
    (J.optional H.jsonify)
    H.jsonify)
  (md.sort, md.names, md.locals, md.description, md.annotation)

