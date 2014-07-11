module Unison.Metadata where

import Dict as M
import Unison.Parser as P
import Unison.Parser (Parser, (#), (#|))
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
parseSort = P.union' <| \t ->
  if | t == "Type" -> P.unit Type
     | t == "Term" -> P.unit Term

parseQuery : Parser Query
parseQuery = P.newtyped Query P.string

parseNames : Parser Names
parseNames = P.newtyped Names (P.array P.string)

parseMetadata : Parser Metadata
parseMetadata =
  let md s ns ls d a =
    Metadata { sort = s, names = ns, locals = ls,
               description = d, annotation = a }
  in P.product5
    md
    parseSort
    parseNames
    (P.map M.fromList (P.array (P.tuple2 V.parse parseNames)))
    (P.optional H.parse)
    H.parse
