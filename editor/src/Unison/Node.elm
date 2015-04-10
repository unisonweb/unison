-- Interface to the Unison node
module Unison.Node where

import Dict as M
import Maybe
import Elmz.Json.Encoder as Encoder
import Elmz.Json.Encoder (Encoder)
import Elmz.Json.Decoder as Decoder
import Elmz.Json.Decoder (Decoder)
import Elmz.Json.Request (Request)
import Elmz.Json.Request as Request
import Set as S
import Signal
import Signal ((<~),(~),Signal)
import Unison.Action as A
import Unison.Action (Action)
import Unison.Metadata as MD
import Unison.Metadata (Metadata, Query)
import Unison.Term as E
import Unison.Path as Path
import Unison.Reference as Reference
import Unison.Reference (Reference)
import Unison.Term (Term)
import Unison.Type as T
import Unison.Type (Type)
import Unison.Var as V
type alias Path = Path.Path

type alias Host = String

admissibleTypeOf : Host -> Request (Term, Path.Path) Type
admissibleTypeOf host = Request.post host "admissible-type-of"
  (Encoder.tuple2 E.encodeTerm Path.encodePath)
  T.decodeType

createTerm : Host -> Request (Term, Metadata) Reference
createTerm host = Request.post host "create-term"
  (Encoder.tuple2 E.encodeTerm MD.encodeMetadata)
  Reference.decode

createType : Host -> Request (Type, Metadata) Reference
createType host = Request.post host "create-type"
  (Encoder.tuple2 T.encodeType MD.encodeMetadata)
  Reference.decode

dependencies : Host -> Request (Maybe (List Reference), Reference) (List Reference)
dependencies host = Request.post host "dependencies"
  (Encoder.tuple2 (Encoder.optional (Encoder.list Reference.encode)) Reference.encode)
  (Decoder.list Reference.decode)

dependents : Host -> Request (Maybe (List Reference), Reference) (List Reference)
dependents host = Request.post host "dependents"
  (Encoder.tuple2 (Encoder.optional (Encoder.list Reference.encode)) Reference.encode)
  (Decoder.list Reference.decode)

type alias Replacement = { path : Path, old : Term, new : Term }

editTerm : Host -> Request (Path, Path, Action, Term) Replacement
editTerm host = Request.post host "edit-term"
  (Encoder.tuple4 Path.encodePath Path.encodePath A.encode E.encodeTerm)
  (Decoder.product3 Replacement Path.decodePath E.decodeTerm E.decodeTerm)

editType : Host -> Request (Path, Action, Type) Type
editType host = Request.post host "edit-type"
  (Encoder.tuple3 Path.encodePath A.encode T.encodeType)
  T.decodeType

evaluateTerms : Host -> Request (List (Path, Term)) (List Replacement)
evaluateTerms host = Request.post host "evaluate-terms"
  (Encoder.list (Encoder.tuple2 Path.encodePath E.encodeTerm))
  (Decoder.list (Decoder.product3 Replacement Path.decodePath E.decodeTerm E.decodeTerm))

type alias LocalInfo =
  { current : Type
  , admissible : Type
  , locals : List Term
  , localApplications : List Int
  , wellTypedLocals : List Term }

localInfo : Host -> Request (Term, Path) LocalInfo
localInfo host = Request.post host "local-info"
  (Encoder.tuple2 E.encodeTerm Path.encodePath)
  (Decoder.product5 LocalInfo
    T.decodeType
    T.decodeType
    (Decoder.list E.decodeTerm)
    (Decoder.list Decoder.int)
    (Decoder.list E.decodeTerm))

metadatas : Host -> Request (List Reference) (List (Reference.Key, Metadata))
metadatas host = Request.post host "metadatas"
  (Encoder.list Reference.encode)
  (Reference.decodeAssociationList MD.decodeMetadata)

type alias SearchResults =
  { query : Query
  , references : List (Reference.Key, Metadata)
  , matches : (List Term, Int)
  , illTypedMatches : (List Term, Int)
  , positionsExamined : List Int }

areResultsComplete : SearchResults -> Bool
areResultsComplete results =
  snd results.illTypedMatches == 0 &&
  snd results.matches == 0

search : Host -> Request (Term, Path, Int, Query, Maybe Type) SearchResults
search host = Request.post host "search"
  (Encoder.tuple5 E.encodeTerm
                  Path.encodePath
                  Encoder.int
                  MD.encodeQuery (Encoder.optional T.encodeType))
  (Decoder.map5 SearchResults
    (Decoder.at ["query"] <| MD.decodeQuery)
    (Decoder.at ["references"] <| Reference.decodeAssociationList MD.decodeMetadata)
    (Decoder.at ["matches"] <| Decoder.tuple2 (Decoder.list E.decodeTerm) Decoder.int)
    (Decoder.at ["illTypedMatches"] <| Decoder.tuple2 (Decoder.list E.decodeTerm) Decoder.int)
    (Decoder.at ["positionsExamined"] <| Decoder.list Decoder.int))

terms : Host -> Request (List Reference) (M.Dict Reference.Key Term)
terms host = Request.post host "terms"
  (Encoder.list Reference.encode)
  (Reference.decodeMap E.decodeTerm)

transitiveDependencies : Host -> Request (Maybe (List Reference), Reference) (List Reference)
transitiveDependencies host = Request.post host "transitive-dependencies"
  (Encoder.tuple2 (Encoder.optional (Encoder.list Reference.encode)) Reference.encode)
  (Decoder.list Reference.decode)

transitiveDependents : Host -> Request (Maybe (List Reference), Reference) (List Reference)
transitiveDependents host = Request.post host "transitive-dependents"
  (Encoder.tuple2 (Encoder.optional (Encoder.list Reference.encode)) Reference.encode)
  (Decoder.list Reference.decode)

typeOf : Host -> Request (Term, Path) Type
typeOf host = Request.post host "type-of"
  (Encoder.tuple2 E.encodeTerm Path.encodePath)
  T.decodeType

types : Host -> Request (List Reference) (M.Dict Reference.Key Type)
types host = Request.post host "types"
  (Encoder.list Reference.encode)
  (Reference.decodeMap T.decodeType)

updateMetadata : Host -> Request (Reference, Metadata) ()
updateMetadata host = Request.post host "update-metadata"
  (Encoder.tuple2 Reference.encode MD.encodeMetadata)
  (Decoder.unit ())
