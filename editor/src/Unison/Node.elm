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

-- split a Signal (Response a) into two streams, Signal a
-- which just echos the the previous value
-- and a Signal String of error messages

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

editTerm : Host -> Request (Path, Action, Term) Term
editTerm host = Request.post host "edit-term"
  (Encoder.tuple3 Path.encodePath A.encode E.encodeTerm)
  E.decodeTerm

editType : Host -> Request (Path, Action, Type) Type
editType host = Request.post host "edit-type"
  (Encoder.tuple3 Path.encodePath A.encode T.encodeType)
  T.decodeType

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

metadatas : Host -> Request (List Reference) (M.Dict Reference.Key Metadata)
metadatas host = Request.post host "metadatas"
  (Encoder.list Reference.encode)
  (Reference.decodeMap MD.decodeMetadata)

search : Host -> Request (Maybe Type, Query) (List Term)
search host = Request.post host "search"
  (Encoder.tuple2 (Encoder.optional T.encodeType) MD.encodeQuery)
  (Decoder.list E.decodeTerm)

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
