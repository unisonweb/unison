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
import Unison.Hash as H
import Unison.Hash (Hash)
import Unison.Metadata as MD
import Unison.Metadata (Metadata, Query)
import Unison.Term as E
import Unison.Path as Path
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

createTerm : Host -> Request (Term, Metadata) Hash
createTerm host = Request.post host "create-term"
  (Encoder.tuple2 E.encodeTerm MD.encodeMetadata)
  H.decode

createType : Host -> Request (Type, Metadata) Hash
createType host = Request.post host "create-type"
  (Encoder.tuple2 T.encodeType MD.encodeMetadata)
  H.decode

dependencies : Host -> Request (Maybe (S.Set Hash), Hash) (S.Set Hash)
dependencies host = Request.post host "dependencies"
  (Encoder.tuple2 (Encoder.optional (Encoder.set H.encode)) H.encode)
  (Decoder.set H.decode)

dependents : Host -> Request (Maybe (S.Set Hash), Hash) (S.Set Hash)
dependents host = Request.post host "dependents"
  (Encoder.tuple2 (Encoder.optional (Encoder.set H.encode)) H.encode)
  (Decoder.set H.decode)

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

metadatas : Host -> Request (List Hash) (M.Dict Hash Metadata)
metadatas host = Request.post host "metadatas"
  (Encoder.list H.encode)
  (Decoder.object MD.decodeMetadata)

search : Host -> Request (Maybe Type, Query) (List Term)
search host = Request.post host "search"
  (Encoder.tuple2 (Encoder.optional T.encodeType) MD.encodeQuery)
  (Decoder.list E.decodeTerm)

terms : Host -> Request (List Hash) (M.Dict Hash Term)
terms host = Request.post host "terms"
  (Encoder.list H.encode)
  (Decoder.object E.decodeTerm)

transitiveDependencies : Host -> Request (Maybe (S.Set Hash), Hash) (S.Set Hash)
transitiveDependencies host = Request.post host "transitive-dependencies"
  (Encoder.tuple2 (Encoder.optional (Encoder.set H.encode)) H.encode)
  (Decoder.set H.decode)

transitiveDependents : Host -> Request (Maybe (S.Set Hash), Hash) (S.Set Hash)
transitiveDependents host = Request.post host "transitive-dependents"
  (Encoder.tuple2 (Encoder.optional (Encoder.set H.encode)) H.encode)
  (Decoder.set H.decode)

typeOf : Host -> Request (Term, Path) Type
typeOf host = Request.post host "type-of"
  (Encoder.tuple2 E.encodeTerm Path.encodePath)
  T.decodeType

types : Host -> Request (List Hash) (M.Dict Hash Type)
types host = Request.post host "types"
  (Encoder.list H.encode)
  (Decoder.object T.decodeType)

updateMetadata : Host -> Request (Hash, Metadata) ()
updateMetadata host = Request.post host "update-metadata"
  (Encoder.tuple2 H.encode MD.encodeMetadata)
  (Decoder.unit ())
