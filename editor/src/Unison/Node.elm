-- Interface to the Unison node
module Unison.Node where

import Dict as M
import Maybe
import Elmz.Json.Encoder as Encoder
import Elmz.Json.Encoder (Encoder)
import Elmz.Json.Decoder as Decoder
import Elmz.Json.Decoder (Decoder)
import Http
import Http (Request, Response)
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

jsonGet : Encoder a -> Host -> String -> a -> Request String
jsonGet = jsonRequest "GET"

jsonPost : Encoder a -> Host -> String -> a -> Request String
jsonPost = jsonRequest "POST"

jsonRequest : String -> Encoder a -> Host -> String -> a -> Request String
jsonRequest verb ja host path a =
  Http.request verb (host ++ "/" ++ path) (Encoder.render ja a) []

decodeResponse : Decoder a -> Response String -> Response a
decodeResponse p r = case r of
  Http.Success body ->
    case Decoder.decodeString p body of
      Err err -> Http.Failure 0 err
      Ok a -> Http.Success a
  Http.Waiting -> Http.Waiting
  Http.Failure code body -> Http.Failure code body

admissibleTypeOf : Signal Host -> Signal (Term, Path.Path) -> Signal (Response Type)
admissibleTypeOf host params =
  let body = Encoder.tuple2 E.encodeTerm Path.encodePath
      req host params = jsonGet body host "admissible-type-of" params
  in decodeResponse T.decodeType <~ Http.send (Signal.map2 req host params)

createTerm : Signal Host -> Signal (Term, Metadata) -> Signal (Response Hash)
createTerm host params =
  let body = Encoder.tuple2 E.encodeTerm MD.encodeMetadata
      req host params = jsonPost body host "create-term" params
  in decodeResponse H.decode <~ Http.send (Signal.map2 req host params)

createType : Signal Host -> Signal (Type, Metadata) -> Signal (Response Hash)
createType host params =
  let body = Encoder.tuple2 T.encodeType MD.encodeMetadata
      req host params = jsonPost body host "create-type" params
  in decodeResponse H.decode <~ Http.send (Signal.map2 req host params)

dependencies : Signal Host
            -> Signal (Maybe (S.Set Hash), Hash)
            -> Signal (Response (S.Set Hash))
dependencies host params =
  let body = Encoder.tuple2 (Encoder.optional (Encoder.set H.encode)) H.encode
      req host params = jsonGet body host "dependencies" params
  in decodeResponse (Decoder.set H.decode) <~ Http.send (Signal.map2 req host params)

dependents : Signal Host
          -> Signal (Maybe (S.Set Hash), Hash)
          -> Signal (Response (S.Set Hash))
dependents host params =
  let body = Encoder.tuple2 (Encoder.optional (Encoder.set H.encode)) H.encode
      req host params = jsonGet body host "dependents" params
  in decodeResponse (Decoder.set H.decode) <~ Http.send (Signal.map2 req host params)

editTerm : Signal Host
        -> Signal (Path, Action, Term)
        -> Signal (Response Term)
editTerm host params =
  let body = Encoder.tuple3 Path.encodePath A.encode E.encodeTerm
      req host params = jsonGet body host "edit-term" params
      decode = decodeResponse E.decodeTerm
  in decode <~ Http.send (Signal.map2 req host params)

{-
editType : Signal Host
        -> Signal (Path, Action, Type)
        -> Signal (Response Type)
editTerm host params =
  let body = Encoder.tuple3 H.encode Path.encode A.encode
      req host params = jsonGet body host "edit-type" params
      decode = decodeResponse (Decoder.tuple2 H.decode E.decodeTerm)
  in decode <~ Http.send (Signal.map2 req host params)
-}

metadatas : Signal Host -> Signal (List Hash) -> Signal (Response (M.Dict Hash Metadata))
metadatas host params =
  let req host params = jsonGet (Encoder.list H.encode) host "metadata" params
  in decodeResponse (Decoder.object MD.decodeMetadata) <~ Http.send (Signal.map2 req host params)

search : Signal Host
      -> Signal (Maybe Type, Query)
      -> Signal (Response (List Term))
search host params =
  let body = Encoder.tuple2 (Encoder.optional T.encodeType)
                            MD.encodeQuery
      req host params = jsonGet body host "search" params
      decode = decodeResponse (Decoder.list E.decodeTerm)
  in decode <~ Http.send (Signal.map2 req host params)

searchLocal : Signal Host
           -> Signal (Hash, Path, Maybe Type)
           -> Signal (Response (List Term))
searchLocal host params =
  let body = Encoder.tuple3 H.encode Path.encodePath (Encoder.optional T.encodeType)
      req host params = jsonGet body host "search-local" params
      decode = Decoder.list E.decodeTerm
  in decodeResponse decode <~ Http.send (Signal.map2 req host params)

terms : Signal Host -> Signal (List Hash) -> Signal (Response (M.Dict Hash Term))
terms host params =
  let req host params = jsonGet (Encoder.list H.encode) host "terms" params
  in decodeResponse (Decoder.object E.decodeTerm) <~ Http.send (Signal.map2 req host params)

transitiveDependencies : Signal Host
                      -> Signal (Maybe (S.Set Hash), Hash)
                      -> Signal (Response (S.Set Hash))
transitiveDependencies host params =
  let body = Encoder.tuple2 (Encoder.optional (Encoder.set H.encode)) H.encode
      req host params = jsonGet body host "transitive-dependencies" params
  in decodeResponse (Decoder.set H.decode) <~ Http.send (Signal.map2 req host params)

transitiveDependents : Signal Host
                    -> Signal (Maybe (S.Set Hash), Hash)
                    -> Signal (Response (S.Set Hash))
transitiveDependents host params =
  let body = Encoder.tuple2 (Encoder.optional (Encoder.set H.encode)) H.encode
      req host params = jsonGet body host "transitive-dependents" params
  in decodeResponse (Decoder.set H.decode) <~ Http.send (Signal.map2 req host params)

types : Signal Host -> Signal (List Hash) -> Signal (Response (M.Dict Hash Type))
types host params =
  let body = Encoder.list H.encode
      req host params = jsonGet body host "types" params
  in decodeResponse (Decoder.object T.decodeType) <~ Http.send (Signal.map2 req host params)

typeOf : Signal Host
      -> Signal (Term, Path)
      -> Signal (Response Type)
typeOf host params =
  let body = Encoder.tuple2 E.encodeTerm Path.encodePath
      req host params = jsonGet body host "type-of" params
      decode = decodeResponse T.decodeType
  in decode <~ Http.send (Signal.map2 req host params)

updateMetadata : Signal Host
              -> Signal (Hash, Metadata)
              -> Signal (Response ())
updateMetadata host params =
  let body = Encoder.tuple2 H.encode MD.encodeMetadata
      req host params = jsonPost body host "update-metadata" params
      decode = decodeResponse (Decoder.unit ())
  in decode <~ Http.send (Signal.map2 req host params)

undefined : a
undefined = undefined
