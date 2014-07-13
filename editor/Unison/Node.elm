-- Interface to the Unison node
module Unison.Node where

import Either (Left, Right, Either)
import Http
import Http (Request, Response)
import Json
import Set as S
import Unison.Hash as H
import Unison.Hash (Hash)
import Unison.Metadata as MD
import Unison.Metadata (Metadata)
import Unison.Term as E
import Unison.Term (Term)
import Unison.Type as T
import Unison.Type (Type)
import Unison.Jsonify as J
import Unison.Jsonify (Jsonify)
import Unison.Path as Path
import Unison.Path (Path)
import Unison.Parser as P
import Unison.Parser (Parser)

type Host = String

-- split a Signal (Response a) into two streams, Signal a
-- which just echos the the previous value
-- and a Signal String of error messages

jsonGet : Jsonify a -> Host -> String -> a -> Request String
jsonGet = jsonRequest "GET"

jsonPost : Jsonify a -> Host -> String -> a -> Request String
jsonPost = jsonRequest "POST"

jsonRequest : String -> Jsonify a -> Host -> String -> a -> Request String
jsonRequest verb ja host path a =
  Http.request verb (host ++ "/" ++ path) (J.render ja a) []

parseResponse : Parser a -> Response String -> Response a
parseResponse p r = case r of
  Http.Success body ->
    let err = Left ("malformed json: " ++ body)
    in case maybe err (P.run p) (Json.fromString body) of
      Left err -> Http.Failure 0 err
      Right a -> Http.Success a
  Http.Waiting -> Http.Waiting
  Http.Failure code body -> Http.Failure code body

admissibleTypeOf : Signal Host -> Signal (Hash, Path) -> Signal (Response Type)
admissibleTypeOf host params =
  let body = J.tuple2 H.jsonify Path.jsonify
      req host params = jsonGet body host "admissible-type-of" params
  in parseResponse T.parseType <~ Http.send (lift2 req host params)

createTerm : Signal Host -> Signal (Term, Metadata) -> Signal (Response Hash)
createTerm host params =
  let body = J.tuple2 E.jsonifyTerm MD.jsonifyMetadata
      req host params = jsonPost body host "create-term" params
  in parseResponse H.parse <~ Http.send (lift2 req host params)

createType : Signal Host -> Signal (Type, Metadata) -> Signal (Response Hash)
createType host params =
  let body = J.tuple2 T.jsonifyType MD.jsonifyMetadata
      req host params = jsonPost body host "create-type" params
  in parseResponse H.parse <~ Http.send (lift2 req host params)

dependencies : Signal Host
            -> Signal (Maybe (S.Set Hash), Hash)
            -> Signal (Response (S.Set Hash))
dependencies host params =
  let body = J.tuple2 (J.optional (J.set H.jsonify)) H.jsonify
      req host params = jsonGet body host "dependencies" params
  in parseResponse (P.set H.parse) <~ Http.send (lift2 req host params)

undefined : a
undefined = undefined
