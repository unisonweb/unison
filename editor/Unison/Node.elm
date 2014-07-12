-- Interface to the Unison node
module Unison.Node where

import Http
import Http (Request, Response)
import Json
import Either (Left, Right, Either)
import Unison.Hash as H
import Unison.Hash (Hash)
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
jsonGet ja host path a =
  Http.request "GET" (host ++ "/" ++ path) (J.render ja a) []

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
  let jbody = J.tuple2 H.jsonify Path.jsonify
      req host params = jsonGet jbody host "admissible-type-of" params
  in lift (parseResponse T.parseType) (Http.send (lift2 req host params))

undefined : a
undefined = undefined
