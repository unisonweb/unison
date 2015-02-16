module Elmz.Json.Request where

import Elmz.Json.Encoder (Encoder)
import Elmz.Json.Encoder as Encoder
import Elmz.Json.Decoder (Decoder)
import Elmz.Json.Decoder as Decoder
import Elmz.Signal as Signals
import Http
import Maybe
import Result
import Signal

type alias Request a b =
  { encoder : a -> Http.Request String
  , decoder : Decoder b }

type alias Host = String
type alias Path = String

post : Host -> Path -> Encoder a -> Decoder b -> Request a b
post host path e d = Request (jsonPost e host path) d

map : (b -> c) -> Request a b -> Request a c
map f r = { r | decoder <- Decoder.map f r.decoder }

to : Request a b -> (b -> c) -> Request a c
to r f = map f r

send : Signal a -> Request a b -> Signal (Maybe (Result String b))
send a r =
  Http.send (Signal.map r.encoder a)
  |> Signal.map (decodeResponse r.decoder)
  |> Signals.events
  |> Signal.map (Maybe.withDefault Nothing)

jsonGet : Encoder a -> Host -> String -> a -> Http.Request String
jsonGet = jsonRequest "GET"

jsonPost : Encoder a -> Host -> String -> a -> Http.Request String
jsonPost = jsonRequest "POST"

jsonRequest : String -> Encoder a -> Host -> String -> a -> Http.Request String
jsonRequest verb ja host path a =
  Http.request verb (host ++ "/" ++ path) (Encoder.render ja a) [("Content-Type", "application/json")]

decodeResponse : Decoder a -> Http.Response String -> Maybe (Result String a)
decodeResponse p r = case r of
  Http.Success body -> Just (Decoder.decodeString p body)
  Http.Waiting -> Nothing
  Http.Failure code body -> Just (Result.Err <| "error " ++ toString code ++ "\n" ++ body)
