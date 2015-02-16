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
import Time

type alias Request a b =
  { encoder : a -> Http.Request String
  , decoder : Decoder b }

type alias Host = String
type alias Path = String

type Status e = Inactive | Waiting | Failed e

post : Host -> Path -> Encoder a -> Decoder b -> Request a b
post host path e d = Request (jsonPost e host path) d

map : (b -> c) -> Request a b -> Request a c
map f r = { r | decoder <- Decoder.map f r.decoder }

to : Request a b -> (b -> c) -> Request a c
to r f = map f r

send : Request a b -> Signal (Maybe a) -> Signal (Result (Status String) b)
send r ma =
  let a = Signals.justs ma |> Signal.map (\(Just a) -> a)
      waitings = Signal.map (always (Result.Err Waiting)) a
      results = Http.send (Signal.map r.encoder a) |> Signal.map (decodeResponse r.decoder)
      inactives = Signal.map (always (Result.Err Inactive)) (Time.delay 0 results)
  in results `Signal.merge` inactives `Signal.merge` waitings

isWaiting : Signal (Result (Status String) a) -> Signal Bool
isWaiting results =
  let f r = case r of
              Result.Err Waiting -> True
              _ -> False
  in Signal.map f results

jsonGet : Encoder a -> Host -> String -> a -> Http.Request String
jsonGet = jsonRequest "GET"

jsonPost : Encoder a -> Host -> String -> a -> Http.Request String
jsonPost = jsonRequest "POST"

jsonRequest : String -> Encoder a -> Host -> String -> a -> Http.Request String
jsonRequest verb ja host path a =
  Http.request verb (host ++ "/" ++ path) (Encoder.render ja a) [("Content-Type", "application/json")]

decodeResponse : Decoder a -> Http.Response String -> Result (Status String) a
decodeResponse p r = case r of
  Http.Success body -> case Decoder.decodeString p body of
    Result.Err e -> Result.Err (Failed e)
  Http.Waiting -> Result.Err Waiting
  Http.Failure code body -> Result.Err <| Failed ("error " ++ toString code ++ "\n" ++ body)
