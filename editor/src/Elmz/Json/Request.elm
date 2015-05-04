module Elmz.Json.Request where

import Elmz.Json.Encoder exposing (Encoder)
import Elmz.Json.Encoder as Encoder
import Elmz.Json.Decoder exposing (Decoder)
import Elmz.Json.Decoder as Decoder
import Elmz.Signal as Signals
import Http
import Maybe
import Result
import Signal
import Task exposing (Task)
import Time

type alias Request a b =
  { encoder : a -> Out String
  , decoder : Decoder b }

type alias Out a = { verb : String, url : String, body : a }
type alias Host = String
type alias Path = String

type Status e = Inactive | Waiting | Failed e

post : Host -> Path -> Encoder a -> Decoder b -> Request a b
post host path e d =
  let out a = Out "POST" (host ++ "/" ++ path) (Encoder.render e a)
  in Request out d

contramap : (a0 -> a) -> Request a b -> Request a0 b
contramap f r = { r | encoder <- r.encoder << f }

map : (b -> c) -> Request a b -> Request a c
map f r = { r | decoder <- Decoder.map f r.decoder }

to : Request a b -> (b -> c) -> Request a c
to r f = map f r

sendPost : Request a b -> a -> Task Http.Error b
sendPost r a =
  let out = r.encoder a
  in Http.post r.decoder out.url (Http.string out.body)
