module Unison.Hash where

import Elmz.Json.Encoder as Encoder
import Elmz.Json.Encoder (Encoder)
import Elmz.Json.Decoder as Decoder
import Elmz.Json.Decoder (Decoder)

type Hash = String

base64 : Hash -> String
base64 h = h

decode : Decoder Hash
decode = Decoder.string

encode : Encoder Hash
encode = Encoder.string
