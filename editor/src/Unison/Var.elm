module Unison.Var where

import Elmz.Json.Encoder as Encoder
import Elmz.Json.Encoder (Encoder)
import Elmz.Json.Decoder as Decoder
import Elmz.Json.Decoder (Decoder)

type alias I = Int

z : I
z = 0

succ : I -> I
succ i = i + 1

decode : Decoder I
decode = Decoder.int

encode : Encoder I
encode i = Encoder.float (toFloat i)
