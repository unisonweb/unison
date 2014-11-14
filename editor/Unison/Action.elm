module Unison.Action where

import Elmz.Json.Encoder as Encoder
import Elmz.Json.Encoder (Encoder)
import Elmz.Json.Decoder as Decoder
import Elmz.Json.Decoder (Decoder)
import Unison.Term (Term)
import Unison.Term as E

data Action
  = Abstract -- Turn target into function parameter
  | Beta -- Beta reduce the target
  | Eta -- Eta reduce the target
  | LetFloat -- Float the target out to a let binding, as far as possible
  | WHNF -- Simplify target to weak head normal form
  | Apply Term -- Replace the target, `e`, with `f e`

encode : Encoder Action
encode a = case a of
  Abstract -> Encoder.tag' "Abstract" Encoder.product0 ()
  Beta -> Encoder.tag' "Beta" Encoder.product0 ()
  Eta -> Encoder.tag' "Eta" Encoder.product0 ()
  LetFloat -> Encoder.tag' "LetFloat" Encoder.product0 ()
  WHNF -> Encoder.tag' "WHNF" Encoder.product0 ()
  Apply f -> Encoder.tag' "Apply" E.encodeTerm f
