module Unison.Action where

import Elmz.Json.Encoder as Encoder
import Elmz.Json.Encoder (Encoder)
import Elmz.Json.Decoder as Decoder
import Elmz.Json.Decoder (Decoder)
import Unison.Term (Term)
import Unison.Term as E

type Action
  = Abstract -- Turn target into function parameter
  | Step -- Beta reduce the target
  | Eta -- Eta reduce the target
  | LetFloat -- Float the target out to a let binding, as far as possible
  | WHNF -- Simplify target to weak head normal form
  | Noop -- Do nothing to the target

encode : Encoder Action
encode a = case a of
  Abstract -> Encoder.string "Abstract"
  Step -> Encoder.string "Step"
  Eta -> Encoder.string "Eta"
  LetFloat -> Encoder.string "LetFloat"
  WHNF -> Encoder.string "WHNF"
  Noop -> Encoder.string "Noop"
