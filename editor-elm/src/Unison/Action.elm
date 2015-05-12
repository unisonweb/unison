module Unison.Action where

import Elmz.Json.Encoder as Encoder
import Elmz.Json.Encoder exposing (Encoder)
import Elmz.Json.Decoder as Decoder
import Elmz.Json.Decoder exposing (Decoder)
import Unison.Symbol exposing (Symbol)
import Unison.Symbol as Symbol

type Action
  = Abstract -- Turn target into function parameter
  | AbstractLet -- Turn target into let bound expression
  | AllowRec -- Turn a let into a let rec
  | EtaReduce -- Eta reduce the target
  | FloatOut -- Float the target binding out one level
  | Inline -- Delete a let binding by inlining its definition into usage sites
  | MergeLet -- Merge a let block into its parent let block
  | Noop -- Do nothing to the target
  | Rename Symbol -- Rename the target var
  | Step -- Link + beta reduce the target
  | SwapDown -- Swap the target let binding with the subsequent binding
  | SwapUp -- Swap the target let binding with the previous binding
  | WHNF -- Simplify target to weak head normal form

encode : Encoder Action
encode a = case a of
  Abstract -> Encoder.tag' "Abstract" Encoder.product0 ()
  AbstractLet -> Encoder.tag' "AbstractLet" Encoder.product0 ()
  AllowRec -> Encoder.tag' "AllowRec" Encoder.product0 ()
  EtaReduce -> Encoder.tag' "EtaReduce" Encoder.product0 ()
  FloatOut -> Encoder.tag' "FloatOut" Encoder.product0 ()
  Inline -> Encoder.tag' "Inline" Encoder.product0 ()
  MergeLet -> Encoder.tag' "MergeLet" Encoder.product0 ()
  Noop -> Encoder.tag' "Noop" Encoder.product0 ()
  Rename s -> Encoder.tag' "Rename" Symbol.encodeSymbol s
  Step -> Encoder.tag' "Step" Encoder.product0 ()
  SwapDown -> Encoder.tag' "SwapDown" Encoder.product0 ()
  SwapUp -> Encoder.tag' "SwapUp" Encoder.product0 ()
  WHNF -> Encoder.tag' "WHNF" Encoder.product0 ()
