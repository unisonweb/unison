module Unison.Action where

import Unison.Jsonify as J
import Unison.Jsonify (Jsonify)
import Unison.Term (Term)
import Unison.Term as E

data Action
  = Abstract -- Turn target into function parameter
  | Beta -- Beta reduce the target
  | Eta -- Eta reduce the target
  | LetFloat -- Float the target out to a let binding, as far as possible
  | WHNF -- Simplify target to weak head normal form
  | Apply Term -- Replace the target, `e`, with `f e`

jsonify : Jsonify Action
jsonify a = case a of
  Abstract -> J.tag' "Abstract" J.product0 ()
  Beta -> J.tag' "Beta" J.product0 ()
  Eta -> J.tag' "Eta" J.product0 ()
  LetFloat -> J.tag' "LetFloat" J.product0 ()
  WHNF -> J.tag' "WHNF" J.product0 ()
  Apply f -> J.tag' "Apply" E.jsonifyTerm f
