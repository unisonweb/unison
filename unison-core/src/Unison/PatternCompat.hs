{-# LANGUAGE PatternSynonyms #-}

module Unison.PatternCompat where

import qualified Unison.Pattern as P

type Pattern = P.Pattern ()

{-# COMPLETE Unbound, Var, Boolean, Int, Nat, Float, Text, Char, Constructor, As, EffectPure, EffectBind, SequenceLiteral, SequenceOp #-}

pattern Unbound = P.Unbound ()

pattern Var = P.Var ()

pattern Boolean b = P.Boolean () b

pattern Int n = P.Int () n

pattern Nat n = P.Nat () n

pattern Float n = P.Float () n

pattern Text t = P.Text () t

pattern Char c = P.Char () c

pattern Constructor r cid ps = P.Constructor () r cid ps

pattern As p = P.As () p

pattern EffectPure p = P.EffectPure () p

pattern EffectBind r cid ps k = P.EffectBind () r cid ps k

pattern SequenceLiteral ps = P.SequenceLiteral () ps

pattern SequenceOp ph op pt = P.SequenceOp () ph op pt

{-# COMPLETE Snoc, Cons, Concat #-}

type SeqOp = P.SeqOp

pattern Snoc = P.Snoc

pattern Cons = P.Cons

pattern Concat = P.Concat
