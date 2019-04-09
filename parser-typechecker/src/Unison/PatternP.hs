{-# Language PatternSynonyms, ViewPatterns #-}

module Unison.PatternP where

import qualified Unison.Pattern as P

type Pattern loc = P.PatternP loc

pattern Unbound loc = P.UnboundP loc
pattern Var loc = P.VarP loc
pattern Boolean loc b = P.BooleanP loc b
pattern Int loc n = P.IntP loc n
pattern Nat loc n = P.NatP loc n
pattern Float loc n = P.FloatP loc n
pattern Text loc t = P.TextP loc t
pattern Constructor loc r cid ps = P.ConstructorP loc r cid ps
pattern As loc p = P.AsP loc p
pattern EffectPure loc p = P.EffectPureP loc p
pattern EffectBind loc r c args k = P.EffectBindP loc r c args k
pattern SequenceLiteral loc ps = P.SequenceLiteralP loc ps
pattern SequenceOp loc ph op pt = P.SequenceOpP loc ph op pt

type SeqOp = P.SeqOp
pattern Snoc = P.Snoc
pattern Cons = P.Cons
-- pattern Concat = P.Concat

loc :: P.PatternP loc -> loc
loc = P.loc

setLoc :: P.PatternP loc -> loc -> P.PatternP loc
setLoc = P.setLoc
