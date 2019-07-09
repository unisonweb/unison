{-# Language PatternSynonyms #-}

module Unison.PatternP where

import qualified Unison.Pattern as P
import Unison.Referent (Referent)
import Unison.Reference (Reference)
import Data.Set.Internal (Set)

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
pattern SequenceOp loc l op r = P.SequenceOpP loc l op r

type SeqOp = P.SeqOp
pattern Snoc = P.Snoc
pattern Cons = P.Cons
pattern Concat = P.Concat

loc :: P.PatternP loc -> loc
loc = P.loc

setLoc :: P.PatternP loc -> loc -> P.PatternP loc
setLoc = P.setLoc

labeledDependencies :: P.PatternP loc -> Set (Either Reference Referent)
labeledDependencies = P.labeledDependencies