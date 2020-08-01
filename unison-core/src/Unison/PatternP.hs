{-# Language PatternSynonyms #-}

module Unison.PatternP where

import Unison.Prelude

import qualified Unison.Pattern as P
import Unison.LabeledDependency (LabeledDependency)
import Unison.Reference (ReferenceH)

type Pattern loc = P.PatternP loc
type PatternH h loc = P.PatternH h loc
type PatternH' d e loc = P.PatternH' d e loc

pattern Unbound loc = P.UnboundP loc
pattern Var loc = P.VarP loc
pattern Boolean loc b = P.BooleanP loc b
pattern Int loc n = P.IntP loc n
pattern Nat loc n = P.NatP loc n
pattern Float loc n = P.FloatP loc n
pattern Text loc t = P.TextP loc t
pattern Char loc c = P.CharP loc c
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

type ConstructorId = P.ConstructorId

loc :: P.PatternP loc -> loc
loc = P.loc

hmap :: (h -> h') -> P.PatternH h loc -> P.PatternH h' loc
hmap = P.hmap

setLoc :: P.PatternP loc -> loc -> P.PatternP loc
setLoc = P.setLoc

labeledDependencies :: P.PatternP loc -> Set LabeledDependency
labeledDependencies = P.labeledDependencies

generalizedDependencies
  :: Ord r
  => (ReferenceH h -> r)
  -> (ReferenceH h -> P.ConstructorId -> r)
  -> (ReferenceH h -> r)
  -> (ReferenceH h -> P.ConstructorId -> r)
  -> (ReferenceH h -> r)
  -> P.PatternH h loc
  -> Set r
generalizedDependencies = P.generalizedDependencies
