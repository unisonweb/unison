{-# Language PatternSynonyms #-}

module Unison.PatternP where

import qualified Unison.Pattern as P

type Pattern loc = P.PatternP loc

pattern Unbound loc = P.UnboundP loc
pattern Var loc = P.VarP loc
pattern Boolean loc b = P.BooleanP loc b
pattern Int64 loc n = P.Int64P loc n
pattern UInt64 loc n = P.UInt64P loc n
pattern Float loc n = P.FloatP loc n
pattern Constructor loc r cid ps = P.ConstructorP loc r cid ps
pattern As loc p = P.AsP loc p
pattern EffectPure loc p = P.EffectPureP loc p
pattern EffectBind loc r c args k = P.EffectBindP loc r c args k

loc :: P.PatternP loc -> loc
loc = P.loc

setLoc :: P.PatternP loc -> loc -> P.PatternP loc
setLoc = P.setLoc
