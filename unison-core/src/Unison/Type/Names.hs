{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Type.Names where

import Unison.Prelude

import Unison.Type
import qualified Data.Set as Set
import qualified Unison.ABT as ABT
import Unison.Var (Var)
import qualified Unison.Names3 as Names
import qualified Unison.Names.ResolutionResult as Names
import qualified Unison.Name as Name
import qualified Unison.Util.List as List

bindNames
  :: Var v
  => Set v
  -> Names.Names0
  -> Type v a
  -> Names.ResolutionResult v a (Type v a)
bindNames keepFree ns0 t = let
  ns = Names.Names ns0 mempty
  fvs = ABT.freeVarOccurrences keepFree t
  rs = [(v, a, Names.lookupHQType (Name.convert $ Name.fromVar v) ns) | (v,a) <- fvs ]
  ok (v, a, rs) = if Set.size rs == 1 then pure (v, Set.findMin rs)
                  else Left (pure (Names.TypeResolutionFailure v a rs))
  in List.validate ok rs <&> \es -> bindExternal es t