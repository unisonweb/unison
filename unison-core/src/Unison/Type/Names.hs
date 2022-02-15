{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
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
import qualified Unison.Names as Names
import qualified Unison.NamesWithHistory as Names
import qualified Unison.Names.ResolutionResult as Names
import qualified Unison.Name as Name
import qualified Unison.Util.List as List
import qualified Data.Set.NonEmpty as NES

bindNames
  :: Var v
  => Set v
  -> Names.Names
  -> Type v a
  -> Names.ResolutionResult v a (Type v a)
bindNames keepFree ns0 t = let
  ns = Names.NamesWithHistory ns0 mempty
  fvs = ABT.freeVarOccurrences keepFree t
  rs = [(v, a, Names.lookupHQType (Name.convert $ Name.unsafeFromVar v) ns) | (v,a) <- fvs ]
  ok (v, a, rs) =
    if Set.size rs == 1
       then pure (v, Set.findMin rs)
       else
         case NES.nonEmptySet rs of
           Nothing -> Left (pure (Names.TypeResolutionFailure v a Names.NotFound))
           Just rs' -> Left (pure (Names.TypeResolutionFailure v a (Names.Ambiguous ns0 rs')))
  in List.validate ok rs <&> \es -> bindExternal es t
