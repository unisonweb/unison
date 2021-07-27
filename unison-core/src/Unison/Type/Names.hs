{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Type.Names where

import Unison.Prelude

import Unison.Type
import qualified Control.Monad.Writer.Strict as Writer
import Data.Functor.Identity (runIdentity)
import Data.Monoid (Any(..))
import           Data.List.Extra (nubOrd)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Prelude.Extras (Eq1(..),Show1(..),Ord1(..))
import qualified Unison.ABT as ABT
import           Unison.Hashable (Hashable1)
import qualified Unison.Hashable as Hashable
import qualified Unison.Kind as K
import           Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import qualified Unison.Reference.Util as ReferenceUtil
import           Unison.Var (Var)
import qualified Unison.Var as Var
import qualified Unison.Settings as Settings
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