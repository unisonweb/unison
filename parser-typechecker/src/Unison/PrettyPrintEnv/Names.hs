{-# Language OverloadedStrings #-}

module Unison.PrettyPrintEnv.Names (fromNames, fromSuffixNames) where

import Unison.Prelude

import qualified Data.Set as Set
import qualified Unison.HashQualified as HQ
import qualified Unison.Name as Name
import Unison.Names3 (NamesWithHistory)
import qualified Unison.Names3 as Names
import Unison.PrettyPrintEnv (PrettyPrintEnv (PrettyPrintEnv))
import Unison.Util.List (safeHead)

fromNames :: Int -> NamesWithHistory -> PrettyPrintEnv
fromNames len names = PrettyPrintEnv terms' types' where
  terms' r = shortestName . Set.map Name.convert $ Names.termName len r names
  types' r = shortestName . Set.map Name.convert $ Names.typeName len r names
  shortestName ns = safeHead $ HQ.sortByLength (toList ns)

fromSuffixNames :: Int -> NamesWithHistory -> PrettyPrintEnv
fromSuffixNames len names = PrettyPrintEnv terms' types' where
  terms' r = safeHead $ Names.suffixedTermName len r names
  types' r = safeHead $ Names.suffixedTypeName len r names
