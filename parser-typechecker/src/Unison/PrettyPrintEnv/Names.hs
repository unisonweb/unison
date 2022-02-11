{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# Language OverloadedStrings #-}

module Unison.PrettyPrintEnv.Names (fromNames, fromSuffixNames) where

import Unison.Prelude

import qualified Unison.HashQualified' as HQ'
import Unison.NamesWithHistory (NamesWithHistory)
import qualified Unison.NamesWithHistory as Names
import Unison.PrettyPrintEnv (PrettyPrintEnv (PrettyPrintEnv))
import Unison.Util.List (safeHead)

fromNames :: Int -> NamesWithHistory -> PrettyPrintEnv
fromNames len names = PrettyPrintEnv terms' types' where
  terms' r = shortestName (Names.termName len r names)
  types' r = shortestName (Names.typeName len r names)
  shortestName ns = safeHead $ HQ'.sortByLength (toList ns)

fromSuffixNames :: Int -> NamesWithHistory -> PrettyPrintEnv
fromSuffixNames len names = PrettyPrintEnv terms' types' where
  terms' r = safeHead $ Names.suffixedTermName len r names
  types' r = safeHead $ Names.suffixedTypeName len r names
