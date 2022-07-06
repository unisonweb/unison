{-# LANGUAGE OverloadedStrings #-}

module Unison.PrettyPrintEnv.Names (fromNames, fromSuffixNames) where

import qualified Unison.HashQualified' as HQ'
import Unison.NamesWithHistory (NamesWithHistory)
import qualified Unison.NamesWithHistory as Names
import Unison.Prelude
import Unison.PrettyPrintEnv (PrettyPrintEnv (PrettyPrintEnv))

fromNames :: Int -> NamesWithHistory -> PrettyPrintEnv
fromNames len names = PrettyPrintEnv terms' types'
  where
    terms' r = shortestName (Names.termName len r names)
    types' r = shortestName (Names.typeName len r names)
    shortestName ns = HQ'.sortByLength (toList ns)

fromSuffixNames :: Int -> NamesWithHistory -> PrettyPrintEnv
fromSuffixNames len names = PrettyPrintEnv terms' types'
  where
    terms' r = Names.suffixedTermName len r names
    types' r = Names.suffixedTypeName len r names
