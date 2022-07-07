{-# LANGUAGE OverloadedStrings #-}

module Unison.PrettyPrintEnv.Names (fromNames, fromSuffixNames) where

import qualified Unison.HashQualified' as HQ'
import Unison.Name (Name)
import qualified Unison.Name as Name
import Unison.NamesWithHistory (NamesWithHistory)
import qualified Unison.NamesWithHistory as Names
import Unison.Prelude
import Unison.PrettyPrintEnv (PrettyPrintEnv (PrettyPrintEnv))

fromNames :: Int -> Maybe Name -> NamesWithHistory -> PrettyPrintEnv
fromNames len mayBias names = PrettyPrintEnv terms' types'
  where
    terms' r = prioritize (toList $ Names.termName len r names)
    types' r = prioritize (toList $ Names.typeName len r names)
    prioritize = sortOn \n ->
      (length . Name.commonPrefix (HQ'.toName n) <$> mayBias, HQ'.toPriority n)

fromSuffixNames :: Int -> Maybe Name -> NamesWithHistory -> PrettyPrintEnv
fromSuffixNames len mayBias names = PrettyPrintEnv terms' types'
  where
    terms' r = Names.suffixedTermName len mayBias r names
    types' r = Names.suffixedTypeName len mayBias r names
