{-# LANGUAGE OverloadedStrings #-}

module Unison.PrettyPrintEnv.Names (fromNames, fromSuffixNames, fromNamesWithBias, fromSuffixNamesWithBias) where

import qualified Unison.HashQualified' as HQ'
import Unison.Name (Name)
import qualified Unison.Name as Name
import Unison.NamesWithHistory (NamesWithHistory)
import qualified Unison.NamesWithHistory as Names
import Unison.Prelude
import Unison.PrettyPrintEnv (PrettyPrintEnv (PrettyPrintEnv))
import Unison.Util.List (safeHead)

-- | Creates a PPE which is biased towards a particular name.
--
-- This is helpful when printing a specific definition,
--
-- e.g. when pretty-printing for `view base.List.map`, we should prefer names which are close
-- to `base.List.map`.
fromNamesWithBias :: Int -> Maybe Name -> NamesWithHistory -> PrettyPrintEnv
fromNamesWithBias len mayBias names = PrettyPrintEnv terms' types'
  where
    terms' r = safeHead $ prioritize (toList $ Names.termName len r names)
    types' r = safeHead $ prioritize (toList $ Names.typeName len r names)
    prioritize = sortOn \n ->
      (negate . length . Name.commonPrefix (HQ'.toName n) <$> mayBias, HQ'.toPriority n)

-- | Creates a _suffixified_ PPE which is biased towards a particular name.
--
-- This is helpful when printing a specific definition,
--
-- e.g. when pretty-printing for `view base.List.map`, we should prefer names which are close
-- to `base.List.map`.
fromSuffixNamesWithBias :: Int -> Maybe Name -> NamesWithHistory -> PrettyPrintEnv
fromSuffixNamesWithBias len mayBias names = PrettyPrintEnv terms' types'
  where
    terms' r = safeHead $ Names.suffixedTermName len mayBias r names
    types' r = safeHead $ Names.suffixedTypeName len mayBias r names

fromNames :: Int -> NamesWithHistory -> PrettyPrintEnv
fromNames hl = fromNamesWithBias hl Nothing

fromSuffixNames :: Int -> NamesWithHistory -> PrettyPrintEnv
fromSuffixNames hl = fromSuffixNamesWithBias hl Nothing
