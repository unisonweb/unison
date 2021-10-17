{-# Language OverloadedStrings #-}

module Unison.PrettyPrintEnv.Names (fromNames, fromSuffixNames) where

import Unison.Prelude

import qualified Data.Set as Set
import qualified Unison.HashQualified as HQ
import Unison.Names3 (Names)
import qualified Unison.Names3 as Names
import Unison.PrettyPrintEnv (PrettyPrintEnv (PrettyPrintEnv))
import Unison.Util.List (safeHead)
import Unison.Util.Convert (convert)

fromNames :: Int -> Names -> PrettyPrintEnv
fromNames len names = PrettyPrintEnv terms' types' where
  terms' r = shortestName . Set.map convert $ Names.termName len r names
  types' r = shortestName . Set.map convert $ Names.typeName len r names
  shortestName ns = safeHead $ HQ.sortByLength (toList ns)

fromSuffixNames :: Int -> Names -> PrettyPrintEnv
fromSuffixNames len names = PrettyPrintEnv terms' types' where
  terms' r = safeHead $ Names.suffixedTermName len r names
  types' r = safeHead $ Names.suffixedTypeName len r names
