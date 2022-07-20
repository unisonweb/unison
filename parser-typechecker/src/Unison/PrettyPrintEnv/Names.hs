{-# LANGUAGE OverloadedStrings #-}

module Unison.PrettyPrintEnv.Names (fromNames, fromSuffixNames) where

import Data.Bifunctor (second)
import qualified Data.Set as Set
import qualified Unison.HashQualified' as HQ'
import Unison.Name (Name)
import qualified Unison.Name as Name
import qualified Unison.Names as Names
import Unison.NamesWithHistory (NamesWithHistory)
import qualified Unison.NamesWithHistory as NamesWithHistory
import Unison.Prelude
import Unison.PrettyPrintEnv (PrettyPrintEnv (PrettyPrintEnv))
import qualified Unison.Util.Relation as Rel

fromNames :: Int -> NamesWithHistory -> PrettyPrintEnv
fromNames len names = PrettyPrintEnv terms' types'
  where
    terms' r =
      NamesWithHistory.termName len r names
        & Set.toList
        & fmap (\n -> (n, n))
        & prioritize
    types' r =
      NamesWithHistory.typeName len r names
        & Set.toList
        & fmap (\n -> (n, n))
        & prioritize

prioritize :: [(HQ'.HashQualified Name, HQ'.HashQualified Name)] -> [(HQ'.HashQualified Name, HQ'.HashQualified Name)]
prioritize =
  sortOn \case
    (fqn, HQ'.NameOnly name) -> (Nothing, Name.countSegments name, Name.countSegments (HQ'.toName fqn), Name.isAbsolute name)
    (fqn, HQ'.HashQualified name hash) -> (Just hash, Name.countSegments name, Name.countSegments (HQ'.toName fqn), Name.isAbsolute name)

fromSuffixNames :: Int -> NamesWithHistory -> PrettyPrintEnv
fromSuffixNames len names = PrettyPrintEnv terms' types'
  where
    terms' r =
      NamesWithHistory.termName len r names
        & Set.toList
        & fmap (\n -> (n, n))
        & shortestUniqueSuffixes r (Names.terms $ NamesWithHistory.currentNames names)
        & prioritize
    types' r =
      NamesWithHistory.typeName len r names
        & Set.toList
        & fmap (\n -> (n, n))
        & shortestUniqueSuffixes r (Names.types $ NamesWithHistory.currentNames names)
        & prioritize

shortestUniqueSuffixes :: Ord ref => ref -> Rel.Relation Name ref -> [(a, HQ'.HashQualified Name)] -> [(a, HQ'.HashQualified Name)]
shortestUniqueSuffixes ref rel names = names <&> second (fmap (\name -> Name.shortestUniqueSuffix name ref rel))
