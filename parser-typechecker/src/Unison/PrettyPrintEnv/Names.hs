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

fromNames :: forall m. Applicative m => Int -> NamesWithHistory -> PrettyPrintEnv m
fromNames len names = PrettyPrintEnv terms' types'
  where
    terms' r =
      NamesWithHistory.termName len r names
        & Set.toList
        & fmap (\n -> (n, n))
        & prioritize
        & pure
    types' r =
      NamesWithHistory.typeName len r names
        & Set.toList
        & fmap (\n -> (n, n))
        & prioritize
        & pure

-- | Sort the names for a given ref by the following factors (in priority order):
--
-- 1. Prefer Relative Names to Absolute Names
-- 2. Prefer names that aren't hash qualified to those that are
-- 3. Prefer names which have fewer segments in their fully-qualified form
-- 4. Prefer names which have fewer segments in their suffixified form (if applicable)
prioritize :: [(HQ'.HashQualified Name, HQ'.HashQualified Name)] -> [(HQ'.HashQualified Name, HQ'.HashQualified Name)]
prioritize =
  sortOn \case
    (fqn, HQ'.NameOnly name) -> (Name.isAbsolute name, Nothing, Name.countSegments (HQ'.toName fqn), Name.countSegments name)
    (fqn, HQ'.HashQualified name hash) -> (Name.isAbsolute name, Just hash, Name.countSegments (HQ'.toName fqn), Name.countSegments name)

fromSuffixNames :: forall m. Applicative m => Int -> NamesWithHistory -> PrettyPrintEnv m
fromSuffixNames len names = PrettyPrintEnv terms' types'
  where
    terms' r =
      NamesWithHistory.termName len r names
        & Set.toList
        & fmap (\n -> (n, n))
        & shortestUniqueSuffixes r (Names.terms $ NamesWithHistory.currentNames names)
        & prioritize
        & pure
    types' r =
      NamesWithHistory.typeName len r names
        & Set.toList
        & fmap (\n -> (n, n))
        & shortestUniqueSuffixes r (Names.types $ NamesWithHistory.currentNames names)
        & prioritize
        & pure

-- | Reduce the provided names to their minimal unique suffix within the scope of the given
-- relation.
shortestUniqueSuffixes :: Ord ref => ref -> Rel.Relation Name ref -> [(a, HQ'.HashQualified Name)] -> [(a, HQ'.HashQualified Name)]
shortestUniqueSuffixes ref rel names = names <&> second (fmap (\name -> Name.shortestUniqueSuffix name ref rel))
