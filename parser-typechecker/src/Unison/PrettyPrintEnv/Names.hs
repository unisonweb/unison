module Unison.PrettyPrintEnv.Names
  ( fromNames,
    fromSuffixNames,
    prioritize,
    shortestUniqueSuffixes,
  )
where

import Data.Set qualified as Set
import Unison.HashQualified' qualified as HQ'
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.Names (Names)
import Unison.Names qualified as Names
import Unison.NamesWithHistory qualified as Names
import Unison.Prelude
import Unison.PrettyPrintEnv (PrettyPrintEnv (PrettyPrintEnv))
import Unison.Util.Relation qualified as Rel

fromNames :: Int -> Names -> PrettyPrintEnv
fromNames len names = PrettyPrintEnv terms' types'
  where
    terms' r =
      Names.termName len r names
        & Set.toList
        & fmap (\n -> (n, n))
        & prioritize
    types' r =
      Names.typeName len r names
        & Set.toList
        & fmap (\n -> (n, n))
        & prioritize

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

fromSuffixNames :: Int -> Names -> PrettyPrintEnv
fromSuffixNames len names = PrettyPrintEnv terms' types'
  where
    terms' r =
      Names.termName len r names
        & Set.toList
        & fmap (\n -> (n, n))
        & shortestUniqueSuffixes (Names.terms names)
        & prioritize
    types' r =
      Names.typeName len r names
        & Set.toList
        & fmap (\n -> (n, n))
        & shortestUniqueSuffixes (Names.types names)
        & prioritize

-- | Reduce the provided names to their minimal unique suffix within the scope of the given
-- relation.
shortestUniqueSuffixes :: (Ord ref) => Rel.Relation Name ref -> [(a, HQ'.HashQualified Name)] -> [(a, HQ'.HashQualified Name)]
shortestUniqueSuffixes rel names = names <&> second (fmap (\name -> Name.shortestUniqueSuffix name rel))
