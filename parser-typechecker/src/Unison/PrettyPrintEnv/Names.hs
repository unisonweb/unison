{-# LANGUAGE OverloadedStrings #-}

module Unison.PrettyPrintEnv.Names (fromNames) where

import qualified Data.Set as Set
import Data.Set.NonEmpty (NESet)
import qualified Unison.Codebase.Path as Path
import qualified Unison.HashQualified' as HQ'
import Unison.Name (Name)
import qualified Unison.Name as Name
import qualified Unison.Names as Names
import Unison.NamesWithHistory (NamesWithHistory)
import qualified Unison.NamesWithHistory as NamesWithHistory
import Unison.Prelude
import Unison.PrettyPrintEnv (Perspective (..), PrettyPrintEnv (..), Suffixify (..))
import qualified Unison.Util.Relation as Rel

-- | Creates a PPE which is biased towards a particular name.
--
-- This is helpful when printing a specific definition,
--
-- e.g. when pretty-printing for `view base.List.map`, we should prefer names which are close
-- to `base.List.map`.
fromNames :: Int -> Maybe (NESet Path.Absolute) -> Perspective -> Maybe Name -> Suffixify -> NamesWithHistory -> PrettyPrintEnv
fromNames hashLen restrictions perspective bias suffixify names = PrettyPrintEnv {termNames = terms', typeNames = types', bias, perspective, suffixify, restrictions}
  where
    terms' restr p b suff r =
      NamesWithHistory.termName hashLen r names
        & Set.toList
        & restrict restr
        & prioritizeBias b
        & relativizeToPerspective p
        & case suff of
          Suffixify -> shortestUniqueSuffixes r (Names.terms $ NamesWithHistory.currentNames names)
          NoSuffixify -> id
    types' restr p b suff r =
      NamesWithHistory.typeName hashLen r names
        & Set.toList
        & restrict restr
        & prioritizeBias b
        & relativizeToPerspective p
        & case suff of
          Suffixify -> shortestUniqueSuffixes r (Names.types $ NamesWithHistory.currentNames names)
          NoSuffixify -> id

-- | Adjust names to be relative to a perspective.
relativizeToPerspective :: Perspective -> [HQ'.HashQualified Name] -> [HQ'.HashQualified Name]
relativizeToPerspective p ns =
  case p of
    Root -> ns
    RelativeTo p ->
      ns <&> fmap \name -> fromMaybe name $ do
        pathName <- Path.toName $ Path.unabsolute p
        Name.stripNamePrefix pathName name

-- | Only return names within the given set of paths.
restrict :: Maybe (NESet Path.Absolute) -> [HQ'.HashQualified Name] -> [HQ'.HashQualified Name]
restrict Nothing ns = ns
restrict (Just paths) ns = do
  let pathNames = catMaybes (Path.toName . Path.unabsolute <$> toList paths)
  ns & filter (\hqname -> any (\pn -> Name.isPrefixOf pn (HQ'.toName hqname)) pathNames)

prioritizeBias :: Maybe Name -> [HQ'.HashQualified Name] -> [HQ'.HashQualified Name]
prioritizeBias mayBias = sortOn \n ->
  (negate . length . Name.commonPrefix (HQ'.toName n) <$> mayBias, HQ'.toPriority n)

shortestUniqueSuffixes :: Ord ref => ref -> Rel.Relation Name ref -> [HQ'.HashQualified Name] -> [HQ'.HashQualified Name]
shortestUniqueSuffixes ref rel names = names <&> fmap (\name -> Name.shortestUniqueSuffix name ref rel)
