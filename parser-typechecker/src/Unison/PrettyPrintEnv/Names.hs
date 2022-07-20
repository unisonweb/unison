{-# LANGUAGE OverloadedStrings #-}

module Unison.PrettyPrintEnv.Names (fromNames) where

import Data.Bifunctor (second)
import qualified Data.Set as Set
import Data.Set.NonEmpty (NESet)
import Debug.Pretty.Simple (pTraceShow, pTraceShowId)
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
        & fmap (\n -> (n, n))
        & (\ns -> pTraceShow ("Prefilter" :: String, show r, ns) ns)
        & restrict restr
        & (\ns -> pTraceShow ("Postfilter" :: String, show r, restr, ns) ns)
        & relativizeToPerspective p
        & case suff of
          Suffixify -> shortestUniqueSuffixes r (Names.terms $ NamesWithHistory.currentNames names)
          NoSuffixify -> id
        & (\ns -> pTraceShow ("Postsuffixify" :: String, suff, ns) ns)
        & prioritizeBias b
        & (\ns -> pTraceShow ("Postbias" :: String, show bias, ns) ns)
        & pTraceShowId
    types' restr p b suff r =
      NamesWithHistory.typeName hashLen r names
        & Set.toList
        & fmap (\n -> (n, n))
        & (\ns -> pTraceShow ("Prefilter" :: String, r, ns) ns)
        & restrict restr
        & (\ns -> pTraceShow ("Postfilter" :: String, r, restr, ns) ns)
        & relativizeToPerspective p
        & case suff of
          Suffixify -> shortestUniqueSuffixes r (Names.types $ NamesWithHistory.currentNames names)
          NoSuffixify -> id
        & (\ns -> pTraceShow ("Postsuffixify" :: String, suff, ns) ns)
        & prioritizeBias b
        & (\ns -> pTraceShow ("Postbias" :: String, bias, ns) ns)
        & pTraceShowId

-- | Adjust names to be relative to a perspective.
relativizeToPerspective :: Perspective -> [(a, HQ'.HashQualified Name)] -> [(a, HQ'.HashQualified Name)]
relativizeToPerspective p ns =
  case p of
    Root -> ns
    RelativeTo p ->
      ns
        <&> second
          ( fmap \name -> fromMaybe name $ do
              pathName <- Path.toName' . Path.AbsolutePath' $ p
              Name.stripNamePrefix pathName name
          )

-- | Only return names within the given set of paths.
restrict :: Maybe (NESet Path.Absolute) -> [(HQ'.HashQualified Name, a)] -> [(HQ'.HashQualified Name, a)]
restrict Nothing ns = ns
restrict (Just paths) ns = do
  case catMaybes (Path.toName' . Path.AbsolutePath' <$> toList paths) of
    [] -> ns
    pathNames -> ns & filter (\(hqname, _a) -> any (\pn -> Name.isPrefixOf pn (HQ'.toName hqname)) pathNames)

prioritizeBias :: Maybe Name -> [(HQ'.HashQualified Name, HQ'.HashQualified Name)] -> [(HQ'.HashQualified Name, HQ'.HashQualified Name)]
prioritizeBias mayBias = sortOn \(n, n2) ->
  (negate . length . Name.commonPrefix (HQ'.toName n) <$> mayBias, HQ'.toPriority n2, HQ'.toPriority n)

shortestUniqueSuffixes :: Ord ref => ref -> Rel.Relation Name ref -> [(a, HQ'.HashQualified Name)] -> [(a, HQ'.HashQualified Name)]
shortestUniqueSuffixes ref rel names = names <&> second (fmap (\name -> Name.shortestUniqueSuffix name ref rel))
