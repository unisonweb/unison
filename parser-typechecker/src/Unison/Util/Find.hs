{-# LANGUAGE ViewPatterns #-}

module Unison.Util.Find
  ( fuzzyFinder,
    simpleFuzzyFinder,
    simpleFuzzyScore,
    fuzzyFindInBranch,
    fuzzyFindMatchArray,
    prefixFindInBranch,
  )
where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Text as Text
-- http://www.serpentine.com/blog/2007/02/27/a-haskell-regular-expression-tutorial/
-- https://www.stackage.org/haddock/lts-13.9/regex-base-0.93.2/Text-Regex-Base-Context.html -- re-exported by TDFA
-- https://www.stackage.org/haddock/lts-13.9/regex-tdfa-1.2.3.1/Text-Regex-TDFA.html
import qualified Text.Regex.TDFA as RE
import Unison.Codebase.SearchResult (SearchResult)
import qualified Unison.Codebase.SearchResult as SR
import Unison.HashQualified' (HashQualified)
import qualified Unison.HashQualified' as HQ
import qualified Unison.Name as Name
import Unison.NamePrinter (prettyHashQualified')
import Unison.Names2 (Names0)
import qualified Unison.Names2 as Names
import Unison.Prelude
import qualified Unison.Reference as Reference
import qualified Unison.Referent as Referent
import qualified Unison.ShortHash as SH
import Unison.Util.Monoid (intercalateMap)
import qualified Unison.Util.Pretty as P
import qualified Unison.Util.Relation as R

fuzzyFinder ::
  forall a.
  String ->
  [a] ->
  (a -> String) ->
  [(a, P.Pretty P.ColorText)]
fuzzyFinder query items render =
  sortAndCleanup $ fuzzyFindMatchArray query items render
  where
    sortAndCleanup = List.map snd . List.sortOn fst

simpleFuzzyFinder ::
  forall a.
  String ->
  [a] ->
  (a -> String) ->
  [(a, P.Pretty P.ColorText)]
simpleFuzzyFinder query items render =
  sortAndCleanup $ do
    a <- items
    let s = render a
    score <- toList (simpleFuzzyScore query s)
    pure ((a, hi s), score)
  where
    hi = highlightSimple query
    sortAndCleanup = List.map fst . List.sortOn snd

-- highlights `query` if it is a prefix of `s`, or if it
-- appears in the final segement of s (after the final `.`)
highlightSimple :: String -> String -> P.Pretty P.ColorText
highlightSimple "" = P.string
highlightSimple query = go
  where
    go [] = mempty
    go s@(h : t)
      | query `List.isPrefixOf` s = hiQuery <> go (drop len s)
      | otherwise = P.string [h] <> go t
    len = length query
    hiQuery = P.hiBlack (P.string query)

simpleFuzzyScore :: String -> String -> Maybe Int
simpleFuzzyScore query s
  | query `List.isPrefixOf` s = Just (bonus s 2)
  | query `List.isSuffixOf` s = Just (bonus s 1)
  | query `List.isInfixOf` s = Just (bonus s 3)
  | lowerquery `List.isInfixOf` lowers = Just (bonus s 4)
  | otherwise = Nothing
  where
    -- prefer relative names
    bonus ('.' : _) n = n * 10
    bonus _ n = n
    lowerquery = Char.toLower <$> query
    lowers = Char.toLower <$> s

-- This logic was split out of fuzzyFinder because the `RE.MatchArray` has an
-- `Ord` instance that helps us sort the fuzzy matches in a nice way. (see
-- comment below.)  `Editor.fuzzyNameDistance` uses this `Ord` instance.
fuzzyFindMatchArray ::
  forall a.
  String ->
  [a] ->
  (a -> String) ->
  [(RE.MatchArray, (a, P.Pretty P.ColorText))]
fuzzyFindMatchArray query items render =
  scoreAndHighlight $ items
  where
    scoreAndHighlight = catMaybes . List.map go
    go :: a -> Maybe (RE.MatchArray, (a, P.Pretty P.ColorText))
    go a =
      let string = render a
          text = Text.pack string
          matches = RE.matchOnce regex string
          addContext matches =
            let highlighted = highlight P.bold text . tail . toList $ matches
             in (matches, (a, highlighted))
       in addContext <$> matches
    -- regex "Foo" = "(\\F).*(\\o).*(\\o)"
    regex :: RE.Regex
    regex =
      let s =
            if null query
              then ".*"
              else intercalateMap ".*" esc query
            where
              esc c = "(\\" <> [c] <> ")"
       in RE.makeRegexOpts
            RE.defaultCompOpt
              { RE.caseSensitive = False,
                -- newSyntax = False,  otherwise "\<" and "\>"
                -- matches word boundaries instead of literal < and >
                RE.newSyntax = False
              }
            RE.defaultExecOpt
            s

-- Sort on:
-- a. length of match group to find the most compact match
-- b. start position of the match group to find the earliest match
-- c. the item itself for alphabetical ranking
-- Ord MatchArray already provides a. and b.  todo: c.

prefixFindInBranch ::
  Names0 -> HashQualified -> [(SearchResult, P.Pretty P.ColorText)]
prefixFindInBranch b hq = fmap getName $
  case HQ.toName hq of
    -- query string includes a name component, so do a prefix find on that
    (Name.toString -> n) ->
      filter (filterName n) (candidates b hq)
  where
    filterName n sr =
      fromString n `Name.isPrefixOf` (HQ.toName . SR.name) sr

-- only search before the # before the # and after the # after the #
fuzzyFindInBranch ::
  Names0 ->
  HashQualified ->
  [(SearchResult, P.Pretty P.ColorText)]
fuzzyFindInBranch b hq =
  case HQ.toName hq of
    (Name.toString -> n) ->
      simpleFuzzyFinder
        n
        (candidates b hq)
        (Name.toString . HQ.toName . SR.name)

getName :: SearchResult -> (SearchResult, P.Pretty P.ColorText)
getName sr = (sr, P.syntaxToColor $ prettyHashQualified' (SR.name sr))

candidates :: Names.Names' Name.Name -> HashQualified -> [SearchResult]
candidates b hq = typeCandidates <> termCandidates
  where
    -- filter branch by hash
    typeCandidates =
      fmap typeResult . filterTypes . R.toList . Names.types $ b
    termCandidates =
      fmap termResult . filterTerms . R.toList . Names.terms $ b
    filterTerms = case HQ.toHash hq of
      Just sh -> List.filter $ SH.isPrefixOf sh . Referent.toShortHash . snd
      Nothing -> id
    filterTypes = case HQ.toHash hq of
      Just sh -> List.filter $ SH.isPrefixOf sh . Reference.toShortHash . snd
      Nothing -> id
    typeResult (n, r) =
      SR.typeResult
        (Names._hqTypeName b n r)
        r
        (Names._hqTypeAliases b n r)
    termResult (n, r) =
      SR.termResult
        (Names._hqTermName b n r)
        r
        (Names._hqTermAliases b n r)

type Pos = Int

type Len = Int

-- This [(Pos, Len)] type is the same as `tail . toList` of a regex MatchArray
highlight ::
  (P.Pretty P.ColorText -> P.Pretty P.ColorText) ->
  Text ->
  [(Pos, Len)] ->
  P.Pretty P.ColorText
highlight on = highlight' on id

highlight' ::
  (P.Pretty P.ColorText -> P.Pretty P.ColorText) ->
  (P.Pretty P.ColorText -> P.Pretty P.ColorText) ->
  Text ->
  [(Pos, Len)] ->
  P.Pretty P.ColorText
highlight' on off t groups = case groups of
  [] -> (off . P.text) t
  (0, _) : _ -> go groups
  (start, _) : _ -> (off . P.text . Text.take start) t <> go groups
  where
    go = \case
      [] -> error "unpossible I think"
      (start, len) : (start2, len2) : groups
        | start + len == start2 ->
          -- avoid an on/off since there's no gap between groups
          go ((start, len + len2) : groups)
      (start, len) : groups ->
        let (selected, remaining) = Text.splitAt len . Text.drop start $ t
         in (on . P.text) selected <> case groups of
              [] -> (off . P.text) remaining
              (start2, _) : _ ->
                (off . P.text . Text.drop (start + len) . Text.take start2 $ t)
                  <> go groups
