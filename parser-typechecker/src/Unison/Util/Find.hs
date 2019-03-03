{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Unison.Util.Find where

-- import Debug.Trace
import Data.Foldable (toList)
import Data.Maybe (catMaybes)
import qualified Data.List as List
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Text.Regex.TDFA as RE
import qualified Unison.Util.Pretty as P
import Unison.Util.Monoid (intercalateMap)

fuzzyFinder :: String -> [String] -> [(String, P.Pretty P.ColorText)]
fuzzyFinder query items = fuzzyFinder' query items id

fuzzyFinder' :: forall a.
  String -> [a] -> (a -> String) -> [(a, P.Pretty P.ColorText)]
fuzzyFinder' query items render = sortAndCleanup . scoreAndHighlight $ items
  where
  scoreAndHighlight = catMaybes . List.map go
  sortAndCleanup = List.map snd . List.sortOn fst
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
  regex = let
    s = --traceShowId . trace "regex" $
        if null query then ".*"
        else intercalateMap ".*" esc query where esc c = "(" <> [c] <> ")"
    in RE.makeRegexOpts
        RE.defaultCompOpt { RE.caseSensitive = False }
        RE.defaultExecOpt
        s
  --todo: make regex case-insensitive using CompOption
  -- https://www.stackage.org/haddock/lts-13.9/regex-tdfa-1.2.3.1/Text-Regex-TDFA-String.html#v:execute
  -- Sort on:
  -- a. length of match group to find the most compact match
  -- b. start position of the match group to find the earliest match
  -- c. the item itself for alphabetical ranking
  -- Ord MatchArray already provides a. and b.  todo: c.

-- only search before the # before the # and after the # after the #
-- fuzzyFindHashQualified :: HashQualified
--                        -> Branch0
--                        -> [(Either Referent Reference, P.Pretty P.ColorText)]
-- fuzzyFindHashQualified hq ns =


type Pos = Int
type Len = Int
-- This [(Pos, Len)] type is the same as `tail . toList` of a regex MatchArray
highlight :: (P.Pretty P.ColorText -> P.Pretty P.ColorText)
          -> Text
          -> [(Pos, Len)]
          -> P.Pretty P.ColorText
highlight on = highlight' on id

highlight' :: (P.Pretty P.ColorText -> P.Pretty P.ColorText)
          -> (P.Pretty P.ColorText -> P.Pretty P.ColorText)
          -> Text
          -> [(Pos, Len)]
          -> P.Pretty P.ColorText
highlight' on off t groups = case groups of
  [] -> (off . P.text)  t
  (0,_) : _ -> go groups
  (start,_) : _ -> (off . P.text . Text.take start) t <> go groups
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
