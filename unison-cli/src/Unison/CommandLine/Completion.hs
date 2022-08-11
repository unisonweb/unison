{-
   This module defines tab-completion strategies for entering info via the CLI
-}
module Unison.CommandLine.Completion
  ( -- * Completers
    exactComplete,
    prefixCompleteTermTypeOrNamespace,
    noCompletions,
    prefixCompleteNamespace,
    -- Currently unused
    fuzzySuffixSegmentCompletionFilter,
  )
where

import qualified Control.Lens as Lens
import Data.List (intercalate, isPrefixOf)
import qualified Data.List as List
import Data.List.Extra (splitOn)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NESet
import qualified Data.Text as Text
import qualified System.Console.Haskeline as Line
import System.Console.Haskeline.Completion (Completion)
import qualified U.Codebase.Branch as V2Branch
import qualified U.Codebase.Causal as V2Causal
import qualified U.Util.Monoid as Monoid
import Unison.Codebase (Codebase)
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.SqliteCodebase.Conversions as Cv
import qualified Unison.NameSegment as NameSegment
import Unison.Prelude
import qualified Unison.Util.Find as Fuzzy
import qualified Unison.Util.Pretty as P
import Prelude hiding (readFile, writeFile)

data CompletionType
  = NamespaceCompletion
  | TermCompletion
  | TypeCompletion
  | PatchCompletion
  deriving (Show, Eq, Ord)

noCompletions ::
  Monad m =>
  String ->
  Codebase m v a ->
  Branch.Branch m ->
  Path.Absolute ->
  m [System.Console.Haskeline.Completion.Completion]
noCompletions _ _ _ _ = pure []

-- |
--
-- Finds names of the selected completion types in the current namespace.
-- The caller is responsible for filtering those names according to whatever strategy they
-- want, e.g. prefix matching, fuzzy finding, etc.
--
-- Given a codebase with these terms:
--
-- @@
-- .base.List.map.doc
-- .base.List
-- .bar.foo
-- @@
--
-- We will return:
--
-- @@
-- -- Note how we return `base` even if it's not a prefix match, it's the caller's job to
-- filter however they like.
-- .> cd bar<Tab>
-- bar
-- base
--
-- .> cd base<Tab>
-- base
-- base.List
--
-- .> cd base.<Tab>
-- base.List
--
-- .> cd base.List.<Tab>
-- base.List.|map
completeWithinNamespace ::
  forall m v a.
  Monad m =>
  (String -> [String] -> [System.Console.Haskeline.Completion.Completion]) ->
  -- | The types of completions to return
  NESet CompletionType ->
  -- | The portion of this are that the user has already typed.
  String ->
  Codebase m v a ->
  Branch.Branch m ->
  Path.Absolute ->
  m [System.Console.Haskeline.Completion.Completion]
completeWithinNamespace mkCompletions compTypes query codebase _root currentPath = do
  Codebase.getShallowBranchFromRoot codebase absQueryPath >>= \case
    Nothing -> do
      pure []
    Just cb -> do
      b <- V2Causal.value cb
      let currentBranchSuggestions =
            namesInBranch b
              <&> \match -> Text.unpack . Path.toText' $ queryPathPrefix Lens.:> NameSegment.NameSegment match
      childSuggestions <- getChildSuggestions b
      pure . mkCompletions query $ currentBranchSuggestions <> childSuggestions
  where
    fullQueryPath :: Path.Path'
    fullQueryPath = Path.fromText' (Text.pack query)
    queryPathPrefix :: Path.Path'
    -- The trailing segment of the query.
    -- E.g.
    -- .base.Li -> Just "Li"
    -- .base. -> Nothing
    -- .base -> Just "base"
    querySuffix :: Maybe NameSegment.NameSegment
    (queryPathPrefix, querySuffix) = case Lens.unsnoc fullQueryPath of
      Nothing ->
        if Path.isAbsolute fullQueryPath
          then (Path.AbsolutePath' Path.absoluteEmpty, Nothing)
          else (Path.RelativePath' Path.relativeEmpty, Nothing)
      Just (p, segment)
        -- The parser straight up ignores a trailing '.', so we correct for that here,
        -- E.g. if the query was ".base." we only want to see *children* of '.base'
        | List.isSuffixOf "." query -> (fullQueryPath, Nothing)
        | otherwise -> (p, Just segment)
    absQueryPath :: Path.Absolute
    absQueryPath = Path.resolve currentPath queryPathPrefix
    getChildSuggestions :: V2Branch.Branch m -> m [String]
    getChildSuggestions b = do
      case querySuffix of
        Nothing -> pure []
        Just suffix -> do
          case Map.lookup (Cv.namesegment1to2 suffix) (V2Branch.children b) of
            Nothing -> pure []
            Just childCausal -> do
              childBranch <- V2Causal.value childCausal
              namesInBranch childBranch
                & fmap
                  ( \match -> Text.unpack . Path.toText' $ queryPathPrefix Lens.:> suffix Lens.:> NameSegment.NameSegment match
                  )
                & pure
    namesInBranch :: V2Branch.Branch m -> [Text]
    namesInBranch b =
      V2Branch.unNameSegment
        <$> ( Monoid.whenM (NESet.member NamespaceCompletion compTypes) (Map.keys $ V2Branch.children b)
                <> Monoid.whenM (NESet.member TermCompletion compTypes) (Map.keys $ V2Branch.terms b)
                <> Monoid.whenM (NESet.member TypeCompletion compTypes) (Map.keys $ V2Branch.types b)
                <> Monoid.whenM (NESet.member PatchCompletion compTypes) (Map.keys $ V2Branch.patches b)
            )

prefixCompleteNamespace ::
  forall m v a.
  Monad m =>
  String ->
  Codebase m v a ->
  Branch.Branch m -> -- Root Branch
  Path.Absolute -> -- Current path
  m [Line.Completion]
prefixCompleteNamespace = completeWithinNamespace prefixCompletionFilter (NESet.singleton NamespaceCompletion)

prefixCompleteTermTypeOrNamespace ::
  forall m v a.
  Monad m =>
  String ->
  Codebase m v a ->
  Branch.Branch m -> -- Root Branch
  Path.Absolute -> -- Current path
  m [Line.Completion]
prefixCompleteTermTypeOrNamespace = completeWithinNamespace prefixCompletionFilter (NESet.fromList (TermCompletion NE.:| [TypeCompletion, NamespaceCompletion]))

prefixCompletionFilter :: String -> [String] -> [System.Console.Haskeline.Completion.Completion]
prefixCompletionFilter query completions =
  completions
    & filter (List.isPrefixOf query)
    & fmap (prettyCompletionWithQueryPrefix False query)

-- | Filters and sorts completions based on a fuzzy match over the final segment of the
-- query and the completions, e.g.:
fuzzySuffixSegmentCompletionFilter :: String -> [String] -> [System.Console.Haskeline.Completion.Completion]
fuzzySuffixSegmentCompletionFilter query fullCompletions =
  let (queryPrefix, querySuffix) = case (Lens.unsnoc . splitOn "." $ query) of
        Nothing -> ("", query)
        Just (prefix, querySeg) -> (intercalate "." prefix, querySeg)
      searchItems :: [(String, String)]
      searchItems =
        fullCompletions
          -- Assert that the path prefix matches before we bother with fuzzy search.
          & filter (List.isPrefixOf queryPrefix)
          & fmap
            ( \compl -> case (Lens.unsnoc . splitOn "." $ compl) of
                Nothing -> (compl, compl)
                Just (_, segment) -> (compl, segment)
            )
   in Fuzzy.simpleFuzzyFinder querySuffix searchItems snd
        & fmap (\((fullCompletion, _suffix), pretty) -> prettyCompletion False (fullCompletion, pretty))
        & fixupCompletion query

-- | Renders a completion option with the prefix matching the query greyed out.
prettyCompletionWithQueryPrefix ::
  Bool ->
  -- | query
  String ->
  -- | completion
  String ->
  Line.Completion
prettyCompletionWithQueryPrefix endWithSpace query s =
  let coloredMatch = P.hiBlack (P.string query) <> P.string (drop (length query) s)
   in Line.Completion s (P.toAnsiUnbroken coloredMatch) endWithSpace

-- discards formatting in favor of better alignment
-- prettyCompletion (s, p) = Line.Completion s (P.toPlainUnbroken p) True
-- preserves formatting, but Haskeline doesn't know how to align
prettyCompletion :: Bool -> (String, P.Pretty P.ColorText) -> Line.Completion
prettyCompletion endWithSpace (s, p) = Line.Completion s (P.toAnsiUnbroken p) endWithSpace

-- | Constructs a list of 'Completion's from a query and completion options by
-- filtering them for prefix matches. A completion will be selected if it's an exact match for
-- a provided option.
exactComplete :: String -> [String] -> [Line.Completion]
exactComplete q ss = go <$> filter (isPrefixOf q) ss
  where
    go s = prettyCompletionWithQueryPrefix (s == q) q s

-- workaround for https://github.com/judah/haskeline/issues/100
-- if the common prefix of all the completions is smaller than
-- the query, we make all the replacements equal to the query,
-- which will preserve what the user has typed
fixupCompletion :: String -> [Line.Completion] -> [Line.Completion]
fixupCompletion _q [] = []
fixupCompletion _q [c] = [c]
fixupCompletion q cs@(h : t) =
  let commonPrefix (h1 : t1) (h2 : t2) | h1 == h2 = h1 : commonPrefix t1 t2
      commonPrefix _ _ = ""
      overallCommonPrefix =
        foldl commonPrefix (Line.replacement h) (Line.replacement <$> t)
   in if not (q `isPrefixOf` overallCommonPrefix)
        then [c {Line.replacement = q} | c <- cs]
        else cs
