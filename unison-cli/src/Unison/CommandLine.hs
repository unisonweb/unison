{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.CommandLine
  ( -- * Pretty Printing
    allow,
    backtick,
    aside,
    bigproblem,
    note,
    nothingTodo,
    plural,
    plural',
    problem,
    tip,
    warn,
    warnNote,

    -- * Completers
    completion,
    completion',
    exactComplete,
    fuzzyComplete,
    fuzzyCompleteHashQualified,
    prefixIncomplete,
    prettyCompletion,
    prettyCompletionWithQueryPrefix,
    fixupCompletion,
    completeWithinQueryNamespace,

    -- * Other
    parseInput,
    prompt,
    watchConfig,
    watchFileSystem,
  )
where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM (atomically)
import Control.Lens (ifor)
import Data.Configurator (autoConfig, autoReload)
import qualified Data.Configurator as Config
import Data.Configurator.Types (Config, Worth (..))
import Data.List (isPrefixOf, isSuffixOf)
import qualified Data.List as List
import Data.List.Extra (nubOrd)
import Data.ListLike (ListLike)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified System.Console.Haskeline as Line
import System.FilePath (takeFileName)
import Text.Regex.TDFA ((=~))
import Unison.Codebase.Branch (Branch0)
import Unison.Codebase.Editor.Input (Event (..), Input (..))
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.Watch as Watch
import qualified Unison.CommandLine.Globbing as Globbing
import Unison.CommandLine.InputPattern (InputPattern (..))
import qualified Unison.CommandLine.InputPattern as InputPattern
import qualified Unison.HashQualified as HQ
import qualified Unison.HashQualified' as HQ'
import Unison.Names (Names)
import Unison.Prelude
import qualified Unison.Server.SearchResult as SR
import qualified Unison.Util.ColorText as CT
import qualified Unison.Util.Find as Find
import qualified Unison.Util.Pretty as P
import Unison.Util.TQueue (TQueue)
import qualified Unison.Util.TQueue as Q
import Prelude hiding (readFile, writeFile)

disableWatchConfig :: Bool
disableWatchConfig = False

allow :: FilePath -> Bool
allow p =
  -- ignore Emacs .# prefixed files, see https://github.com/unisonweb/unison/issues/457
  not (".#" `isPrefixOf` takeFileName p)
    && (isSuffixOf ".u" p || isSuffixOf ".uu" p)

watchConfig :: FilePath -> IO (Config, IO ())
watchConfig path =
  if disableWatchConfig
    then pure (Config.empty, pure ())
    else do
      (config, t) <- autoReload autoConfig [Optional path]
      pure (config, killThread t)

watchFileSystem :: TQueue Event -> FilePath -> IO (IO ())
watchFileSystem q dir = do
  (cancel, watcher) <- Watch.watchDirectory dir allow
  t <- forkIO . forever $ do
    (filePath, text) <- watcher
    atomically . Q.enqueue q $ UnisonFileChanged (Text.pack filePath) text
  pure (cancel >> killThread t)

warnNote :: String -> String
warnNote s = "⚠️  " <> s

backtick :: IsString s => P.Pretty s -> P.Pretty s
backtick s = P.group ("`" <> s <> "`")

tip :: (ListLike s Char, IsString s) => P.Pretty s -> P.Pretty s
tip s = P.column2 [("Tip:", P.wrap s)]

note :: (ListLike s Char, IsString s) => P.Pretty s -> P.Pretty s
note s = P.column2 [("Note:", P.wrap s)]

aside :: (ListLike s Char, IsString s) => P.Pretty s -> P.Pretty s -> P.Pretty s
aside a b = P.column2 [(a <> ":", b)]

warn :: (ListLike s Char, IsString s) => P.Pretty s -> P.Pretty s
warn = emojiNote "⚠️"

problem :: (ListLike s Char, IsString s) => P.Pretty s -> P.Pretty s
problem = emojiNote "❗️"

bigproblem :: (ListLike s Char, IsString s) => P.Pretty s -> P.Pretty s
bigproblem = emojiNote "‼️"

emojiNote :: (ListLike s Char, IsString s) => String -> P.Pretty s -> P.Pretty s
emojiNote lead s = P.group (fromString lead) <> "\n" <> P.wrap s

nothingTodo :: (ListLike s Char, IsString s) => P.Pretty s -> P.Pretty s
nothingTodo = emojiNote "😶"

completion :: String -> Line.Completion
completion s = Line.Completion s s True

completion' :: String -> Line.Completion
completion' s = Line.Completion s s False

-- discards formatting in favor of better alignment
-- prettyCompletion (s, p) = Line.Completion s (P.toPlainUnbroken p) True
-- preserves formatting, but Haskeline doesn't know how to align
prettyCompletion :: Bool -> (String, P.Pretty P.ColorText) -> Line.Completion
prettyCompletion endWithSpace (s, p) = Line.Completion s (P.toAnsiUnbroken p) endWithSpace

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

fuzzyCompleteHashQualified :: Names -> String -> [Line.Completion]
fuzzyCompleteHashQualified b q0@(HQ'.fromString -> query) = case query of
  Nothing -> []
  Just query ->
    fixupCompletion q0 $
      makeCompletion <$> Find.fuzzyFindInBranch b query
  where
    makeCompletion (sr, p) =
      prettyCompletion False (HQ.toString . SR.name $ sr, p)

fuzzyComplete :: String -> [String] -> [Line.Completion]
fuzzyComplete absQuery@('.' : _) ss = completeWithinQueryNamespace absQuery ss
fuzzyComplete fuzzyQuery ss =
  fixupCompletion fuzzyQuery (prettyCompletion False <$> Find.simpleFuzzyFinder fuzzyQuery ss id)

-- | Constructs a list of 'Completion's from a query and completion options by
-- filtering them for prefix matches. A completion will be selected if it's an exact match for
-- a provided option.
exactComplete :: String -> [String] -> [Line.Completion]
exactComplete q ss = go <$> filter (isPrefixOf q) ss
  where
    go s = prettyCompletionWithQueryPrefix (s == q) q s

-- | Completes a list of options, limiting options to the same namespace as the query,
-- or the namespace's children if the query is itself a namespace.
--
-- E.g.
-- query: "base"
-- would match: ["base", "base.List", "base2"]
-- wouldn't match: ["base.List.map", "contrib", "base2.List"]
completeWithinQueryNamespace :: String -> [String] -> [Line.Completion]
completeWithinQueryNamespace q ss = (go <$> (limitToQueryNamespace q $ ss))
  where
    go s = prettyCompletionWithQueryPrefix (s == q) q s
    limitToQueryNamespace :: String -> [String] -> [String]
    limitToQueryNamespace query xs =
      nubOrd $ catMaybes (fmap ((query <>) . thing) . List.stripPrefix query <$> xs)
      where
        thing ('.' : rest) = '.' : takeWhile (/= '.') rest
        thing other = takeWhile (/= '.') other

prefixIncomplete :: String -> [String] -> [Line.Completion]
prefixIncomplete q ss = go <$> filter (isPrefixOf q) ss
  where
    go s =
      prettyCompletion
        False
        (s, P.hiBlack (P.string q) <> P.string (drop (length q) s))

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

parseInput ::
  -- | Root branch, used to expand globs
  Branch0 m ->
  -- | Current path from root, used to expand globs
  Path.Absolute ->
  -- | Numbered arguments
  [String] ->
  -- | Input Pattern Map
  Map String InputPattern ->
  -- | command:arguments
  [String] ->
  Either (P.Pretty CT.ColorText) Input
parseInput rootBranch currentPath numberedArgs patterns segments = do
  case segments of
    [] -> Left ""
    command : args -> case Map.lookup command patterns of
      Just pat@(InputPattern {parse}) -> do
        let expandedNumbers :: [String]
            expandedNumbers = foldMap (expandNumber numberedArgs) args
        expandedGlobs <- ifor expandedNumbers $ \i arg -> do
          let targets = case InputPattern.argType pat i of
                Just argT -> InputPattern.globTargets argT
                Nothing -> mempty
          case Globbing.expandGlobs targets rootBranch currentPath arg of
            -- No globs encountered
            Nothing -> pure [arg]
            Just [] -> Left $ "No matches for: " <> fromString arg
            Just matches -> pure matches
        parse (concat expandedGlobs)
      Nothing ->
        Left
          . warn
          . P.wrap
          $ "I don't know how to "
            <> P.group (fromString command <> ".")
            <> "Type `help` or `?` to get help."

-- Expand a numeric argument like `1` or a range like `3-9`
expandNumber :: [String] -> String -> [String]
expandNumber numberedArgs s =
  maybe
    [s]
    (map (\i -> fromMaybe (show i) . atMay numberedArgs $ i - 1))
    expandedNumber
  where
    rangeRegex = "([0-9]+)-([0-9]+)" :: String
    (junk, _, moreJunk, ns) =
      s =~ rangeRegex :: (String, String, String, [String])
    expandedNumber =
      case readMay s of
        Just i -> Just [i]
        Nothing ->
          -- check for a range
          case (junk, moreJunk, ns) of
            ("", "", [from, to]) ->
              (\x y -> [x .. y]) <$> readMay from <*> readMay to
            _ -> Nothing

prompt :: String
prompt = "> "

-- `plural [] "cat" "cats" = "cats"`
-- `plural ["meow"] "cat" "cats" = "cat"`
-- `plural ["meow", "meow"] "cat" "cats" = "cats"`
plural :: Foldable f => f a -> b -> b -> b
plural items one other = case toList items of
  [_] -> one
  _ -> other

plural' :: Integral a => a -> b -> b -> b
plural' 1 one _other = one
plural' _ _one other = other
