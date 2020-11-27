{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.CommandLine where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM (atomically)
import qualified Control.Monad.Extra as Monad
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State as State
import Data.Configurator (autoConfig, autoReload)
import Data.Configurator.Types (Config, Worth (..))
import Data.List (isPrefixOf, isSuffixOf)
import Data.ListLike (ListLike)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified System.Console.Haskeline as Line
import System.FilePath (takeFileName)
import Unison.Codebase (Codebase)
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.Branch as Branch
import Unison.Codebase.Causal (Causal)
import qualified Unison.Codebase.Causal as Causal
import Unison.Codebase.Editor.Input (Event (..), Input (..))
import qualified Unison.Codebase.SearchResult as SR
import qualified Unison.Codebase.Watch as Watch
import Unison.CommandLine.InputPattern (InputPattern (parse))
import qualified Unison.HashQualified' as HQ
import Unison.Names2 (Names0)
import Unison.Prelude
import qualified Unison.Util.ColorText as CT
import qualified Unison.Util.Find as Find
import qualified Unison.Util.Pretty as P
import Unison.Util.TQueue (TQueue)
import qualified Unison.Util.TQueue as Q
import Prelude hiding (readFile, writeFile)

allow :: FilePath -> Bool
allow p =
  -- ignore Emacs .# prefixed files, see https://github.com/unisonweb/unison/issues/457
  not (".#" `isPrefixOf` takeFileName p)
    && (isSuffixOf ".u" p || isSuffixOf ".uu" p)

watchConfig :: FilePath -> IO (Config, IO ())
watchConfig path = do
  (config, t) <- autoReload autoConfig [Optional path]
  pure (config, killThread t)

watchFileSystem :: TQueue Event -> FilePath -> IO (IO ())
watchFileSystem q dir = do
  (cancel, watcher) <- Watch.watchDirectory dir allow
  t <- forkIO . forever $ do
    (filePath, text) <- watcher
    atomically . Q.enqueue q $ UnisonFileChanged (Text.pack filePath) text
  pure (cancel >> killThread t)

watchBranchUpdates :: IO (Branch.Branch IO) -> TQueue Event -> Codebase IO v a -> IO (IO ())
watchBranchUpdates currentRoot q codebase = do
  (cancelExternalBranchUpdates, externalBranchUpdates) <-
    Codebase.rootBranchUpdates codebase
  thread <- forkIO . forever $ do
    updatedBranches <- externalBranchUpdates
    currentRoot <- currentRoot
    -- Since there's some lag between when branch files are written and when
    -- the OS generates a file watch event, we skip branch update events
    -- that are causally before the current root.
    --
    -- NB: Sadly, since the file watching API doesn't have a way to silence
    -- the events from a specific individual write, this is ultimately a
    -- heuristic. If a fairly recent head gets deposited at just the right
    -- time, it would get ignored by this logic. This seems unavoidable.
    let maxDepth = 20 -- if it's further back than this, consider it new
    let isNew b = not <$> beforeHash maxDepth b (Branch._history currentRoot)
    notBefore <- filterM isNew (toList updatedBranches)
    when (length notBefore > 0) $
      atomically . Q.enqueue q . IncomingRootBranch $ Set.fromList notBefore
  pure (cancelExternalBranchUpdates >> killThread thread)

-- `True` if `h` is found in the history of `c` within `maxDepth` path length
-- from the tip of `c`
beforeHash :: forall m h e. Monad m => Word -> Causal.RawHash h -> Causal m h e -> m Bool
beforeHash maxDepth h c =
  Reader.runReaderT (State.evalStateT (go c) Set.empty) (0 :: Word)
  where
    go c | h == Causal.currentHash c = pure True
    go c = do
      currentDepth :: Word <- Reader.ask
      if currentDepth >= maxDepth
        then pure False
        else do
          seen <- State.get
          cs <- lift . lift $ toList <$> sequence (Causal.children c)
          let unseens = filter (\c -> c `Set.notMember` seen) cs
          State.modify' (<> Set.fromList cs)
          Monad.anyM (Reader.local (1 +) . go) unseens

warnNote :: String -> String
warnNote s = "⚠️  " <> s

backtick :: IsString s => P.Pretty s -> P.Pretty s
backtick s = P.group ("`" <> s <> "`")

backtickEOS :: IsString s => P.Pretty s -> P.Pretty s
backtickEOS s = P.group ("`" <> s <> "`.")

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

prettyCompletion :: (String, P.Pretty P.ColorText) -> Line.Completion
-- -- discards formatting in favor of better alignment
-- prettyCompletion (s, p) = Line.Completion s (P.toPlainUnbroken p) True
-- preserves formatting, but Haskeline doesn't know how to align
prettyCompletion (s, p) = Line.Completion s (P.toAnsiUnbroken p) True

-- avoids adding a space after successful completion
prettyCompletion' :: (String, P.Pretty P.ColorText) -> Line.Completion
prettyCompletion' (s, p) = Line.Completion s (P.toAnsiUnbroken p) False

prettyCompletion'' :: Bool -> (String, P.Pretty P.ColorText) -> Line.Completion
prettyCompletion'' spaceAtEnd (s, p) = Line.Completion s (P.toAnsiUnbroken p) spaceAtEnd

fuzzyCompleteHashQualified :: Names0 -> String -> [Line.Completion]
fuzzyCompleteHashQualified b q0@(HQ.fromString -> query) = case query of
  Nothing -> []
  Just query ->
    fixupCompletion q0 $
      makeCompletion <$> Find.fuzzyFindInBranch b query
  where
    makeCompletion (sr, p) =
      prettyCompletion' (HQ.toString . SR.name $ sr, p)

fuzzyComplete :: String -> [String] -> [Line.Completion]
fuzzyComplete q ss =
  fixupCompletion q (prettyCompletion' <$> Find.simpleFuzzyFinder q ss id)

exactComplete :: String -> [String] -> [Line.Completion]
exactComplete q ss = go <$> filter (isPrefixOf q) ss
  where
    go s =
      prettyCompletion''
        (s == q)
        (s, P.hiBlack (P.string q) <> P.string (drop (length q) s))

prefixIncomplete :: String -> [String] -> [Line.Completion]
prefixIncomplete q ss = go <$> filter (isPrefixOf q) ss
  where
    go s =
      prettyCompletion''
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
  Map String InputPattern -> [String] -> Either (P.Pretty CT.ColorText) Input
parseInput patterns ss = case ss of
  [] -> Left ""
  command : args -> case Map.lookup command patterns of
    Just pat -> parse pat args
    Nothing ->
      Left
        . warn
        . P.wrap
        $ "I don't know how to "
          <> P.group (fromString command <> ".")
          <> "Type `help` or `?` to get help."

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
