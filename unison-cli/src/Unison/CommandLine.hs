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

    -- * Other
    parseInput,
    prompt,
    watchConfig,
    watchFileSystem,
  )
where

import Control.Concurrent (forkIO, killThread)
import Control.Lens (ifor)
import Control.Monad.Trans.Except
import Data.Configurator (autoConfig, autoReload)
import Data.Configurator qualified as Config
import Data.Configurator.Types (Config, Worth (..))
import Data.List (isPrefixOf, isSuffixOf)
import Data.ListLike (ListLike)
import Data.Map qualified as Map
import Data.Semialign qualified as Align
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.These (These (..))
import Data.Vector qualified as Vector
import System.FilePath (takeFileName)
import Text.Regex.TDFA ((=~))
import Unison.Codebase.Branch (Branch0)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Editor.Input (Event (..), Input (..))
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.Watch qualified as Watch
import Unison.CommandLine.Globbing qualified as Globbing
import Unison.CommandLine.InputPattern (InputPattern (..))
import Unison.CommandLine.InputPattern qualified as InputPattern
import Unison.Prelude
import Unison.Util.ColorText qualified as CT
import Unison.Util.Monoid (foldMapM)
import Unison.Util.Pretty qualified as P
import Unison.Util.TQueue qualified as Q
import UnliftIO.STM
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

watchFileSystem :: Q.TQueue Event -> FilePath -> IO (IO ())
watchFileSystem q dir = do
  (cancel, watcher) <- Watch.watchDirectory dir allow
  t <- forkIO . forever $ do
    (filePath, text) <- watcher
    atomically . Q.enqueue q $ UnisonFileChanged (Text.pack filePath) text
  pure (cancel >> killThread t)

warnNote :: String -> String
warnNote s = "⚠️  " <> s

backtick :: (IsString s) => P.Pretty s -> P.Pretty s
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

parseInput ::
  IO (Branch0 IO) ->
  -- | Current path from root, used to expand globs
  Path.Absolute ->
  -- | Numbered arguments
  [String] ->
  -- | Input Pattern Map
  Map String InputPattern ->
  -- | command:arguments
  [String] ->
  IO (Either (P.Pretty CT.ColorText) Input)
parseInput getRoot currentPath numberedArgs patterns segments = runExceptT do
  let getCurrentBranch0 :: IO (Branch0 IO)
      getCurrentBranch0 = do
        rootBranch <- getRoot
        pure $ Branch.getAt0 (Path.unabsolute currentPath) rootBranch

  case segments of
    [] -> throwE ""
    command : args -> case Map.lookup command patterns of
      Just pat@(InputPattern {parse}) -> do
        let expandedNumbers :: [String]
            expandedNumbers =
              foldMap (expandNumber numberedArgs) args
        expandedGlobs <- ifor expandedNumbers $ \i arg -> do
          if Globbing.containsGlob arg
            then do
              rootBranch <- liftIO getRoot
              let targets = case InputPattern.argType pat i of
                    Just argT -> InputPattern.globTargets argT
                    Nothing -> mempty
              case Globbing.expandGlobs targets rootBranch currentPath arg of
                -- No globs encountered
                Nothing -> pure [arg]
                Just [] -> throwE $ "No matches for: " <> fromString arg
                Just matches -> pure matches
            else pure [arg]
        resolvedArgs <- lift $ fzfResolve getCurrentBranch0 pat (concat expandedGlobs)
        except $ parse resolvedArgs
      Nothing ->
        throwE
          . warn
          . P.wrap
          $ "I don't know how to "
            <> P.group (fromString command <> ".")
            <> "Type `help` or `?` to get help."

-- Expand a numeric argument like `1` or a range like `3-9`
expandNumber :: [String] -> String -> [String]
expandNumber numberedArgs s = case expandedNumber of
  Nothing -> [s]
  Just nums ->
    [s | i <- nums, Just s <- [vargs Vector.!? (i - 1)]]
  where
    vargs = Vector.fromList numberedArgs
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

fzfResolve :: (IO (Branch0 IO)) -> InputPattern -> [String] -> IO [String]
fzfResolve getCurrentBranch pat args =
  (Align.align (argTypes pat) args) & foldMapM \case
    This (opt, argType) ->
      fromMaybe [] <$> runMaybeT do
        InputPattern.FZFResolver {argDescription, search} <- hoistMaybe $ InputPattern.fzfResolver argType
        guard (opt `elem` [InputPattern.Required, InputPattern.OnePlus])
        liftIO $ Text.putStrLn $ argDescription
        currentBranch <- liftIO getCurrentBranch
        MaybeT . fmap (Just . fmap Text.unpack) $ search currentBranch
    That arg -> pure [arg]
    These _ arg -> pure [arg]

prompt :: String
prompt = "> "

-- `plural [] "cat" "cats" = "cats"`
-- `plural ["meow"] "cat" "cats" = "cat"`
-- `plural ["meow", "meow"] "cat" "cats" = "cats"`
plural :: (Foldable f) => f a -> b -> b -> b
plural items one other = case toList items of
  [_] -> one
  _ -> other

plural' :: (Integral a) => a -> b -> b -> b
plural' 1 one _other = one
plural' _ _one other = other
