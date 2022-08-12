{-# LANGUAGE OverloadedStrings #-}

-- | Command-line fuzzy selection of arbitrary values.
--     Shells out to fzf for the actual selection.
module Unison.CommandLine.FuzzySelect
  ( fuzzySelect,
    Options (..),
    defaultOptions,
  )
where

import Control.Monad.Except (runExceptT, throwError)
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.IO (BufferMode (NoBuffering), hPutStrLn, stderr)
import Unison.Prelude
import qualified UnliftIO
import qualified UnliftIO.Async as Async
import UnliftIO.Directory (findExecutable)
import UnliftIO.Exception (bracket)
import UnliftIO.IO (hGetBuffering, hSetBuffering, stdin)
import qualified UnliftIO.Process as Proc
import UnliftIO.STM

-- | Fuzzy Selection options
data Options = Options
  { allowMultiSelect :: Bool
  }

-- | Default 'Options'
defaultOptions :: Options
defaultOptions =
  Options
    { allowMultiSelect = True
    }

-- | Convert options into command-line args for fzf
optsToArgs :: Options -> [String]
optsToArgs opts =
  defaultArgs <> case opts of
    Options {allowMultiSelect = True} -> ["-m"]
    _ -> []
  where
    defaultArgs =
      -- Don't show or match on the first column of input.
      -- This allows us to prepend each line with a number, and use that number to determine
      -- which values from the input list were selected.
      [ "--with-nth",
        "2..",
        -- Use only half the screen (it's nice to see what you were working on when searching)
        "--height=50%",
        -- But if 50% of the screen is too small, ensure show at least 10 results.
        "--min-height=10"
      ]

-- | Allows prompting the user to interactively fuzzy-select a result from a list of options, currently shells out to `fzf` under the hood.
-- If fzf is missing, or an error (other than ctrl-c) occurred, returns Nothing.
fuzzySelect :: forall a. Options -> (a -> Text) -> ((a -> IO ()) -> IO ()) -> IO (Maybe [a])
fuzzySelect opts intoSearchText streamHandler =
  UnliftIO.handleAny handleException
    . handleError
    . restoreBuffering
    . runExceptT
    $ do
      fzfPath <-
        liftIO (findExecutable "fzf") >>= \case
          Nothing -> throwError "I couldn't find the `fzf` executable on your path, consider installing `fzf` to enable fuzzy searching."
          Just fzfPath -> pure fzfPath
      numberedLinesVar <- newTVarIO (mempty, 0)
      let fzfArgs :: [String] =
            optsToArgs opts
      let mkSearchText :: Int -> a -> Text
          mkSearchText n ch = tShow (n) <> " " <> intoSearchText ch
      let fzfProc :: Proc.CreateProcess =
            (Proc.proc fzfPath fzfArgs)
              { Proc.std_in = Proc.CreatePipe,
                Proc.std_out = Proc.CreatePipe,
                Proc.delegate_ctlc = True
              }
      (Just stdin', Just stdout', _, procHandle) <- Proc.createProcess fzfProc
      let emitInputRow a = do
            n <- atomically $ do
              (as, count) <- readTVar numberedLinesVar
              writeTVar numberedLinesVar $! (IntMap.insert count a as, count + 1)
              pure count
            Text.hPutStrLn stdin' (mkSearchText n a)
      -- Generally no-buffering is helpful for highly interactive processes.
      hSetBuffering stdin NoBuffering
      hSetBuffering stdin' NoBuffering
      result <- liftIO . UnliftIO.tryAny $ do
        -- Defer to the caller to stream input.
        void $ Async.race (Proc.waitForProcess procHandle) (streamHandler emitInputRow)
        -- Wire up the interactive terminal to fzf now that the inputs have been loaded.
        -- hDuplicateTo stdin stdin'
        void $ Proc.waitForProcess procHandle
        Text.lines <$> liftIO (Text.hGetContents stdout')
      -- Ignore any errors from fzf, or from trying to write to pipes which may have been
      -- closed by a ctrl-c, just treat it as an empty selection.
      let selections = fromRight [] result
      -- Since we prefixed every search term with its number earlier, we know each result
      -- is prefixed with a number, we need to parse it and use it to select the matching
      -- value from our input list.
      let selectedNumbers =
            selections
              & mapMaybe (readMaybe @Int . Text.unpack . Text.takeWhile (/= ' '))
              & Set.fromList
      (as, _) <- readTVarIO numberedLinesVar
      pure $ mapMaybe (\n -> IntMap.lookup n as) (toList selectedNumbers)
  where
    handleException :: SomeException -> IO (Maybe [a])
    handleException err = traceShowM err *> hPutStrLn stderr "Oops, something went wrong. No input selected." *> pure Nothing
    handleError :: IO (Either Text [a]) -> IO (Maybe [a])
    handleError m =
      m >>= \case
        Left err -> Text.hPutStrLn stderr err *> pure Nothing
        Right as -> pure (Just as)
    restoreBuffering :: IO c -> IO c
    restoreBuffering action =
      bracket (hGetBuffering stdin) (hSetBuffering stdin) (const action)
