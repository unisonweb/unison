{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# LANGUAGE OverloadedStrings #-}

{- | Command-line fuzzy selection of arbitrary values.
     Shells out to fzf for the actual selection.
-}
module Unison.CommandLine.FuzzySelect
  ( fuzzySelect
  , Options(..)
  , defaultOptions
  ) where
import Unison.Prelude
import qualified UnliftIO.Process as Proc
import UnliftIO.Exception (bracket_, bracket)
import UnliftIO.IO (hSetBuffering, stdin, hGetBuffering)
import System.IO (BufferMode (NoBuffering), hPutStrLn, stderr)
import GHC.IO.Handle (hDuplicateTo)
import qualified Data.Text.IO as Text
import qualified Data.Text as Text
import qualified Data.Set as Set
import UnliftIO (handleAny)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import Control.Monad.Except (runExceptT, throwError)
import UnliftIO.Directory (findExecutable)

-- | Fuzzy Selection options
data Options = Options
  { allowMultiSelect :: Bool
  }

-- | Default 'Options'
defaultOptions :: Options
defaultOptions = Options
  { allowMultiSelect = True
  }

-- | Convert options into command-line args for fzf
optsToArgs :: Options -> [String]
optsToArgs opts = defaultArgs <> case opts of
    Options {allowMultiSelect=True} -> ["-m"]
    _ -> []
  where
    defaultArgs =
      -- Don't show or match on the first column of input.
      -- This allows us to prepend each line with a number, and use that number to determine
      -- which values from the input list were selected.
      [ "--with-nth", "2.."
      ]

-- | Run the given IO block within a fresh terminal screen, clean it up and restore the
-- previous screen after the block is finished.
withTempScreen :: IO a -> IO a
withTempScreen =
  bracket_
    (Proc.callCommand "tput smcup") -- Stash existing screen, create a new one
    (Proc.callCommand "tput rmcup") -- Delete the temporary screen, restore the original.


-- | Allows prompting the user to interactively fuzzy-select a result from a list of options, currently shells out to `fzf` under the hood.
-- If fzf is missing, or an error (other than ctrl-c) occurred, returns Nothing.
fuzzySelect :: forall a. Options -> (a -> Text) -> [a] -> IO (Maybe [a])
fuzzySelect opts intoSearchText choices =
  handleAny handleException
    . handleError
    . withTempScreen
    . restoreBuffering
    . runExceptT $ do
    fzfPath <- liftIO (findExecutable "fzf") >>= \case
      Nothing -> throwError "I couldn't find the `fzf` executable on your path, consider installing `fzf` to enable fuzzy searching."
      Just fzfPath -> pure fzfPath
    let fzfArgs :: [String]
          = optsToArgs opts
    let numberedChoices :: [(Int, a)]
          = zip  [0..] choices
    let searchTexts :: [Text]
          = (\(n, ch) -> tShow (n) <> " " <> intoSearchText ch) <$> numberedChoices
    let fzfProc :: Proc.CreateProcess
          = (Proc.proc fzfPath fzfArgs)
              { Proc.std_in=Proc.CreatePipe
              , Proc.std_out=Proc.CreatePipe
              , Proc.delegate_ctlc=True
              }
    (Just stdin', Just stdout', _, procHandle) <- Proc.createProcess fzfProc
    -- Generally no-buffering is helpful for highly interactive processes.
    hSetBuffering stdin NoBuffering
    hSetBuffering stdin' NoBuffering
    -- Dump the search terms into fzf's stdin
    liftIO $ traverse (Text.hPutStrLn stdin') searchTexts
    -- Wire up the interactive terminal to fzf now that the inputs have been loaded.
    liftIO $ hDuplicateTo stdin stdin'
    exitCode <- Proc.waitForProcess procHandle
    case exitCode of
      ExitSuccess -> pure ()
      -- Thrown on ctrl-c in fzf
      ExitFailure 130 -> pure () -- output handle will be empty and no results will be returned.
      ExitFailure _ -> throwError "Oops, something went wrong. No input selected."
    selections <- Text.lines <$> liftIO (Text.hGetContents stdout')
    -- Since we prefixed every search term with its number earlier, we know each result
    -- is prefixed with a number, we need to parse it and use it to select the matching
    -- value from our input list.
    let selectedNumbers = selections
                        & mapMaybe (readMaybe @Int . Text.unpack . Text.takeWhile (/= ' '))
                        & Set.fromList
    pure $ mapMaybe (\(n, a) -> if n `Set.member` selectedNumbers then Just a else Nothing) numberedChoices
  where
    handleException :: SomeException -> IO (Maybe [a])
    handleException _ = hPutStrLn stderr "Oops, something went wrong. No input selected." *> pure Nothing
    handleError :: IO (Either Text [a]) -> IO (Maybe [a])
    handleError m = m >>= \case
      Left err -> Text.hPutStrLn stderr err *> pure Nothing
      Right as -> pure (Just as)
    restoreBuffering :: IO c -> IO c
    restoreBuffering action =
      bracket (hGetBuffering stdin) (hSetBuffering stdin) (const action)
