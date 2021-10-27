{-# LANGUAGE OverloadedStrings #-}
module Unison.CommandLine.FuzzySelect
  ( fuzzySelect
  , Options(..)
  , defaultOptions
  ) where
import Unison.Prelude
import qualified UnliftIO.Process as Proc
import UnliftIO.Exception (bracket_)
import UnliftIO.IO (hSetBuffering, stdin)
import System.IO (BufferMode (NoBuffering), hPutStrLn, stderr)
import GHC.IO.Handle (hDuplicateTo)
import qualified Data.Text.IO as Text
import qualified Data.Text as Text
import qualified Data.Set as Set
import UnliftIO (handleAny)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import Control.Monad.Except (runExceptT, throwError)
import UnliftIO.Directory (findExecutable)

data Options = Options
  { allowMultiSelect :: Bool
  }

defaultOptions :: Options
defaultOptions = Options
  { allowMultiSelect = True
  }

optsToArgs :: Options -> [String]
optsToArgs opts = defaultArgs <> case opts of
    Options {allowMultiSelect=True} -> [" -m"]
    _ -> []
  where
    defaultArgs =
      -- Don't show or match on the first column of input.
      -- This allows us to prepend each line with a number, and use that number to determine
      -- which values from the input list were selected.
      [ "--with-nth 2.."
      ]

withTempScreen :: IO a -> IO a
withTempScreen =
  bracket_
    (Proc.callCommand "tput smcup") -- Stash existing screen, create a new one
    (Proc.callCommand "tput rmcup") -- Delete the temporary screen, restore the original.


-- | Allows prompting the user to interactively fuzzy-select a result from a list of options, currently shells out to `fzf` under the hood.
-- If fzf is missing, or an error (other than ctrl-c) occurred, returns Nothing.
fuzzySelect :: forall a. Options -> (a -> Text) -> [a] -> IO (Maybe [a])
fuzzySelect opts intoSearchText choices = handleAny handler . handleError $ withTempScreen .  runExceptT $ do
    fzfPath <- liftIO (findExecutable "fzf404") >>= \case
      Nothing -> throwError "Consider installing fzf to improve your experience with unison."
      Just fzfPath -> pure fzfPath
    let fzfArgs :: [String]
          = optsToArgs opts
    let numberedChoices :: [(Int, a)]
          = zip  [0..] choices
    let searchTexts :: [Text]
          = (\(n, ch) -> tShow (n) <> " " <> intoSearchText ch) <$> numberedChoices
    let fzfProc :: Proc.CreateProcess
          = (Proc.proc fzfPath fzfArgs){Proc.std_in=Proc.CreatePipe, Proc.std_out=Proc.CreatePipe}
    (Just stdin', Just stdout', _, procHandle) <- Proc.createProcess fzfProc
    hSetBuffering stdin NoBuffering
    hSetBuffering stdin' NoBuffering
    liftIO $ traverse (Text.hPutStrLn stdin') searchTexts
    liftIO $ hDuplicateTo stdin stdin'
    exitCode <- Proc.waitForProcess procHandle
    case exitCode of
      ExitSuccess -> pure ()
      -- Thrown on ctrl-c in fzf
      ExitFailure 130 -> pure () -- output handle will be empty and no results will be returned.
      ExitFailure _ -> throwError "Oops, something went wrong. No input selected."
    selections <- Text.lines <$> liftIO (Text.hGetContents stdout')
    let selectedNumbers = selections
                        & fmap (readMaybe @Int . Text.unpack . Text.takeWhile (/= ' '))
                        & catMaybes
                        & Set.fromList
    pure $ mapMaybe (\(n, a) -> if n `Set.member` selectedNumbers then Just a else Nothing) numberedChoices
  where
    -- If an error occurrs during fuzzy-finding we just return zero results
    -- Since it's entirely possible the user just hit ctrl-c.
    handler :: SomeException -> IO (Maybe [a])
    handler _ = hPutStrLn stderr "Oops, something went wrong. No input selected." *> pure Nothing
    handleError :: IO (Either Text [a]) -> IO (Maybe [a])
    handleError m = m >>= \case
      Left err -> Text.hPutStrLn stderr err *> pure Nothing
      Right as -> pure (Just as)
