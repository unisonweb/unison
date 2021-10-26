{-# LANGUAGE OverloadedStrings #-}
module Unison.CommandLine.FuzzySelect
  ( fuzzySelect
  , Options(..)
  ) where
import Unison.Prelude
import qualified UnliftIO.Process as Proc
import UnliftIO.Exception (bracket_)
import UnliftIO.IO (hSetBuffering, stdin)
import System.IO (BufferMode (NoBuffering))
import GHC.IO.Handle (hDuplicateTo)
import qualified Data.Text.IO as Text
import qualified Data.Text as Text
import qualified Data.Set as Set

data Options = Options
  { allowMultiSelect :: Bool
  }

defaultFZFCommand :: String
defaultFZFCommand =
  unwords
  [ "fzf"
  , "--with-nth=2.." -- Don't show or match on the first column of input. We use this to identify results.
  ]

optsToCommand :: Options -> String
optsToCommand opts = defaultFZFCommand <> case opts of
  Options {allowMultiSelect=True} -> "-m"
  _ -> ""

withTempScreen :: IO a -> IO a
withTempScreen =
  bracket_
    (Proc.callCommand "tput smcup") -- Stash existing screen, create a new one
    (Proc.callCommand "tput rmcup") -- Delete the temporary screen, restore the original.

fuzzySelect :: forall a. Options -> (a -> Text) -> [a] -> IO [a]
fuzzySelect opts intoSearchText choices = withTempScreen $ do
  let command = optsToCommand opts
  let numberedChoices :: [(Int, a)]
        = zip  [0..] choices
  let searchTexts :: [Text]
        = (\(n, ch) -> tShow (n) <> " " <> intoSearchText ch) <$> numberedChoices
  (Just stdin', Just stdout', _, ph) <- Proc.createProcess ((Proc.shell command){Proc.std_in=Proc.CreatePipe, Proc.std_out=Proc.CreatePipe})
  hSetBuffering stdin NoBuffering
  hSetBuffering stdin' NoBuffering
  hSetBuffering stdout' NoBuffering
  traverse (Text.hPutStrLn stdin') searchTexts
  hDuplicateTo stdin stdin'
  Proc.waitForProcess ph
  selections <- Text.lines <$> Text.hGetContents stdout'
  let selectedNumbers = selections
                      & fmap (readMaybe @Int . Text.unpack . Text.takeWhile (/= ' '))
                      & catMaybes
                      & Set.fromList
  pure $ mapMaybe (\(n, a) -> if n `Set.member` selectedNumbers then Just a else Nothing) numberedChoices
