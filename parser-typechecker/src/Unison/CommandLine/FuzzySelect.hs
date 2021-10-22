module Unison.CommandLine.FuzzySelect where
import Unison.Prelude
import qualified UnliftIO.Process as Proc
import UnliftIO.Exception (bracket_)
import UnliftIO.IO (hSetBuffering, stdin)
import System.IO (BufferMode (NoBuffering))
import GHC.IO.Handle (hDuplicateTo)
import qualified Data.Text.IO as Text
import qualified Data.Text as Text

withTempScreen :: IO a -> IO a
withTempScreen =
  bracket_
    (Proc.callCommand "tput smcup") -- Stash existing screen, create a new one
    (Proc.callCommand "tput rmcup") -- Delete the temporary screen, restore the original.

fuzzySelect :: [Text] -> IO [Text]
fuzzySelect choices = withTempScreen $ do
  (Just stdin', Just stdout', _, ph) <- Proc.createProcess ((Proc.shell "fzf -m"){Proc.std_in=Proc.CreatePipe, Proc.std_out=Proc.CreatePipe})
  hSetBuffering stdin NoBuffering
  hSetBuffering stdin' NoBuffering
  hSetBuffering stdout' NoBuffering
  traverse (Text.hPutStrLn stdin') choices
  hDuplicateTo stdin stdin'
  Proc.waitForProcess ph
  Text.lines <$> Text.hGetContents stdout'
