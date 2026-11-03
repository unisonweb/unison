-- | Delivering what a transcript tool has to say. Long output goes to a file, so that the client can search it
-- instead of having all of it in its context.
module Unison.MCP.Output
  ( OutputDir,
    withOutputDir,
    deliver,
    maxInlineLines,
    captureHandles,
  )
where

import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.Directory (getTemporaryDirectory, removePathForcibly)
import System.IO qualified as IO
import System.IO.Temp (createTempDirectory, withSystemTempFile)
import Unison.Prelude
import UnliftIO qualified

-- | A directory, private to this server process, for output that is too long to return directly.
newtype OutputDir = OutputDir FilePath

-- | The directory is removed when the action finishes.
withOutputDir :: (OutputDir -> IO a) -> IO a
withOutputDir action =
  UnliftIO.bracket
    (getTemporaryDirectory >>= \tmp -> OutputDir <$> createTempDirectory tmp "unison-mcp")
    (\(OutputDir dir) -> removePathForcibly dir)
    action

-- | The most lines a response may have, not counting blank ones; anything longer is written to a file.
maxInlineLines :: Int
maxInlineLines = 75

-- | The text to return for some output: the output itself, or if it is long, where it was written.
deliver :: OutputDir -> Text -> IO Text
deliver (OutputDir dir) output
  | length (filter (not . Text.null . Text.strip) outputLines) <= maxInlineLines = pure output
  | otherwise = do
      (path, handle) <- IO.openTempFile dir "output-.md"
      IO.hSetEncoding handle IO.utf8
      Text.hPutStr handle output
      IO.hClose handle
      pure $
        "The output is "
          <> tShow lineCount
          <> " lines long, so it was written to "
          <> Text.pack path
          <> ". Read or grep that file for what you need."
  where
    outputLines = Text.lines output
    lineCount = length outputLines

-- | Capture stdout and stderr for the duration of the action, since the MCP server talks over stdout.
captureHandles :: IO a -> IO (Text, Text, a)
captureHandles action =
  withSystemTempFile "stdout.txt" \stdoutPath stdoutHandle ->
    withSystemTempFile "stderr.txt" \stderrPath stderrHandle -> do
      a <-
        UnliftIO.bracket
          ( do
              oldStdout <- hDuplicate IO.stdout
              hDuplicateTo stdoutHandle IO.stdout
              oldStderr <- hDuplicate IO.stderr
              hDuplicateTo stderrHandle IO.stderr
              pure (oldStdout, oldStderr)
          )
          ( \(oldStdout, oldStderr) -> do
              hDuplicateTo oldStdout IO.stdout
              IO.hClose oldStdout
              hDuplicateTo oldStderr IO.stderr
              IO.hClose oldStderr
          )
          (\_ -> action)
      IO.hClose stdoutHandle
      output <- Text.readFile stdoutPath
      IO.hClose stderrHandle
      errOutput <- Text.readFile stderrPath
      pure (output, errOutput, a)
