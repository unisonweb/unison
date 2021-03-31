{-# Language OverloadedStrings #-}
{-# Language QuasiQuotes #-}

module Unison.Test.IO where

import Unison.Prelude
import EasyTest
import qualified System.IO.Temp as Temp
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Shellmet ()
import Data.String.Here (iTrim)
import System.FilePath ((</>))
import System.Directory (removeDirectoryRecursive)

import Unison.Codebase (Codebase, CodebasePath)
import qualified Unison.Codebase.TranscriptParser as TR
import Unison.Parser (Ann)
import Unison.Symbol (Symbol)
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.Init as Codebase
import qualified Unison.Codebase.FileCodebase as FileCodebase

-- * IO Tests

test :: Test ()
test = scope "IO" . tests $ [ testHandleOps ]

-- * Implementation

-- | Test reading from and writing to a handle
--
-- The transcript writes expectedText to a file, reads the same file and
-- writes the read text to the result file which is then checked by the haskell.
testHandleOps :: Test ()
testHandleOps =
  withScopeAndTempDir "handleOps" $ \workdir codebase -> do
  let myFile = workdir </> "handleOps.txt"
      resultFile = workdir </> "handleOps.result"
      expectedText = "Good Job!" :: Text.Text
  runTranscript_ False workdir codebase [iTrim|
```ucm:hide
.> builtins.mergeio
```

```unison
use io IO

main : '{IO} ()
main = 'let
  fp = ${Text.pack myFile}
  res = ${Text.pack resultFile}
  expected = ${expectedText}

  -- Write to myFile
  h1 = io.openFile (FilePath fp) Write
  putText h1 expected
  io.closeFile h1

  -- Read from myFile
  h2 = builtin.io.openFile (FilePath fp) Read
  myC = getText h2
  io.closeFile h2

  -- Write what we read from myFile to resultFile
  h3 = io.openFile (FilePath res) Write
  putText h3 myC
  builtin.io.closeFile h3
```

```ucm
.> run main
```
|]

  res <- io $ TextIO.readFile (resultFile)
  if res == expectedText
    then ok
    else crash $ "Failed to read expectedText from file: " ++ show myFile

-- * Utilities

initCodebase :: Codebase.Init IO Symbol Ann -> FilePath -> String -> IO (CodebasePath, IO (), Codebase IO Symbol Ann)
initCodebase cbInit tmpDir name = do
  let codebaseDir = tmpDir </> name
  (finalize, c) <- Codebase.openNewUcmCodebaseOrExit cbInit codebaseDir
  pure (codebaseDir, finalize, c)

-- run a transcript on an existing codebase
runTranscript_
  :: MonadIO m
  => Bool
  -> FilePath
  -> Codebase IO Symbol Ann
  -> String
  -> m ()
runTranscript_ newRt tmpDir c transcript = do
  let configFile = tmpDir </> ".unisonConfig"
  let cwd = tmpDir </> "cwd"
  let err err = error $ "Parse error: \n" <> show err

  -- parse and run the transcript
  flip (either err) (TR.parse "transcript" (Text.pack transcript)) $ \stanzas ->
    void . liftIO $
      TR.run (Just newRt) cwd configFile stanzas c
        >>= traceM . Text.unpack

withScopeAndTempDir :: String -> (FilePath -> Codebase IO Symbol Ann -> Test ()) -> Test ()
withScopeAndTempDir name body = scope name $ do
  tmp <- liftIO $ Temp.getCanonicalTemporaryDirectory >>= flip Temp.createTempDirectory name
  (_, closeCodebase, codebase) <- liftIO $ initCodebase FileCodebase.init tmp "user"
  body tmp codebase
  liftIO do
    closeCodebase
    removeDirectoryRecursive tmp
