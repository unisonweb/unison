{-# Language OverloadedStrings #-}
{-# Language QuasiQuotes #-}

module Unison.Test.Git where

import EasyTest
import Data.String.Here (iTrim)
import Unison.Prelude
import Data.Text (Text)
import qualified Data.Text as Text
import qualified System.IO.Temp as Temp
import Shellmet ()
import System.FilePath ((</>))
import System.Directory (getCurrentDirectory)

import Unison.Codebase.FileCodebase as FC
import qualified Unison.Codebase.TranscriptParser as TR

test :: Test ()
test = scope "git" . tests $ [testBareRepo]

testBareRepo :: Test ()
testBareRepo = scope "testBareRepo" $ do
  io . Temp.withSystemTempDirectory "testBareRepo" $ \tmp -> do
    -- create a git repo and a transcript that references it
    let repo = tmp </> "repo.git"
    "git" ["init", "--bare", Text.pack repo]
    let transcript = makeTranscript repo

    -- initialize an fresh codebase
    let codebase = tmp </> "codebase"
    FC.initCodebase codebase

    let configFile = tmp </> ".unisonConfig"

    case TR.parse "transcript" transcript of
      Left err -> error $ "Parse error: \n" <> show err
      Right stanzas -> void $ do
        currentDir <- getCurrentDirectory
        theCodebase <- FC.getCodebaseOrExit $ Just codebase
        TR.run currentDir configFile stanzas theCodebase
  ok

makeTranscript :: FilePath -> Text
makeTranscript repoPath = Text.pack $ [iTrim|
```unison
x = 3
```
```ucm
.foo> add
.foo> push ${repoPath}
```
Now we pull what we pushed
```ucm
.foo2> pull ${repoPath}
.foo2> ls
```
|]
