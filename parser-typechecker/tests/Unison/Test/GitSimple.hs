{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Unison.Test.GitSimple where

import Control.Lens (view, _1)
import Data.String.Here (iTrim)
import qualified Data.Text as Text
import Data.Typeable (Typeable)
import EasyTest
import Shellmet ()
import System.Directory (removeDirectoryRecursive)
import System.FilePath ((</>))
import qualified System.IO.Temp as Temp
import Unison.Codebase (Codebase, CodebasePath)
import qualified Unison.Codebase.SqliteCodebase as FC
import qualified Unison.Codebase.TranscriptParser as TR
import Unison.Parser (Ann)
import Unison.Prelude
import Unison.Symbol (Symbol)

test :: Test ()
test = scope "git-simple" . tests $ [testPull]

-- [ testPull
-- , testPush
-- , syncComplete
-- , syncTestResults
-- ]

traceTranscriptOutput :: Bool
traceTranscriptOutput = False

authorTranscript :: (Semigroup a1, IsString a1, Show a2, Typeable a2, Typeable a1) => a2 -> a1
authorTranscript repo =
  [iTrim|
```ucm:hide
.builtin> alias.type ##Nat Nat
.builtin> alias.term ##Nat.+ Nat.+
```
```unison
unique type outside.A = A Nat
unique type outside.B = B Nat Nat
outside.c = 3
outside.d = 4

unique type inside.X = X outside.A
inside.y = c + c
```
```ucm
.myLib> debug.file
.myLib> add
.myLib> push ${repo}
```
|]

userTranscript :: (Semigroup a1, IsString a1, Show a2, Typeable a2, Typeable a1) => a2 -> a1
userTranscript repo =
  [iTrim|
```ucm:hide
.builtin> alias.type ##Nat Nat
.builtin> alias.term ##Nat.+ Nat.+
```
```ucm
.yourLib> pull ${repo}:.inside
```
```unison
> y + #msp7bv40rv + 1
```
|]

-- goal of this test is to make sure that pull doesn't grab a ton of unneeded
-- dependencies
testPull :: Test ()
testPull = scope "pull" $ do
  -- let's push a broader set of stuff, pull a narrower one (to a fresh codebase)
  -- and verify that we have the definitions we expected and don't have some of
  -- the ones we didn't expect.

  -- put all our junk into here
  tmp <- io $ Temp.getCanonicalTemporaryDirectory >>= flip Temp.createTempDirectory "git-pull"

  -- initialize author and user codebases
  (_authorDir, closeAuthor, authorCodebase) <- io $ initCodebase tmp "author"
  (_userDir, closeUser, userCodebase) <- io $ initCodebase tmp "user"

  -- initialize git repo
  let repo = tmp </> "repo.git"
  io $ "git" ["init", "--bare", Text.pack repo]

  -- run author/push transcript
  authorOutput <- runTranscript tmp authorCodebase (authorTranscript repo)

  -- -- check out the resulting repo so we can inspect it
  -- io $ "git" ["clone", Text.pack repo, Text.pack $ tmp </> "repo" ]

  -- run user/pull transcript
  userOutput <- runTranscript tmp userCodebase (userTranscript repo)

  io do
    closeAuthor
    closeUser

    writeFile
      "unison-src/transcripts/GitSimple.hs.output.md"
      (authorOutput <> "\n-------\n" <> userOutput)

  -- -- inspect user codebase
  -- scope "user-should-have" $
  --   for userShouldHave $ \path ->
  --     scope (makeTitle path) $ io (doesFileExist $ userDir </> path) >>= expect
  -- scope "user-should-not-have" $ -- this definitely won't pass with current implementation
  --   for userShouldNotHave $ \path ->
  --     scope (makeTitle path) $ io (doesFileExist $ userDir </> path) >>= expect . not

  -- if we haven't crashed, clean up!
    removeDirectoryRecursive tmp

-- initialize a fresh codebase
initCodebaseDir :: FilePath -> String -> IO CodebasePath
initCodebaseDir tmpDir name = view _1 <$> initCodebase tmpDir name

initCodebase :: FilePath -> String -> IO (CodebasePath, IO (), Codebase IO Symbol Ann)
initCodebase tmpDir name = do
  let codebaseDir = tmpDir </> name
  (close, c) <- FC.initCodebase codebaseDir
  pure (codebaseDir, close, c)

-- run a transcript on an existing codebase
runTranscript :: MonadIO m => FilePath -> Codebase IO Symbol Ann -> String -> m String
runTranscript tmpDir c transcript = do
  let configFile = tmpDir </> ".unisonConfig"
  -- transcript runner wants a "current directory" for I guess writing scratch files?
  let cwd = tmpDir </> "cwd"
  let err err = error $ "Parse error: \n" <> show err

  -- parse and run the transcript
  flip (either err) (TR.parse "transcript" (Text.pack transcript)) $ \stanzas ->
    liftIO . fmap Text.unpack $ TR.run Nothing cwd configFile stanzas c
