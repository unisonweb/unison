{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Unison.Test.GitSimple where

import qualified Data.Text as Text
import EasyTest
import Shellmet ()
import System.Directory (removeDirectoryRecursive)
import System.FilePath ((</>))
import qualified System.IO.Temp as Temp
import Unison.Prelude
import Unison.Test.Ucm (CodebaseFormat, Transcript)
import qualified Unison.Test.Ucm as Ucm
import Data.String.Here.Interpolated (i)

-- keep it off for CI, since the random temp dirs it generates show up in the
-- output, which causes the test output to change, and the "no change" check
-- to fail
writeTranscriptOutput :: Bool
writeTranscriptOutput = False

test :: Test ()
test = scope "git-simple" . tests $
  flip map [(Ucm.CodebaseFormat1 , "fc"), (Ucm.CodebaseFormat2, "sc")]
  \(fmt, name) -> scope name $ tests [
  pushPullTest "one-term" fmt
-- simplest-author
    (\repo -> [i|
      ```unison
      c = 3
      ```
      ```ucm
      .> debug.file
      .> add
      .> push ${repo}
      ```
    |])
-- simplest-user
    (\repo -> [i|
      ```ucm
      .> pull ${repo}
      .> alias.term ##Nat.+ +
      ```
      ```unison
      > #msp7bv40rv + 1
      ```
    |])
  ,
  pushPullTest "one-term2" fmt
-- simplest-author
    (\repo -> [i|
      ```unison
      c = 3
      ```
      ```ucm
      .> debug.file
      .myLib> add
      .myLib> push ${repo}
      ```
      |])
-- simplest-user
    (\repo -> [i|
      ```ucm
      .yourLib> pull ${repo}
      ```
      ```unison
      > c
      ```
      |])
  ,
  pushPullTest "one-type" fmt
-- simplest-author
    (\repo -> [i|
      ```unison
      type Foo = Foo
      ```
      ```ucm
      .myLib> debug.file
      .myLib> add
      .myLib> push ${repo}
      ```
      |])
-- simplest-user
    (\repo -> [i|
      ```ucm
      .yourLib> pull ${repo}
      ```
      ```unison
      > Foo.Foo
      ```
      |])
  ,
  pushPullTest "patching" fmt
    (\repo -> [i|
      ```ucm
      .myLib> alias.term ##Nat.+ +
      ```
      ```unison
      improveNat x = x + 3
      ```
      ```ucm
      .myLib> add
      .myLib> ls
      .myLib> move.namespace .myLib .workaround1552.myLib.v1
      .workaround1552.myLib> ls
      .workaround1552.myLib> fork v1 v2
      .workaround1552.myLib.v2>
      ```
      ```unison
      improveNat x = x + 100
      ```
      ```ucm
      .workaround1552.myLib.v2> update
      .workaround1552.myLib> push ${repo}
      ```
    |])
    (\repo -> [i|
      ```ucm
      .myApp> pull ${repo}:.v1 external.yourLib
      .myApp> alias.term ##Nat.* *
      ````
      ```unison
      greatApp = improveNat 5 * improveNat 6
      > greatApp
      ```
      ```ucm
      .myApp> add
      .myApp> pull ${repo}:.v2 external.yourLib
      ```
      ```unison
      > greatApp
      ```
      ```ucm
      .myApp> patch external.yourLib.patch
      ```
      ```unison
      > greatApp
      ```
    |])
    -- ,
  -- pushPullTest "regular" fmt
  --   (\repo -> [i|
  --   ```ucm:hide
  --   .builtin> alias.type ##Nat Nat
  --   .builtin> alias.term ##Nat.+ Nat.+
  --   ```
  --   ```unison
  --   unique type outside.A = A Nat
  --   unique type outside.B = B Nat Nat
  --   outside.c = 3
  --   outside.d = 4

  --   unique type inside.X = X outside.A
  --   inside.y = c + c
  --   ```
  --   ```ucm
  --   .myLib> debug.file
  --   .myLib> add
  --   .myLib> push ${repo}
  --   ```
  --   |])

  --       (\repo -> [i|
  --   ```ucm:hide
  --   .builtin> alias.type ##Nat Nat
  --   .builtin> alias.term ##Nat.+ Nat.+
  --   ```
  --   ```ucm
  --   .yourLib> pull ${repo}:.inside
  --   ```
  --   ```unison
  --   > y + #msp7bv40rv + 1
  --   ```
  --   |])
  ]

-- type inside.X#skinr6rvg7
-- type outside.A#l2fmn9sdbk
-- type outside.B#nsgsq4ot5u
-- inside.y#omqnfettvj
-- outside.c#msp7bv40rv
-- outside.d#52addbrohu
-- .myLib> #6l0nd3i15e
-- .myLib.inside> #5regvciils
-- .myLib.inside.X> #kvcjrmgki6
-- .myLib.outside> #uq1mkkhlf1
-- .myLib.outside.A> #0e3g041m56
-- .myLib.outside.B> #j57m94daqi

pushPullTest :: String -> CodebaseFormat -> (FilePath -> Transcript) -> (FilePath -> Transcript) -> Test ()
pushPullTest name fmt authorScript userScript = scope name do
  io do
    repo <- initGitRepo
    author <- Ucm.initCodebase fmt
    authorOutput <- Ucm.runTranscript author Ucm.Runtime1 (authorScript repo)
    user <- Ucm.initCodebase fmt
    userOutput <- Ucm.runTranscript user Ucm.Runtime1 (userScript repo)

    when writeTranscriptOutput $ writeFile
      ("unison-src"</>"transcripts"</>("GitSimple." ++ name ++ ".output.md"))
      (authorOutput <> "\n-------\n" <> userOutput)

    -- if we haven't crashed, clean up!
    removeDirectoryRecursive repo
    Ucm.deleteCodebase author
    Ucm.deleteCodebase user
  ok

initGitRepo :: IO FilePath
initGitRepo = do
  tmp <- Temp.getCanonicalTemporaryDirectory >>= flip Temp.createTempDirectory ("git-simple")
  let repo = tmp </> "repo.git"
  "git" ["init", "--bare", Text.pack repo]
  pure repo
