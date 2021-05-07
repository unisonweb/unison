{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}

module Unison.Test.BaseUpgradePushPullTest where

import Data.String.Here.Interpolated (i)
import EasyTest
import Shellmet ()
import qualified Unison.Test.Ucm as Ucm
import Unison.Test.GitSync (initGitRepo)

-- keep it off for CI, since the random temp dirs it generates show up in the
-- output, which causes the test output to change, and the "no change" check
-- to fail
writeTranscriptOutput :: Bool
writeTranscriptOutput = False

test :: Test ()
test = scope "base-upgrade-push-pull-test" do
  io do
    v1 <- Ucm.initCodebase Ucm.CodebaseFormat1
    putStrLn =<< Ucm.runTranscript v1 [i|
      ```ucm
      .> pull /Users/arya/base _base
      ```
    |]
    v2 <- Ucm.upgradeCodebase v1
    repo <- initGitRepo
    putStrLn =<< Ucm.runTranscript v2 [i|
      ```ucm
      .> push ${repo} _base
      ```
    |]
    v2' <- Ucm.initCodebase Ucm.CodebaseFormat2
    putStrLn $ show v2'
    putStrLn =<< Ucm.runTranscript v2' [i|
      ```ucm
      .> pull ${repo} _base
      .> test
      ```
    |]
  ok
