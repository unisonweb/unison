{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# LANGUAGE OverloadedStrings #-}

module IntegrationTests.ArgumentParsing where

import Data.List (intercalate)

import Data.Text (pack)
import Data.Time (getCurrentTime, diffUTCTime)
import EasyTest
import Shellmet (($|))
import System.Exit (ExitCode (ExitSuccess))
import System.Process (readProcessWithExitCode)
import Text.Printf

uFile :: String
uFile = "unison-cli/integration-tests/IntegrationTests/print.u"

transcriptFile :: String
transcriptFile = "unison-cli/integration-tests/IntegrationTests/transcript.md"

unisonCmdString :: String
unisonCmdString = unlines
  [ "print : '{IO, Exception} ()"
  , "print _ = base.io.printLine \"ok\""
  ]

tempCodebase :: String
tempCodebase = "tempcodebase"

test :: Test ()
test =
  EasyTest.using (pure ()) clearTempCodebase \_ ->
     scope "argument-parsing" . tests $
      [ expectExitCode ExitSuccess "stack" ["exec", "--", "unison", "--help"] ""
      , expectExitCode ExitSuccess "stack" ["exec", "--", "unison", "-h"] ""
      , expectExitCode ExitSuccess "stack" ["exec", "--", "unison", "version", "--help"] ""
      , expectExitCode ExitSuccess "stack" ["exec", "--", "unison", "init", "--help"] ""
      , expectExitCode ExitSuccess "stack" ["exec", "--", "unison", "run", "--help"] ""
      , expectExitCode ExitSuccess "stack" ["exec", "--", "unison", "run.file", "--help"] ""
      , expectExitCode ExitSuccess "stack" ["exec", "--", "unison", "run.pipe", "--help"] ""
      , expectExitCode ExitSuccess "stack" ["exec", "--", "unison", "transcript", "--help"] ""
      , expectExitCode ExitSuccess "stack" ["exec", "--", "unison", "transcript.fork", "--help"] ""
      , expectExitCode ExitSuccess "stack" ["exec", "--", "unison", "headless", "--help"] ""
      , expectExitCode ExitSuccess "stack" ["exec", "--", "unison", "version"] ""
      -- , expectExitCode ExitSuccess "stack" ["exec", "--", "unison", "run"] "" -- how?
      , expectExitCode ExitSuccess "stack" ["exec", "--", "unison", "run.file", uFile, "print", "--codebase-create", tempCodebase] ""
      , expectExitCode ExitSuccess "stack" ["exec", "--", "unison", "run.pipe", "print", "--codebase-create", tempCodebase] unisonCmdString
      , expectExitCode ExitSuccess "stack" ["exec", "--", "unison", "transcript", transcriptFile, "--codebase-create", tempCodebase] ""
      , expectExitCode ExitSuccess "stack" ["exec", "--", "unison", "transcript.fork", transcriptFile, "--codebase-create", tempCodebase] ""
      -- , expectExitCode ExitSuccess "stack" ["exec", "--", "unison", "headless"] "" -- ?
      -- options
      , expectExitCode ExitSuccess "stack" ["exec", "--", "unison", "--port", "8000", "--codebase-create", tempCodebase, "--no-base"] ""
      , expectExitCode ExitSuccess "stack" ["exec", "--", "unison", "--host", "localhost", "--codebase-create", tempCodebase, "--no-base"] ""
      , expectExitCode ExitSuccess "stack" ["exec", "--", "unison", "--token", "MY_TOKEN", "--codebase-create", tempCodebase, "--no-base"] "" -- ?
      , expectExitCode ExitSuccess "stack" ["exec", "--", "unison", "--codebase-create", tempCodebase, "--no-base"] ""
      , expectExitCode ExitSuccess "stack" ["exec", "--", "unison", "--ui", tempCodebase, "--codebase-create", tempCodebase, "--no-base"] ""
      , scope "can compile, then run compiled artifact" $ tests
        [ expectExitCode ExitSuccess "stack" ["exec", "--", "unison", "transcript", transcriptFile] ""
        , expectExitCode ExitSuccess "stack" ["exec", "--", "unison", "run.compiled", "./unison-cli/integration-tests/IntegrationTests/main.uc"] ""
        ]
      ]

expectExitCode :: ExitCode -> FilePath -> [String] -> String -> Test ()
expectExitCode expected cmd args stdin = scope (intercalate " " (cmd : args)) do
  start <- io $ getCurrentTime
  (code, _, _) <- io $ readProcessWithExitCode cmd args stdin
  end <- io $ getCurrentTime
  let diff = diffUTCTime end start
  note $ printf "\n[Time: %s sec]" $ show diff
  expectEqual code expected

defaultArgs :: [String]
defaultArgs = ["--codebase-create", tempCodebase, "--no-base"]

clearTempCodebase :: () -> IO()
clearTempCodebase _ = do
    "rm" $| (map pack ["-rf", tempCodebase])
    pure ()
