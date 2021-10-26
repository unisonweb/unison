{-# LANGUAGE OverloadedStrings #-}

module IntegrationTests.ArgumentParsing where

import Data.List (intercalate)

import Data.Text (pack)
import EasyTest
import Shellmet (($|))
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process (readProcessWithExitCode)

uFile :: String
uFile = "cli/integration-tests/IntegrationTests/print.u"

transcriptFile :: String
transcriptFile = "cli/integration-tests/IntegrationTests/transcript.md"

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
      -- , expectExitCode ExitSuccess "stack" ["exec", "--", "unison", "run.compile", "--help"] "" -- Invalid Argument?
      , expectExitCode ExitSuccess "stack" ["exec", "--", "unison", "run.file", "--help"] ""
      , expectExitCode ExitSuccess "stack" ["exec", "--", "unison", "run.pipe", "--help"] ""
      , expectExitCode ExitSuccess "stack" ["exec", "--", "unison", "transcript", "--help"] ""
      , expectExitCode ExitSuccess "stack" ["exec", "--", "unison", "transcript.fork", "--help"] ""
      , expectExitCode ExitSuccess "stack" ["exec", "--", "unison", "headless", "--help"] ""
      , expectExitCode ExitSuccess "stack" ["exec", "--", "unison", "version"] ""
      , expectExitCode ExitSuccess "stack" ["exec", "--", "unison", "init"] "" -- removed
      -- , expectExitCode ExitSuccess "stack" ["exec", "--", "unison", "run"] "" -- how?
      , expectExitCode ExitSuccess "stack" ["exec", "--", "unison", "run.file", uFile, "print"] ""
      , expectExitCode ExitSuccess "stack" ["exec", "--", "unison", "run.pipe", "print"] unisonCmdString
      , expectExitCode ExitSuccess "stack" ["exec", "--", "unison", "transcript", transcriptFile] ""
      , expectExitCode ExitSuccess "stack" ["exec", "--", "unison", "transcript.fork", transcriptFile] ""
      -- , expectExitCode ExitSuccess "stack" ["exec", "--", "unison", "headless"] "" -- ?
      -- options
      , expectExitCode ExitSuccess "stack" ["exec", "--", "unison", "--port", "8000"] ""
      , expectExitCode ExitSuccess "stack" ["exec", "--", "unison", "--host", "localhost"] ""
      , expectExitCode ExitSuccess "stack" ["exec", "--", "unison", "--token", "MY_TOKEN"] "" -- ?
      , expectExitCode ExitSuccess "stack" ["exec", "--", "unison", "--codebase-create", tempCodebase] ""
      , expectExitCode ExitSuccess "stack" ["exec", "--", "unison", "--codebase", tempCodebase] ""
      , expectExitCode ExitSuccess "stack" ["exec", "--", "unison", "--ui", tempCodebase] ""
      , expectExitCode ExitSuccess "stack" ["exec", "--", "unison", "--no-base"] ""

      -- Failure
      , expectExitCode (ExitFailure 1) "stack" ["exec", "--", "unison", "--port", "x"] "" -- must be number
      , expectExitCode (ExitFailure 1) "stack" ["exec", "--", "unison", "run.compile", "--help"] "" -- ?
      , expectExitCode (ExitFailure 1) "stack" ["exec", "--", "unison", "run.file"] "" -- without file
      , expectExitCode (ExitFailure 1) "stack" ["exec", "--", "unison", "run.file", uFile] "" -- without SYMBOL
      , expectExitCode (ExitFailure 1) "stack" ["exec", "--", "unison", "transcript"] "" -- without file
      , expectExitCode (ExitFailure 1) "stack" ["exec", "--", "unison", "transcript.fork"] "" -- without file
      ]

expectExitCode :: ExitCode -> FilePath -> [String] -> String -> Test ()
expectExitCode expected cmd args stdin = scope (intercalate " " (cmd : args)) do
  (code, _, _) <- io $ readProcessWithExitCode cmd args stdin
  expectEqual code expected

clearTempCodebase :: () -> IO()
clearTempCodebase _ = do
    "rm" $| (map pack ["-rf", tempCodebase])
    pure ()