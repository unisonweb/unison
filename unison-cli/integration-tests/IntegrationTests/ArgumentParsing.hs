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

tempCodebase :: String
tempCodebase = "tempcodebase"

test :: Test ()
test =
  EasyTest.using (pure ()) clearTempCodebase \_ ->
     scope "argument-parsing" . tests $
      [ expectExitCode ExitSuccess ["--help"]
      , expectExitCode ExitSuccess ["-h"]
      , expectExitCode ExitSuccess ["version", "--help"]
      , expectExitCode ExitSuccess ["init", "--help"]
      , expectExitCode ExitSuccess ["run", "--help"]
      -- , expectExitCode ExitSuccess ["run.compile", "--help"] -- Invalid Argument?
      , expectExitCode ExitSuccess ["run.file", "--help"]
      , expectExitCode ExitSuccess ["run.pipe", "--help"]
      , expectExitCode ExitSuccess ["transcript", "--help"]
      , expectExitCode ExitSuccess ["transcript.fork", "--help"]
      , expectExitCode ExitSuccess ["headless", "--help"]
      , expectExitCode ExitSuccess ["version"]
      , expectExitCode ExitSuccess ["init"] -- removed
      -- , expectExitCode ExitSuccess ["run"] -- how?
      , expectExitCode ExitSuccess ["run.file", uFile, "print"]
      , expectExitCode ExitSuccess ["transcript", transcriptFile]
      , expectExitCode ExitSuccess ["transcript.fork", transcriptFile]
      -- , expectExitCode ExitSuccess ["headless"] -- ?
      -- options
      , expectExitCode ExitSuccess ["--port", "8000"]
      , expectExitCode ExitSuccess ["--host", "localhost"]
      , expectExitCode ExitSuccess ["--token", "MY_TOKEN"] -- ?
      , expectExitCode ExitSuccess ["--codebase-create", tempCodebase]
      , expectExitCode ExitSuccess ["--codebase", tempCodebase]
      , expectExitCode ExitSuccess ["--ui", tempCodebase]
      , expectExitCode ExitSuccess ["--no-base"]

      -- Failure
      , expectExitCode (ExitFailure 1) ["--port", "x"] -- must be number
      , expectExitCode (ExitFailure 1) ["run.compile", "--help"] -- ?
      , expectExitCode (ExitFailure 1) ["run.file"] -- without file
      , expectExitCode (ExitFailure 1) ["run.file", uFile] -- without SYMBOL
      , expectExitCode (ExitFailure 1) ["transcript"] -- without file
      , expectExitCode (ExitFailure 1) ["transcript.fork"] -- without file

      -- others
      , testRunPipe
      ]

expectExitCode :: ExitCode -> [String] -> Test ()
expectExitCode expected args = scope (intercalate " " args) do
  (code, _, _) <- io $ readProcessWithExitCode "stack" fullargs ""
  expectEqual code expected
  where
    fullargs :: [String]
    fullargs = ["exec", "--", "unison"] ++ args

testRunPipe :: Test ()
testRunPipe = scope "run.pipe print" do
  (code, _, _) <- io $ readProcessWithExitCode "stack" fullargs stdin
  expectEqual code ExitSuccess
  where
    stdin = unlines [ "print : '{IO, Exception} ()"
                    , "print _ = base.io.printLine \"ok\""
                    ]

    fullargs :: [String]
    fullargs = ["exec", "--", "unison", "run.pipe", "print"]

clearTempCodebase :: () -> IO()
clearTempCodebase _ = do
    "rm" $| (map pack ["-rf", tempCodebase])
    pure ()