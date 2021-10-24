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
  scope "argument-parsing" . tests $
    map
      (expectExitCode ExitSuccess)
      [ ["--help"],
        ["-h"],
        ["version", "--help"],
        ["init", "--help"],
        ["run", "--help"],
        -- ,["run.compile", "--help"] -- Invalid Argument?
        ["run.file", "--help"],
        ["run.pipe", "--help"],
        ["transcript", "--help"],
        ["transcript.fork", "--help"],
        ["headless", "--help"],
        ["version"],
        ["init"], -- removed
        -- ,["run"] -- how?
        ,["run.file", uFile, "print"]
        ,["transcript", transcriptFile]
        ,["transcript.fork", transcriptFile]
        -- ,["headless"] -- ?
        -- options
        ,["--port", "8000"]
        ,["--host", "localhost"]
        ,["--token", "MY_TOKEN"] -- ?
        ,["--codebase-create", tempCodebase]
        ,["--codebase", tempCodebase]
        ,["--ui", tempCodebase]
        ["--no-base"]
      ]
      ++ map
        (expectExitCode (ExitFailure 1))
        [ ["--port", "x"], -- must be number
          ["run.compile", "--help"], -- ?
          ["run.file"], -- without file
          ["run.file", uFile], -- without SYMBOL
          ["transcript"], -- without file
          ["transcript.fork"] -- without file
        ]
      ++ [testRunPipe, clearTempCodebase]

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
    stdin =
      "print : '{IO, Exception} () \n\
      \print _ = base.io.printLine \"ok\""

    fullargs :: [String]
    fullargs = ["exec", "--", "unison", "run.pipe", "print"]

clearTempCodebase :: Test ()
clearTempCodebase = scope "(clear)" do
  _ <- io do
    "rm" $| (map pack ["-rf", tempCodebase])
  ok