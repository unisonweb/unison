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
      [ expectExitCode ExitSuccess "stack" defaultArgs ["exec", "--", "unison", "--help"] ""
      , expectExitCode ExitSuccess "stack" defaultArgs ["exec", "--", "unison", "-h"] ""
      , expectExitCode ExitSuccess "stack" defaultArgs ["exec", "--", "unison", "version", "--help"] ""
      , expectExitCode ExitSuccess "stack" defaultArgs ["exec", "--", "unison", "init", "--help"] ""
      , expectExitCode ExitSuccess "stack" defaultArgs ["exec", "--", "unison", "run", "--help"] ""
      , expectExitCode ExitSuccess "stack" defaultArgs ["exec", "--", "unison", "run.file", "--help"] ""
      , expectExitCode ExitSuccess "stack" defaultArgs ["exec", "--", "unison", "run.pipe", "--help"] ""
      , expectExitCode ExitSuccess "stack" defaultArgs ["exec", "--", "unison", "transcript", "--help"] ""
      , expectExitCode ExitSuccess "stack" defaultArgs ["exec", "--", "unison", "transcript.fork", "--help"] ""
      , expectExitCode ExitSuccess "stack" defaultArgs ["exec", "--", "unison", "headless", "--help"] ""
      , expectExitCode ExitSuccess "stack" defaultArgs ["exec", "--", "unison", "version"] ""
      -- , expectExitCode ExitSuccess "stack" defaultArgs ["exec", "--", "unison", "run"] "" -- how?
      , expectExitCode ExitSuccess "stack" defaultArgs ["exec", "--", "unison", "run.file", uFile, "print"] ""
      , expectExitCode ExitSuccess "stack" defaultArgs ["exec", "--", "unison", "run.pipe", "print"] unisonCmdString
      , expectExitCode ExitSuccess "stack" [] ["exec", "--", "unison", "transcript", transcriptFile, "--codebase-create", tempCodebase] ""
      , expectExitCode ExitSuccess "stack" [] ["exec", "--", "unison", "transcript.fork", transcriptFile, "--codebase-create", tempCodebase] ""
      -- , expectExitCode ExitSuccess "stack" defaultArgs ["exec", "--", "unison", "headless"] "" -- ?
      -- options
      , expectExitCode ExitSuccess "stack" defaultArgs ["exec", "--", "unison", "--port", "8000"] ""
      , expectExitCode ExitSuccess "stack" defaultArgs ["exec", "--", "unison", "--host", "localhost"] ""
      , expectExitCode ExitSuccess "stack" defaultArgs ["exec", "--", "unison", "--token", "MY_TOKEN"] "" -- ?
      , expectExitCode ExitSuccess "stack" defaultArgs ["exec", "--", "unison"] ""
      , expectExitCode ExitSuccess "stack" defaultArgs ["exec", "--", "unison", "--ui", tempCodebase] ""
      , scope "can compile, then run compiled artifact" $ tests
        [ expectExitCode ExitSuccess "stack" defaultArgs ["exec", "--", "unison", "transcript", transcriptFile] ""
        , expectExitCode ExitSuccess "stack" defaultArgs ["exec", "--", "unison", "run.compiled", "./unison-cli/integration-tests/IntegrationTests/main.uc"] ""
        ]
      ]

expectExitCode :: ExitCode -> FilePath -> [String] -> [String] -> String -> Test ()
expectExitCode expected cmd defArgs args stdin = scope (intercalate " " (cmd : args <> defArgs)) do
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
