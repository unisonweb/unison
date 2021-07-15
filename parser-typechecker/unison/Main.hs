{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Main where

#if defined(mingw32_HOST_OS)
import qualified GHC.ConsoleHandler as WinSig
#else
import qualified System.Posix.Signals as Sig
#endif

import Control.Concurrent (mkWeakThreadId, myThreadId, newEmptyMVar, takeMVar)
import Control.Error.Safe (rightMay)
import Control.Exception (AsyncException (UserInterrupt), throwTo)
import Data.ByteString.Char8 (unpack)
import Data.Configurator.Types (Config)
import qualified Data.Text as Text
import qualified GHC.Conc
import qualified Network.URI.Encode as URI
import System.Directory (canonicalizePath, getCurrentDirectory, removeDirectoryRecursive)
import System.Environment (getArgs, getProgName)
import qualified System.Exit as Exit
import qualified System.FilePath as FP
import System.IO.Error (catchIOError)
import qualified System.IO.Temp as Temp
import System.Mem.Weak (deRefWeak)
import qualified System.Path as Path
import Text.Megaparsec (runParser)
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.Init as Codebase
import qualified Unison.Codebase.Editor.Input as Input
import Unison.Codebase.Editor.RemoteRepo (RemoteNamespace)
import qualified Unison.Codebase.Editor.VersionParser as VP
import Unison.Codebase.Execute (execute)
import qualified Unison.Codebase.FileCodebase as FC
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.SqliteCodebase as SC
import qualified Unison.Codebase.TranscriptParser as TR
import Unison.CommandLine (plural', watchConfig)
import qualified Unison.CommandLine.Main as CommandLine
import Unison.Parser (Ann)
import Unison.Prelude
import qualified Unison.Codebase.Runtime as Rt
import qualified Unison.PrettyTerminal as PT
import qualified Unison.Runtime.Interface as RTI
import qualified Unison.Server.CodebaseServer as Server
import Unison.Symbol (Symbol)
import qualified Unison.Util.Pretty as P
import qualified Version
import qualified Unison.Codebase.Conversion.Upgrade12 as Upgrade12

usage :: String -> P.Pretty P.ColorText
usage executableStr = P.callout "🌻" $ P.lines [
  P.bold "Usage instructions for the Unison Codebase Manager",
  "You are running version: " <> P.string Version.gitDescribe,
  "",
  P.bold executable,
  P.wrap "Starts Unison interactively, using the codebase in the home directory.",
  "",
  P.bold $ executable <> " -codebase path/to/codebase",
  P.wrap "Starts Unison interactively, using the specified codebase. This flag can also be set before any of the below commands.",
  "",
  P.bold $ executable <> " --old-codebase",
  P.wrap $ "Starts Unison using a v1 codebase. This flag can also be set before any of the below commands.",
  "",
  P.bold $ executable <> " run .mylib.mymain",
  P.wrap "Executes the definition `.mylib.mymain` from the codebase, then exits.",
  "",
  P.bold $ executable <> " run.file foo.u mymain",
  P.wrap "Executes the definition called `mymain` in `foo.u`, then exits.",
  "",
  P.bold $ executable <> " run.pipe mymain",
  P.wrap "Executes the definition called `mymain` from a `.u` file read from the standard input, then exits.",
  "",
  P.bold $ executable <> " transcript mytranscript.md",
  P.wrap $ "Executes the `mytranscript.md` transcript and creates"
        <> "`mytranscript.output.md` if successful. Exits after completion, and deletes"
        <> "the temporary directory created."
        <> "Multiple transcript files may be provided; they are processed in sequence"
        <> "starting from the same codebase.",
  "",
  P.bold $ executable <> " transcript -save-codebase mytranscript.md",
  P.wrap $ "Executes the `mytranscript.md` transcript and creates"
        <> "`mytranscript.output.md` if successful. Exits after completion, and saves"
        <> "the resulting codebase to a new directory on disk."
        <> "Multiple transcript files may be provided; they are processed in sequence"
        <> "starting from the same codebase.",
  "",
  P.bold $ executable <> " transcript.fork mytranscript.md",
  P.wrap $ "Executes the `mytranscript.md` transcript in a copy of the current codebase"
        <> "and creates `mytranscript.output.md` if successful. Exits after completion."
        <> "Multiple transcript files may be provided; they are processed in sequence"
        <> "starting from the same codebase.",
  "",
  P.bold $ executable <> " transcript.fork -save-codebase mytranscript.md",
  P.wrap $ "Executes the `mytranscript.md` transcript in a copy of the current codebase"
        <> "and creates `mytranscript.output.md` if successful. Exits after completion,"
        <> "and saves the resulting codebase to a new directory on disk."
        <> "Multiple transcript files may be provided; they are processed in sequence"
        <> "starting from the same codebase.",
  "",
  P.bold $ executable <> " upgrade-codebase",
  "Upgrades a v1 codebase to a v2 codebase.",
  "",
  P.bold $ executable <> " headless",
  "Runs the codebase server without the command-line interface.",
  "",
  P.bold $ executable <> " version",
  "Prints version of Unison then quits.",
  "",
  P.bold $ executable <> " help",
  "Prints this help."]
      where executable = (P.text . Text.pack) executableStr

installSignalHandlers :: IO ()
installSignalHandlers = do
  main_thread <- myThreadId
  wtid <- mkWeakThreadId main_thread

  let interrupt = do
        r <- deRefWeak wtid
        case r of
          Nothing -> return ()
          Just t  -> throwTo t UserInterrupt

#if defined(mingw32_HOST_OS)
  let sig_handler WinSig.ControlC = interrupt
      sig_handler WinSig.Break    = interrupt
      sig_handler _               = return ()
  _ <- WinSig.installHandler (WinSig.Catch sig_handler)
#else
  _ <- Sig.installHandler Sig.sigQUIT  (Sig.Catch interrupt) Nothing
  _ <- Sig.installHandler Sig.sigINT   (Sig.Catch interrupt) Nothing
#endif

  return ()


data CodebaseFormat = V1 | V2 deriving (Eq)

cbInitFor :: CodebaseFormat -> Codebase.Init IO Symbol Ann
cbInitFor = \case V1 -> FC.init; V2 -> SC.init

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  -- hSetBuffering stdout NoBuffering -- cool

  _ <- installSignalHandlers
  -- We need to know whether the program was invoked with -codebase for
  -- certain messages. Therefore we keep a Maybe FilePath - mcodepath
  -- rather than just deciding on whether to use the supplied path or
  -- the home directory here and throwing away that bit of information
  let (mcodepath, restargs0) = case args of
           "-codebase" : codepath : restargs -> (Just codepath, restargs)
           _                                 -> (Nothing, args)
      (fromMaybe V2 -> cbFormat, restargs) = case restargs0 of
           "--new-codebase" : rest -> (Just V2, rest)
           "--old-codebase" : rest -> (Just V1, rest)
           _ -> (Nothing, restargs0)
      cbInit = case cbFormat of V1 -> FC.init; V2 -> SC.init
  currentDir <- getCurrentDirectory
  configFilePath <- getConfigFilePath mcodepath
  config <-
    catchIOError (watchConfig configFilePath) $ \_ ->
      Exit.die "Your .unisonConfig could not be loaded. Check that it's correct!"
  case restargs of
    [version] | isFlag "version" version ->
      putStrLn $ progName ++ " version: " ++ Version.gitDescribe
    [help] | isFlag "help" help -> PT.putPrettyLn (usage progName)
    ["init"] -> Codebase.initCodebaseAndExit cbInit "main.init" mcodepath
    "run" : [mainName] -> do
      (closeCodebase, theCodebase) <- getCodebaseOrExit cbFormat mcodepath
      runtime <- RTI.startRuntime
      execute theCodebase runtime mainName
      closeCodebase
    "run.file" : file : [mainName] | isDotU file -> do
      e <- safeReadUtf8 file
      case e of
        Left _ -> PT.putPrettyLn $ P.callout "⚠️" "I couldn't find that file or it is for some reason unreadable."
        Right contents -> do
          (closeCodebase, theCodebase) <- getCodebaseOrExit cbFormat mcodepath
          rt <- RTI.startRuntime
          let fileEvent = Input.UnisonFileChanged (Text.pack file) contents
          launch currentDir config rt theCodebase [Left fileEvent, Right $ Input.ExecuteI mainName, Right Input.QuitI]
          closeCodebase
    "run.pipe" : [mainName] -> do
      e <- safeReadUtf8StdIn
      case e of
        Left _ -> PT.putPrettyLn $ P.callout "⚠️" "I had trouble reading this input."
        Right contents -> do
          (closeCodebase, theCodebase) <- getCodebaseOrExit cbFormat mcodepath
          rt <- RTI.startRuntime
          let fileEvent = Input.UnisonFileChanged (Text.pack "<standard input>") contents
          launch
            currentDir config rt theCodebase
            [Left fileEvent, Right $ Input.ExecuteI mainName, Right Input.QuitI]
          closeCodebase
    "transcript" : args' ->
      case args' of
      "-save-codebase" : transcripts -> runTranscripts cbFormat False True mcodepath transcripts
      _                              -> runTranscripts cbFormat False False mcodepath args'
    "transcript.fork" : args' ->
      case args' of
      "-save-codebase" : transcripts -> runTranscripts cbFormat True True mcodepath transcripts
      _                              -> runTranscripts cbFormat True False mcodepath args'
    ["upgrade-codebase"] -> upgradeCodebase mcodepath
    args -> do
      let headless = listToMaybe args == Just "headless"
      (closeCodebase, theCodebase) <- getCodebaseOrExit cbFormat mcodepath
      runtime <- RTI.startRuntime
      Server.start runtime theCodebase $ \token port -> do
        let url =
             "http://127.0.0.1:" <> show port <> "/" <> URI.encode (unpack token)
        when headless $
          PT.putPrettyLn $ P.lines
            ["I've started the codebase API server at" , P.string $ url <> "/api"]
        PT.putPrettyLn $ P.lines
          ["The Unison Codebase UI is running at", P.string $ url <> "/ui"]
        if headless then do
          PT.putPrettyLn $ P.string "Running the codebase manager headless with "
            <> P.shown GHC.Conc.numCapabilities
            <> " "
            <> plural' GHC.Conc.numCapabilities "cpu" "cpus"
            <> "."
          mvar <- newEmptyMVar
          takeMVar mvar
        else do
          PT.putPrettyLn $ P.string "Now starting the Unison Codebase Manager..."
          launch currentDir config runtime theCodebase []
          closeCodebase

upgradeCodebase :: Maybe Codebase.CodebasePath -> IO ()
upgradeCodebase mcodepath =
  Codebase.getCodebaseDir mcodepath >>= \root -> do
    PT.putPrettyLn . P.wrap $
      "I'm upgrading the codebase in " <> P.backticked' (P.string root) "," <> "but it will"
      <> "take a while, and may even run out of memory. If you have"
      <> "trouble, contact us on #alphatesting and we'll try to help."
    Upgrade12.upgradeCodebase root
    PT.putPrettyLn . P.wrap
      $ P.newline
      <> "Try it out and once you're satisfied, you can safely(?) delete the old version from"
      <> P.newline
      <> P.indentN 2 (P.string $ Codebase.codebasePath (FC.init @IO) root)
      <> P.newline
      <> "but there's no rush.  You can access the old codebase again by passing the"
      <> P.backticked "--old-codebase" <> "flag at startup."

prepareTranscriptDir :: CodebaseFormat -> Bool -> Maybe FilePath -> IO FilePath
prepareTranscriptDir cbFormat inFork mcodepath = do
  tmp <- Temp.getCanonicalTemporaryDirectory >>= (`Temp.createTempDirectory` "transcript")
  let cbInit = cbInitFor cbFormat
  if inFork then
    getCodebaseOrExit cbFormat mcodepath >> do
    path <- Codebase.getCodebaseDir mcodepath
    PT.putPrettyLn $ P.lines [
      P.wrap "Transcript will be run on a copy of the codebase at: ", "",
      P.indentN 2 (P.string path)
      ]
    Path.copyDir (Codebase.codebasePath cbInit path) (Codebase.codebasePath cbInit tmp)
  else do
    PT.putPrettyLn . P.wrap $ "Transcript will be run on a new, empty codebase."
    void $ Codebase.openNewUcmCodebaseOrExit cbInit "main.transcript" tmp
  pure tmp

runTranscripts'
  :: CodebaseFormat
  -> Maybe FilePath
  -> FilePath
  -> [String]
  -> IO Bool
runTranscripts' codebaseFormat mcodepath transcriptDir args = do
  currentDir <- getCurrentDirectory
  case args of
    args@(_:_) -> do
      for_ args $ \arg -> case arg of
        md | isMarkdown md -> do
          parsed <- TR.parseFile arg
          case parsed of
            Left err ->
              PT.putPrettyLn $ P.callout "❓" (
                P.lines [
                  P.indentN 2 "A parsing error occurred while reading a file:", "",
                  P.indentN 2 $ P.string err])
            Right stanzas -> do
              configFilePath <- getConfigFilePath mcodepath
              (closeCodebase, theCodebase) <- getCodebaseOrExit codebaseFormat $ Just transcriptDir
              mdOut <- TR.run transcriptDir configFilePath stanzas theCodebase
              closeCodebase
              let out = currentDir FP.</>
                         FP.addExtension (FP.dropExtension arg ++ ".output")
                                         (FP.takeExtension md)
              writeUtf8 out mdOut
              putStrLn $ "💾  Wrote " <> out
        wat ->
              PT.putPrettyLn $ P.callout "❓" (
                P.lines [
                  P.indentN 2 "Unrecognized command, skipping:", "",
                  P.indentN 2 $ P.string wat])
      pure True
    [] ->
      pure False

runTranscripts
  :: CodebaseFormat
  -> Bool
  -> Bool
  -> Maybe FilePath
  -> [String]
  -> IO ()
runTranscripts cbFormat inFork keepTemp mcodepath args = do
  progName <- getProgName
  transcriptDir <- prepareTranscriptDir cbFormat inFork mcodepath
  completed <-
    runTranscripts' cbFormat (Just transcriptDir) transcriptDir args
  when completed $ do
    unless keepTemp $ removeDirectoryRecursive transcriptDir
    when keepTemp $ PT.putPrettyLn $
        P.callout "🌸" (
          P.lines [
            "I've finished running the transcript(s) in this codebase:", "",
            P.indentN 2 (P.string transcriptDir), "",
            P.wrap $ "You can run"
                  <> P.backticked (P.string progName <> " -codebase " <> P.string transcriptDir)
                  <> "to do more work with it."])

  unless completed $ do
      unless keepTemp $ removeDirectoryRecursive transcriptDir
      PT.putPrettyLn (usage progName)
      Exit.exitWith (Exit.ExitFailure 1)

initialPath :: Path.Absolute
initialPath = Path.absoluteEmpty

launch
  :: FilePath
  -> (Config, IO ())
  -> Rt.Runtime Symbol
  -> Codebase.Codebase IO Symbol Ann
  -> [Either Input.Event Input.Input]
  -> IO ()
launch dir config rt code inputs =
  CommandLine.main dir defaultBaseLib initialPath config inputs rt code Version.gitDescribe

isMarkdown :: String -> Bool
isMarkdown md = case FP.takeExtension md of
  ".md" -> True
  ".markdown" -> True
  _ -> False

isDotU :: String -> Bool
isDotU file = FP.takeExtension file == ".u"

-- so we can do `ucm --help`, `ucm -help` or `ucm help` (I hate
-- having to remember which one is supported)
isFlag :: String -> String -> Bool
isFlag f arg = arg == f || arg == "-" ++ f || arg == "--" ++ f

getConfigFilePath :: Maybe FilePath -> IO FilePath
getConfigFilePath mcodepath = (FP.</> ".unisonConfig") <$> Codebase.getCodebaseDir mcodepath

defaultBaseLib :: Maybe RemoteNamespace
defaultBaseLib = rightMay $
  runParser VP.defaultBaseLib "version" (Text.pack Version.gitDescribe)

-- | load an existing codebase or exit.
getCodebaseOrExit :: CodebaseFormat -> Maybe Codebase.CodebasePath -> IO (IO (), Codebase.Codebase IO Symbol Ann)
getCodebaseOrExit cbFormat mdir = do
  let cbInit = cbInitFor cbFormat
  dir <- Codebase.getCodebaseDir mdir
  Codebase.openCodebase cbInit "main" dir >>= \case
    Left _errRequestedVersion -> do
      let
        sayNoCodebase = noCodebaseMsg <$> prettyExe <*> prettyDir <*> pure (fmap P.string mdir)
        suggestUpgrade = suggestUpgradeMessage <$> prettyExe <*> prettyDir <*> pure (fmap P.string mdir)
        prettyExe = P.text . Text.pack <$> getProgName
        prettyDir = P.string <$> canonicalizePath dir
      PT.putPrettyLn' =<< case cbFormat of
        V1 -> sayNoCodebase
        V2 -> FC.openCodebase dir >>= \case
          Left {} -> sayNoCodebase
          Right {} -> suggestUpgrade
      Exit.exitFailure
    Right x -> pure x
  where
    noCodebaseMsg :: _
    noCodebaseMsg executable prettyDir mdir =
      let secondLine =
            case mdir of
              Just dir ->
                "Run `" <> executable <> " -codebase " <> dir
                  <> " init` to create one, then try again!"
              Nothing ->
                "Run `" <> executable <> " init` to create one there,"
                  <> " then try again;"
                  <> " or `"
                  <> executable
                  <> " -codebase <dir>` to load a codebase from someplace else!"
       in P.lines
            [ "No codebase exists in " <> prettyDir <> ".",
              secondLine
            ]
    suggestUpgradeMessage exec resolvedDir specifiedDir =
      P.lines
        ( P.wrap
            <$> [ "I looked for a" <> prettyFmt V2 <> " codebase in " <> P.backticked' resolvedDir ","
                    <> "but found only a"
                    <> prettyFmt V1
                    <> "codebase there.",
                  "",
                  "You can use:"
                ]
        )
        <> P.newline
        <> P.bulleted
          ( P.wrap
              <$> [ P.backticked (P.wrap $ exec <> maybe mempty ("-codebase" <>) specifiedDir <> "upgrade-codebase")
                      <> "to update it to"
                      <> P.group (prettyFmt V2 <> ","),
                    P.backticked (P.wrap $ exec <> maybe mempty ("-codebase" <>) specifiedDir <> "init")
                      <> "to create a new"
                      <> prettyFmt V2
                      <> "codebase alongside it, or",
                    P.backticked (P.wrap $ exec <> "-codebase <dir>")
                      <> "to load a"
                      <> prettyFmt V2
                      <> "codebase from elsewhere."
                  ]
          )



    prettyFmt :: IsString s => CodebaseFormat -> P.Pretty s
    prettyFmt = \case V1 -> "v1"; V2 -> "v2"
