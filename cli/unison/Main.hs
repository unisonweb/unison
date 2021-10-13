{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Concurrent (newEmptyMVar, takeMVar)
import Control.Exception (evaluate)
import Control.Error.Safe (rightMay)
import Data.Configurator.Types (Config)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as Text
import qualified GHC.Conc
import System.Directory (canonicalizePath, getCurrentDirectory, removeDirectoryRecursive)
import System.Environment (getProgName)
import System.Info (os, arch)
import qualified System.Exit as Exit
import qualified System.FilePath as FP
import System.IO.Error (catchIOError)
import qualified System.IO.Temp as Temp
import qualified System.Path as Path
import Text.Megaparsec (runParser)
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Init (InitResult(..), InitError(..), CodebaseInitOptions(..), SpecifiedCodebase(..))
import qualified Unison.Codebase.Init as CodebaseInit
import qualified Unison.Codebase.Editor.Input as Input
import Unison.Codebase.Editor.RemoteRepo (ReadRemoteNamespace)
import qualified Unison.Codebase.Editor.VersionParser as VP
import Unison.Codebase.Execute (execute)
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.SqliteCodebase as SC
import qualified Unison.Codebase.TranscriptParser as TR
import Unison.CommandLine (plural', watchConfig)
import qualified Unison.CommandLine.Welcome as Welcome
import qualified Unison.CommandLine.Main as CommandLine
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import qualified Unison.Codebase.Runtime as Rt
import qualified Unison.PrettyTerminal as PT
import qualified Unison.Runtime.Interface as RTI
import Unison.Runtime.Exception (RuntimeExn(..))
import qualified Unison.Server.CodebaseServer as Server
import Unison.Symbol (Symbol)
import qualified Unison.Util.Pretty as P
import qualified Version
import UnliftIO.Directory ( getHomeDirectory )
import Compat ( installSignalHandlers )
import ArgParse
    ( UsageRenderer,
      GlobalOptions(GlobalOptions, codebasePathOption),
      Command(Launch, PrintVersion, Init, Run, Transcript),
      IsHeadless(WithCLI, Headless),
      ShouldSaveCodebase(..),
      ShouldForkCodebase(..),
      RunSource(..),
      ShouldDownloadBase (..),
      CodebasePathOption(..),
      parseCLIArgs )
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Unison.CommandLine.Welcome (CodebaseInitStatus(..))

main :: IO ()
main = do
  progName <- getProgName
  -- hSetBuffering stdout NoBuffering -- cool

  void installSignalHandlers
  (renderUsageInfo, globalOptions, command) <- parseCLIArgs progName Version.gitDescribeWithDate
  let GlobalOptions{codebasePathOption=mCodePathOption} = globalOptions
  let mcodepath = fmap codebasePathOptionToPath mCodePathOption

  currentDir <- getCurrentDirectory
  configFilePath <- getConfigFilePath mcodepath
  config <-
    catchIOError (watchConfig configFilePath) $ \_ ->
      Exit.die "Your .unisonConfig could not be loaded. Check that it's correct!"
  case command of
     PrintVersion ->
       putStrLn $ progName ++ " version: " ++ Version.gitDescribeWithDate
     Init -> do
      PT.putPrettyLn $
        P.callout
          "⚠️"
          (P.lines ["The Init command has been removed"
                  , P.newline
                  , P.wrap "Use --codebase-create to create a codebase at a specified location and open it:"
                  , P.indentN 2 (P.hiBlue "$ ucm --codebase-create myNewCodebase")
                  , "Running UCM without the --codebase-create flag: "
                  , P.indentN 2 (P.hiBlue "$ ucm")
                  , P.wrap ("will " <> P.bold "always" <> " create a codebase in your home directory if one does not already exist.")
                  ])

     Run (RunFromSymbol mainName) -> do
      ((closeCodebase, theCodebase),_) <- getCodebaseOrExit mCodePathOption
      runtime <- RTI.startRuntime Version.gitDescribeWithDate
      execute theCodebase runtime mainName
      closeCodebase
     Run (RunFromFile file mainName)
       | not (isDotU file) -> PT.putPrettyLn $ P.callout "⚠️" "Files must have a .u extension."
       | otherwise -> do
            e <- safeReadUtf8 file
            case e of
              Left _ -> PT.putPrettyLn $ P.callout "⚠️" "I couldn't find that file or it is for some reason unreadable."
              Right contents -> do
                ((closeCodebase, theCodebase), initRes) <- getCodebaseOrExit mCodePathOption
                rt <- RTI.startRuntime Version.gitDescribeWithDate
                let fileEvent = Input.UnisonFileChanged (Text.pack file) contents
                launch currentDir config rt theCodebase [Left fileEvent, Right $ Input.ExecuteI mainName, Right Input.QuitI] Nothing ShouldNotDownloadBase initRes
                closeCodebase
     Run (RunFromPipe mainName) -> do
      e <- safeReadUtf8StdIn
      case e of
        Left _ -> PT.putPrettyLn $ P.callout "⚠️" "I had trouble reading this input."
        Right contents -> do
          ((closeCodebase, theCodebase), initRes) <- getCodebaseOrExit mCodePathOption
          rt <- RTI.startRuntime Version.gitDescribeWithDate
          let fileEvent = Input.UnisonFileChanged (Text.pack "<standard input>") contents
          launch
            currentDir config rt theCodebase
            [Left fileEvent, Right $ Input.ExecuteI mainName, Right Input.QuitI]
            Nothing
            ShouldNotDownloadBase
            initRes
          closeCodebase
     Run (RunCompiled file) ->
       BL.readFile file >>= \bs ->
       try (evaluate $ RTI.decodeStandalone bs) >>= \case
         Left (PE _cs err) -> do
           PT.putPrettyLn . P.lines $
             [ P.wrap . P.text $
               "I was unable to parse this file as a compiled\
               \ program. The parser generated the following error:"
             , ""
             , P.indentN 2 $ err
             ]
         Right (Left err) ->
           PT.putPrettyLn . P.lines $
             [ P.wrap . P.text $
               "I was unable to parse this file as a compiled\
               \ program. The parser generated the following error:"
             , ""
             , P.indentN 2 . P.wrap $ P.string err
             ]
         Left _ -> do
           PT.putPrettyLn . P.wrap . P.text $
               "I was unable to parse this file as a compiled\
               \ program. The parser generated an unrecognized error."
         Right (Right (v,o,a,rf, w, sto))
           | not vmatch -> mismatchMsg
           | otherwise -> RTI.runStandalone sto w
           where
           vmatch = v == Text.pack Version.gitDescribeWithDate
                 && o == Text.pack os
                 && a == Text.pack arch
           ws s = P.wrap (P.text s)
           ifile | 'c':'u':'.':rest <- reverse file = reverse rest
                 | otherwise = file
           mismatchMsg = PT.putPrettyLn . P.lines $
             [ ws "I can't run this compiled program since \
               \it works with a different version of Unison \
               \than the one you're running."
             , ""
             , "Compiled file version"
             , P.indentN 4
               $ P.text v <> " for " <> P.text o <> " " <> P.text a
             , ""
             , "Your version"
             , P.indentN 4
               $ P.string Version.gitDescribeWithDate <> " for "
               <> P.string os <> " " <> P.string arch
             , ""
             , P.wrap $ "The program was compiled from hash "
                 <> (P.text $ "`" <> rf <> "`.")
                 <> "If you have that hash in your codebase,"
                 <> "you can do:"
             , ""
             , P.indentN 4
               $ ".> compile.output "
                 <> P.text rf <> " " <> P.string ifile
             , ""
             , P.wrap "to produce a new compiled program \
               \that matches your version of Unison."
             ]

     Transcript shouldFork shouldSaveCodebase transcriptFiles ->
       runTranscripts renderUsageInfo shouldFork shouldSaveCodebase mCodePathOption transcriptFiles
     Launch isHeadless codebaseServerOpts downloadBase -> do
       ((closeCodebase, theCodebase),initRes)  <- getCodebaseOrExit mCodePathOption
       runtime <- RTI.startRuntime Version.gitDescribeWithDate
       Server.startServer codebaseServerOpts runtime theCodebase $ \baseUrl -> do
         case isHeadless of
             Headless -> do
                 PT.putPrettyLn $
                   P.lines
                     [ "I've started the Codebase API server at",
                       P.string $ Server.urlFor Server.Api baseUrl,
                       "and the Codebase UI at",
                       P.string $ Server.urlFor Server.UI baseUrl
                     ]

                 PT.putPrettyLn $ P.string "Running the codebase manager headless with "
                     <> P.shown GHC.Conc.numCapabilities
                     <> " "
                     <> plural' GHC.Conc.numCapabilities "cpu" "cpus"
                     <> "."
                 mvar <- newEmptyMVar
                 takeMVar mvar
             WithCLI -> do
                 PT.putPrettyLn $ P.string "Now starting the Unison Codebase Manager (UCM)..."
                 launch currentDir config runtime theCodebase [] (Just baseUrl) downloadBase initRes
                 closeCodebase

prepareTranscriptDir :: ShouldForkCodebase -> Maybe CodebasePathOption -> IO FilePath
prepareTranscriptDir shouldFork mCodePathOption = do
  tmp <- Temp.getCanonicalTemporaryDirectory >>= (`Temp.createTempDirectory` "transcript")
  let cbInit = SC.init
  case shouldFork of
    UseFork -> do
      -- A forked codebase does not need to Create a codebase, because it already exists
      getCodebaseOrExit mCodePathOption
      path <- Codebase.getCodebaseDir (fmap codebasePathOptionToPath mCodePathOption)
      PT.putPrettyLn $ P.lines [
        P.wrap "Transcript will be run on a copy of the codebase at: ", "",
        P.indentN 2 (P.string path)
        ]
      Path.copyDir (CodebaseInit.codebasePath cbInit path) (CodebaseInit.codebasePath cbInit tmp)
    DontFork -> do
      PT.putPrettyLn . P.wrap $ "Transcript will be run on a new, empty codebase."
      void $ CodebaseInit.openNewUcmCodebaseOrExit cbInit "main.transcript" tmp
  pure tmp

runTranscripts'
  :: Maybe FilePath
  -> FilePath
  -> NonEmpty String
  -> IO Bool
runTranscripts' mcodepath transcriptDir args = do
  currentDir <- getCurrentDirectory
  let (markdownFiles, invalidArgs) = NonEmpty.partition isMarkdown args
  for_ markdownFiles $ \fileName -> do
    parsed <- TR.parseFile fileName
    case parsed of
      Left err ->
        PT.putPrettyLn $ P.callout "❓" (
          P.lines [
            P.indentN 2 "A parsing error occurred while reading a file:", "",
            P.indentN 2 $ P.string err])
      Right stanzas -> do
        configFilePath <- getConfigFilePath mcodepath
        -- We don't need to create a codebase through `getCodebaseOrExit` as we've already done so previously.
        ((closeCodebase, theCodebase),_) <- getCodebaseOrExit (Just (DontCreateCodebaseWhenMissing transcriptDir))
        mdOut <- TR.run transcriptDir configFilePath stanzas theCodebase
        closeCodebase
        let out = currentDir FP.</>
                   FP.addExtension (FP.dropExtension fileName ++ ".output")
                                   (FP.takeExtension fileName)
        writeUtf8 out mdOut
        putStrLn $ "💾  Wrote " <> out

  when (not . null $ invalidArgs) $ do
    PT.putPrettyLn $ P.callout "❓" (
      P.lines
        [ P.indentN 2 "Transcripts must have an .md or .markdown extension."
        , P.indentN 2 "Skipping the following invalid files:"
        , ""
        , P.bulleted $ fmap (P.bold . P.string . (<> "\n")) invalidArgs
        ])
  pure True

runTranscripts
  :: UsageRenderer
  -> ShouldForkCodebase
  -> ShouldSaveCodebase
  -> Maybe CodebasePathOption
  -> NonEmpty String
  -> IO ()
runTranscripts renderUsageInfo shouldFork shouldSaveTempCodebase mCodePathOption args = do
  progName <- getProgName
  transcriptDir <- prepareTranscriptDir shouldFork mCodePathOption
  completed <-
    runTranscripts' (Just transcriptDir) transcriptDir args
  case shouldSaveTempCodebase of
    DontSaveCodebase -> removeDirectoryRecursive transcriptDir
    SaveCodebase ->
      if completed
        then
          PT.putPrettyLn $
            P.callout "🌸" (
              P.lines [
                "I've finished running the transcript(s) in this codebase:", "",
                P.indentN 2 (P.string transcriptDir), "",
                P.wrap $ "You can run"
                      <> P.backticked (P.string progName <> " --codebase " <> P.string transcriptDir)
                      <> "to do more work with it."])
        else do
          putStrLn (renderUsageInfo $ Just "transcript")
          Exit.exitWith (Exit.ExitFailure 1)

initialPath :: Path.Absolute
initialPath = Path.absoluteEmpty

launch
  :: FilePath
  -> (Config, IO ())
  -> Rt.Runtime Symbol
  -> Codebase.Codebase IO Symbol Ann
  -> [Either Input.Event Input.Input]
  -> Maybe Server.BaseUrl
  -> ShouldDownloadBase
  -> InitResult IO Symbol Ann
  -> IO ()
launch dir config runtime codebase inputs serverBaseUrl shouldDownloadBase initResult =
  let
    downloadBase = case defaultBaseLib of
                      Just remoteNS | shouldDownloadBase == ShouldDownloadBase -> Welcome.DownloadBase remoteNS
                      _ -> Welcome.DontDownloadBase
    isNewCodebase = case initResult of
      CreatedCodebase{} -> NewlyCreatedCodebase
      _ -> PreviouslyCreatedCodebase

    (gitRef, _date) = Version.gitDescribe
    welcome = Welcome.welcome isNewCodebase downloadBase dir gitRef
  in
    CommandLine.main
      dir
      welcome
      initialPath
      config
      inputs
      runtime
      codebase
      serverBaseUrl

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

defaultBaseLib :: Maybe ReadRemoteNamespace
defaultBaseLib = rightMay $
  runParser VP.defaultBaseLib "version" (Text.pack gitRef)
  where
    (gitRef, _date) = Version.gitDescribe
-- (Unison.Codebase.Init.FinalizerAndCodebase IO Symbol Ann, InitResult IO Symbol Ann)
getCodebaseOrExit :: Maybe CodebasePathOption -> IO ((IO (), Codebase.Codebase IO Symbol Ann), InitResult IO Symbol Ann)
getCodebaseOrExit codebasePathOption = do
  initOptions <- argsToCodebaseInitOptions codebasePathOption
  CodebaseInit.openOrCreateCodebase SC.init "main" initOptions >>= \case
    Error dir error ->
      let
        message = do
          pDir <- prettyDir dir
          executableName <- P.text . Text.pack <$> getProgName

          case error of
            NoCodebaseFoundAtSpecifiedDir ->
              pure (P.lines
                [ "No codebase exists in " <> pDir <> ".",
                  "Run `" <> executableName <> " --codebase-create " <> P.string dir <> " to create one, then try again!"
                ])

            FoundV1Codebase ->
              pure (P.lines
                [ "Found a v1 codebase at " <> pDir <> ".",
                  "v1 codebases are no longer supported in this version of the UCM.",
                  "Please download version M2g of the UCM to upgrade."
                ])
            CouldntCreateCodebase errMessage ->
              pure errMessage
      in do
        msg <- message
        PT.putPrettyLn' msg
        Exit.exitFailure

    c@(CreatedCodebase dir cb) -> do
      pDir <- prettyDir dir
      PT.putPrettyLn' ""
      PT.putPrettyLn' . P.indentN 2 . P.wrap $ "I created a new codebase for you at" <> P.blue pDir
      pure (cb, c)

    o@(OpenedCodebase _ cb) ->
      pure (cb, o)

  where
    prettyDir dir = P.string <$> canonicalizePath dir

argsToCodebaseInitOptions :: Maybe CodebasePathOption -> IO CodebaseInit.CodebaseInitOptions
argsToCodebaseInitOptions pathOption =
  case pathOption of
    Just (CreateCodebaseWhenMissing path)     -> pure $ Specified (CreateWhenMissing path)
    Just (DontCreateCodebaseWhenMissing path) -> pure $ Specified (DontCreateWhenMissing path)
    Nothing                                   -> do Home <$> getHomeDirectory

codebasePathOptionToPath :: CodebasePathOption -> FilePath
codebasePathOptionToPath codebasePathOption =
  case codebasePathOption of
    CreateCodebaseWhenMissing p -> p
    DontCreateCodebaseWhenMissing p -> p
