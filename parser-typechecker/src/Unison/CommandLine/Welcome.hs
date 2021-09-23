{-# LANGUAGE OverloadedStrings #-}
module Unison.CommandLine.Welcome where

import Unison.Prelude
import Unison.Codebase (Codebase)
import qualified Unison.Codebase as Codebase
import Prelude hiding (readFile, writeFile)
import qualified Unison.Util.Pretty as P
import qualified Unison.PrettyTerminal as PT
import System.Random (randomRIO)
import Unison.Codebase.Path (Path)
{-
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.SyncMode as SyncMode
import Unison.Codebase.Editor.Input (Input (..), Event)
import Data.Sequence (singleton)
import Unison.NameSegment (NameSegment(NameSegment))
-}
import Unison.Codebase.Editor.RemoteRepo (ReadRemoteNamespace)


-- IDEAS? 

-- 1)
-- * Refactor existing IO command loop out of main function - see notes in CommandLine.main 
-- * In Welcome.run; use existing interpreter to run commands 
-- * Implement a silencing mechanism

-- 2)
-- * Run Codebase.importRemoteBranch directly in Welcome.runAction
-- * Merge import result into .base

-- WELCOME
data Welcome = Welcome 
  { onboarding :: Onboarding -- Onboarding States
  , downloadBase :: DownloadBase
  , newCodebasePath :: Maybe FilePath
  , watchDir :: FilePath 
  , unisonVersion :: String 
  }

-- ONBOARDING
data CodebaseInitStatus
  = NewlyCreatedCodebase FilePath -- Can transition to [Base, Author, Finished]
  | PreviouslyCreatedCodebase -- Can transition to [Base, Author, Finished, PreviouslyOnboarded]. TODO: Show which codebase path was actually opened...

data Onboarding 
  = Init CodebaseInitStatus -- Can transition to [Base, Author, Finished, PreviouslyOnboarded]
  | Base BaseSteps -- Can transition to [Author, Finished]
  | Author -- Can traisition to [Finished]
  -- End States
  | Finished
  | PreviouslyOnboarded

-- ucm start
--   create codebase
-- ....
-- onboarding
--    print out that we just created a codebase 56 steps earlier
--    figureout if we need to download base ... Needed a codebase and base

-- ucm start
--   codebase already exists
-- ....
-- onboarding
--     figureout if we need to download base ... Needs base, but had an existing codebase

-- ucm start
--   codebase exists
-- ....
-- onboarding
--     this is my 100th time and i've got a codebase, and author and base -> PreviouslyOnboarded

data BaseSteps 
  = DownloadingBase ReadRemoteNamespace
  | DownloadBaseFailed ReadRemoteNamespace Text
  | DownloadBaseSucceeded ReadRemoteNamespace

data DownloadBase = DownloadBase ReadRemoteNamespace | DontDownloadBase

welcome :: DownloadBase -> Maybe FilePath -> FilePath -> String -> Welcome
welcome downloadBase newCodebasePath watchDir unisonVersion =
  case newCodebasePath of 
    Just path -> Welcome (Init (NewlyCreatedCodebase path)) downloadBase newCodebasePath watchDir unisonVersion
    Nothing -> Welcome (Init PreviouslyCreatedCodebase) downloadBase newCodebasePath watchDir unisonVersion
  
run :: Codebase IO v a -> Welcome -> IO ()
run codebase Welcome { onboarding = onboarding, downloadBase = downloadBase, watchDir = dir, unisonVersion = version } = do
  go onboarding
  where
    go :: Onboarding -> IO ()
    go onboarding = 
      case onboarding of 
        Init (NewlyCreatedCodebase path)  -> do
          PT.putPrettyLn (header version)
          PT.putPrettyLn (createdCodebase path)

          determineFirstStep >>= go
        Init PreviouslyCreatedCodebase -> do
          PT.putPrettyLn (header version)

          determineFirstStep >>= go
        Base (DownloadingBase ns@(_, _, path)) -> do
          PT.putPrettyLn $ downloading path
          res <- pullBase ns
          case res of
            Right _ ->
              go $ Base $ DownloadBaseSucceeded ns
            Left errorMsg ->
              go $ Base $ DownloadBaseFailed ns errorMsg

        Base (DownloadBaseSucceeded _) -> do 
          PT.putPrettyLn $ P.lines [
              P.wrap "✅ Success! The base library is the Unison standard library that includes",
              P.wrap "core types and functions to write Unison code."
            ]
          -- getStarted dir >>= PT.putPrettyLn
          
          go Author 
        Base (DownloadBaseFailed _ _) -> do
          PT.putPrettyLn "Download Failed"
          getStarted dir >>= PT.putPrettyLn

        Author -> do 
          PT.putPrettyLn "Enter your author!"
          go Finished

        Finished ->
          getStarted dir >>= PT.putPrettyLn

        PreviouslyOnboarded ->
          getStarted dir >>= PT.putPrettyLn

    determineFirstStep :: IO Onboarding
    determineFirstStep = do
      isBlankCodebase <- Codebase.isBlank codebase
      case downloadBase of
        DownloadBase ns | isBlankCodebase ->
          pure $ Base (DownloadingBase ns)
        _ ->
          pure $ PreviouslyOnboarded



-- HELPERS

pullBase :: ReadRemoteNamespace -> IO (Either Text ())
pullBase _ns =
  {-
  let 
    seg = NameSegment "base"
    rootPath = Path.Path { Path.toSeq = singleton seg }
    abs = Path.Absolute {Path.unabsolute = rootPath}
  in do
    -}
    pure $ Right ()
  -- PullRemoteBranchI (Just ns) (Path.Path' {Path.unPath' = Left abs}) SyncMode.Complete 

asciiartUnison :: P.Pretty P.ColorText
asciiartUnison =
  P.red " _____"
    <> P.hiYellow "     _             "
    <> P.newline
    <> P.red "|  |  |"
    <> P.hiRed "___"
    <> P.hiYellow "|_|"
    <> P.hiGreen "___ "
    <> P.cyan "___ "
    <> P.purple "___ "
    <> P.newline
    <> P.red "|  |  |   "
    <> P.hiYellow "| |"
    <> P.hiGreen "_ -"
    <> P.cyan "| . |"
    <> P.purple "   |"
    <> P.newline
    <> P.red "|_____|"
    <> P.hiRed "_|_"
    <> P.hiYellow "|_|"
    <> P.hiGreen "___"
    <> P.cyan "|___|"
    <> P.purple "_|_|"


downloading :: Path -> P.Pretty P.ColorText
downloading path =
  P.indentN 2 $ P.lines
    [ P.newline <> P.newline,
      P.wrap
        ("🕐 Downloading"
            <> P.blue (P.string (show path))
            <> "of the"
            <> P.bold "base library"
            <> "into"
            <> P.group (P.blue ".base" <> ", this may take a minute...")
        )
    ]


header :: String -> P.Pretty P.ColorText
header version =
  asciiartUnison
    <> P.newline
    <> P.newline
    <> P.linesSpaced
      [ P.wrap "👋 Welcome to Unison!",
        P.wrap ("You are running version: " <> P.bold (P.string version)) <> P.newline
      ]

createdCodebase :: FilePath -> P.Pretty P.ColorText
createdCodebase dir =
  P.indentN 2 . P.wrap $ "I created a new codebase for you at" <> P.blue (P.string dir)

getStarted :: FilePath -> IO (P.Pretty P.ColorText)
getStarted dir = do
  earth <- (["🌎", "🌍", "🌏"] !!) <$> randomRIO (0, 2)

  pure $ P.linesSpaced [
    P.wrap "Get started:",
    P.indentN 2 $ P.column2
      [ ("📖", "Type " <> P.hiBlue "help" <> " to list all commands, or " <> P.hiBlue "help <cmd>" <> " to view help for one command"),
        ("🎨", "Type " <> P.hiBlue "ui" <> " to open the Codebase UI in your default browser"),
        ("📚", "Read the official docs at " <> P.blue "https://unisonweb.org/docs"),
        (earth, "Visit Unison Share at " <> P.blue "https://share.unison-lang.org" <> " to discover libraries"),
        ("👀", "I'm watching for changes to " <> P.bold ".u" <> " files under " <> (P.group . P.blue $ P.string dir))
      ]
    ]