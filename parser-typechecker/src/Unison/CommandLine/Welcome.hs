{-# LANGUAGE OverloadedStrings #-}
module Unison.CommandLine.Welcome where

import Unison.Prelude
import Unison.Codebase (Codebase)
import qualified Unison.Codebase as Codebase
import Prelude hiding (readFile, writeFile)
import qualified Unison.Util.Pretty as P
import System.Random (randomRIO)
import Unison.Codebase.Path (Path)

import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.SyncMode as SyncMode
import Unison.Codebase.Editor.Input
import Data.Sequence (singleton)
import Unison.NameSegment (NameSegment(NameSegment))

import Unison.Codebase.Editor.RemoteRepo (ReadRemoteNamespace)
-- import qualified Unison.Codebase.Editor.Input as Input
import qualified Unison.Codebase.Verbosity as Verbosity

-- IDEAS?

-- Notes:
-- Download base should be quieter - the printout is annoyingly large.
-- use more primitive IO functions for user input and git download.
-- UX issue / design constraint: if we use existing input / output architecture, how will we constrain the user into only entering their authorship info?
-- we don't want the user to have too much "freedom" when entering their author info.
-- Take a look at the transcript parser as an example of how to issue commands that is not in main
-- Not sure about the empyt line to advance mechanic - how might we handle that with input/actions 
-- Another idea:

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
  | PreviouslyCreatedCodebase -- Can transition to [Base, Author, Finished, PreviouslyOnboarded]. 

data Onboarding
  = Init CodebaseInitStatus -- Can transition to [DownloadingBase, Author, Finished, PreviouslyOnboarded]
  | DownloadingBase ReadRemoteNamespace -- Can transition to [Author, Finished]
  | Author -- Can transition to [Finished]
  -- End States
  | Finished
  | PreviouslyOnboarded

data DownloadBase 
  = DownloadBase ReadRemoteNamespace | DontDownloadBase

welcome :: DownloadBase -> Maybe FilePath -> FilePath -> String -> Welcome
welcome downloadBase newCodebasePath watchDir unisonVersion =
  case newCodebasePath of
    Just path -> Welcome (Init (NewlyCreatedCodebase path)) downloadBase newCodebasePath watchDir unisonVersion
    Nothing -> Welcome (Init PreviouslyCreatedCodebase) downloadBase newCodebasePath watchDir unisonVersion

pullBase :: ReadRemoteNamespace -> Either Event Input
pullBase _ns = let
    seg = NameSegment "base"
    rootPath = Path.Path { Path.toSeq = singleton seg }
    abs = Path.Absolute {Path.unabsolute = rootPath}
    pullRemote = PullRemoteBranchI (Just _ns) (Path.Path' {Path.unPath' = Left abs}) SyncMode.Complete Verbosity.Silent 
  in Right pullRemote

run :: Codebase IO v a -> Welcome -> IO [Either Event Input]
run codebase Welcome { onboarding = onboarding, downloadBase = downloadBase, watchDir = dir, unisonVersion = version } = do
  go onboarding []
  where
    go :: Onboarding -> [Either Event Input] -> IO [Either Event Input] 
    go onboarding acc  =
      case onboarding of
        Init (NewlyCreatedCodebase path) -> do 
            determineFirstStep downloadBase codebase >>= \step -> go step ([toInput (createdCodebase path), toInput (header version)] ++ acc) 
        Init PreviouslyCreatedCodebase -> do 
          determineFirstStep downloadBase codebase >>= \step -> go step (headerMsg : acc)
          where 
            headerMsg = toInput (header version)
        DownloadingBase ns@(_, _, path) ->  
          let 
            downloadMsg = Right $ CreateMessage (downloading path)
            pullBaseInput = pullBase ns
          in 
            go Author ([pullBaseInput, downloadMsg] ++ acc)
        Author -> 
          let 
            authorMsg = toInput authorSuggestion 
          in go Finished (authorMsg : acc)
        -- These are our two terminal Welcome conditions, at the end we reverse the order of the desired input commands otherwise they come out backwards
        Finished -> do
          startMsg <- getStarted dir 
          pure $ reverse (toInput startMsg : acc) 
        PreviouslyOnboarded -> do
          startMsg <- getStarted dir 
          pure $ reverse (toInput startMsg : acc) 

toInput :: P.Pretty P.ColorText -> Either Event Input 
toInput pretty = 
  Right $ CreateMessage pretty  

determineFirstStep :: DownloadBase -> Codebase IO v a -> IO Onboarding
determineFirstStep downloadBase codebase = do
  isBlankCodebase <- Codebase.isBlank codebase
  case downloadBase of
    DownloadBase ns | isBlankCodebase ->
      pure $ DownloadingBase ns
    _ ->
      pure PreviouslyOnboarded

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

authorSuggestion :: P.Pretty P.ColorText 
authorSuggestion = 
  P.indentN 2 . P.wrap $ "🖌 You might want to set up your author next." 
                         <> "Type" <> P.hiBlue "create.author" <> " to create an author for this codebase"
                         <> "Read about how to link your author to your code at " 
                         <> P.blue "https://www.unisonweb.org/docs/configuration/#setting-default-metadata-like-license-and-author"


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