{-# LANGUAGE OverloadedStrings #-}

module Unison.CommandLine.Welcome where

import Data.Sequence (singleton)
import System.Random (randomRIO)
import Unison.Codebase (Codebase)
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Editor.Input
import Unison.Codebase.Editor.RemoteRepo (ReadRemoteNamespace)
import Unison.Codebase.Path (Path)
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.SyncMode as SyncMode
import qualified Unison.Codebase.Verbosity as Verbosity
import Unison.NameSegment (NameSegment (NameSegment))
import Unison.Prelude
import qualified Unison.Util.Pretty as P
import Prelude hiding (readFile, writeFile)

data Welcome = Welcome
  { onboarding :: Onboarding, -- Onboarding States
    downloadBase :: DownloadBase,
    watchDir :: FilePath,
    unisonVersion :: String
  }

data DownloadBase
  = DownloadBase ReadRemoteNamespace
  | DontDownloadBase

-- Previously Created is different from Previously Onboarded because a user can
-- 1.) create a new codebase
-- 2.) decide not to go through the onboarding flow until later and exit
-- 3.) then reopen their blank codebase
data CodebaseInitStatus
  = NewlyCreatedCodebase -- Can transition to [Base, Author, Finished]
  | PreviouslyCreatedCodebase -- Can transition to [Base, Author, Finished, PreviouslyOnboarded].

data Onboarding
  = Init CodebaseInitStatus -- Can transition to [DownloadingBase, Author, Finished, PreviouslyOnboarded]
  | DownloadingBase ReadRemoteNamespace -- Can transition to [Author, Finished]
  | Author -- Can transition to [Finished]
  -- End States
  | Finished
  | PreviouslyOnboarded

welcome :: CodebaseInitStatus -> DownloadBase -> FilePath -> String -> Welcome
welcome initStatus downloadBase filePath unisonVersion =
  Welcome (Init initStatus) downloadBase filePath unisonVersion

pullBase :: ReadRemoteNamespace -> Either Event Input
pullBase ns =
  let seg = NameSegment "base"
      rootPath = Path.Path {Path.toSeq = singleton seg}
      abs = Path.Absolute {Path.unabsolute = rootPath}
      pullRemote = PullRemoteBranchI (Just ns) (Path.Path' {Path.unPath' = Left abs}) SyncMode.Complete PullWithHistory Verbosity.Silent
   in Right pullRemote

run :: Codebase IO v a -> Welcome -> IO [Either Event Input]
run codebase Welcome {onboarding = onboarding, downloadBase = downloadBase, watchDir = dir, unisonVersion = version} = do
  go onboarding []
  where
    go :: Onboarding -> [Either Event Input] -> IO [Either Event Input]
    go onboarding acc =
      case onboarding of
        Init NewlyCreatedCodebase -> do
          determineFirstStep downloadBase codebase >>= \step -> go step (headerMsg : acc)
          where
            headerMsg = toInput (header version)
        Init PreviouslyCreatedCodebase -> do
          go PreviouslyOnboarded (headerMsg : acc)
          where
            headerMsg = toInput (header version)
        DownloadingBase ns@(_, _, path) ->
          go Author ([pullBaseInput, downloadMsg] ++ acc)
          where
            downloadMsg = Right $ CreateMessage (downloading path)
            pullBaseInput = pullBase ns
        Author ->
          go Finished (authorMsg : acc)
          where
            authorMsg = toInput authorSuggestion
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
  isEmptyCodebase <- Codebase.getRootBranchExists codebase
  case downloadBase of
    DownloadBase ns
      | isEmptyCodebase ->
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
  P.lines
    [ P.group (P.wrap "🐣 Since this is a fresh codebase, let me download the base library for you." <> P.newline),
      P.wrap
        ( "🕐 Downloading"
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
        P.wrap ("You are running version: " <> P.bold (P.string version))
      ]

authorSuggestion :: P.Pretty P.ColorText
authorSuggestion =
  P.newline
    <> P.lines
      [ P.wrap "📜 🪶 You might want to set up your author information next.",
        P.wrap "Type" <> P.hiBlue " create.author" <> " to create an author for this codebase",
        P.group (P.newline <> P.wrap "Read about how to link your author to your code at"),
        P.wrap $ P.blue "https://www.unison-lang.org/learn/tooling/configuration/"
      ]

getStarted :: FilePath -> IO (P.Pretty P.ColorText)
getStarted dir = do
  earth <- (["🌎", "🌍", "🌏"] !!) <$> randomRIO (0, 2)

  pure $
    P.linesSpaced
      [ P.wrap "Get started:",
        P.indentN 2 $
          P.column2
            [ ("📖", "Type " <> P.hiBlue "help" <> " to list all commands, or " <> P.hiBlue "help <cmd>" <> " to view help for one command"),
              ("🎨", "Type " <> P.hiBlue "ui" <> " to open the Codebase UI in your default browser"),
              ("📚", "Read the official docs at " <> P.blue "https://www.unison-lang.org/learn/"),
              (earth, "Visit Unison Share at " <> P.blue "https://share.unison-lang.org" <> " to discover libraries"),
              ("👀", "I'm watching for changes to " <> P.bold ".u" <> " files under " <> (P.group . P.blue $ P.string dir))
            ]
      ]
