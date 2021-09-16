{-# LANGUAGE OverloadedStrings #-}
module Unison.CommandLine.Welcome where

import Unison.Prelude
import Unison.Codebase (Codebase)
import qualified Unison.Codebase as Codebase
import Prelude hiding (readFile, writeFile)
import qualified Unison.Util.Pretty as P
import System.Random (randomRIO)
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.SyncMode as SyncMode
import Unison.Codebase.Editor.Input (Input (..), Event)
import Data.Sequence (singleton)
import Unison.NameSegment (NameSegment(NameSegment))
import Unison.Codebase.Editor.RemoteRepo (ReadRemoteNamespace)

-- Should Welcome include whether or not the codebase was created just now?

data DownloadBase = DownloadBase ReadRemoteNamespace | DontDownloadBase

data Welcome = Welcome 
  { downloadBase :: DownloadBase
  , watchDir :: FilePath 
  , unisonVersion :: String 
  }

welcome :: Codebase IO v a -> Welcome -> IO ([Either Event Input], P.Pretty P.ColorText)
welcome codebase welcome' = do
  let Welcome{downloadBase=downloadBase, watchDir=dir, unisonVersion=version} = welcome' 
  welcomeMsg <- welcomeMessage dir version
  isBlankCodebase <- Codebase.isBlank codebase
  pure $ case downloadBase of
    DownloadBase ns@(_, _, path) | isBlankCodebase ->
      let
        cmd =
          Right (pullBase ns)

        baseVersion =
          P.string (show path)
          
        downloadMsg =
          P.lines [ P.newline <> P.newline
                  , P.wrap ("🕐 Downloading"
                              <> P.blue baseVersion
                              <> "of the"
                              <> P.bold "base library"
                              <> "into"
                              <> P.group (P.blue ".base" <> ", this may take a minute..."))
                  ]
      in
      ([cmd], welcomeMsg <> downloadMsg)
    _ ->
      ([], welcomeMsg)

welcomeMessage :: FilePath -> String -> IO (P.Pretty P.ColorText)
welcomeMessage dir version = do
  earth <- (["🌎", "🌍", "🌏"] !!) <$> randomRIO (0, 2)

  pure $
    asciiartUnison
      <> P.newline
      <> P.newline
      <> P.linesSpaced
        [ P.wrap "👋 Welcome to Unison!",
          P.wrap ("You are running version: " <> P.bold (P.string version)) <> P.newline,
          P.wrap "Get started:",
          P.indentN
            2
            ( P.column2
                [ ("📖", "Type " <> P.hiBlue "help" <> " to list all commands, or " <> P.hiBlue "help <cmd>" <> " to view help for one command"),
                  ("🎨", "Type " <> P.hiBlue "ui" <> " to open the Codebase UI in your default browser"),
                  ("📚", "Read the official docs at " <> P.blue "https://unisonweb.org/docs"),
                  (earth, "Visit Unison Share at " <> P.blue "https://share.unison-lang.org" <> " to discover libraries"),
                  ("👀", "I'm watching for changes to " <> P.bold ".u" <> " files under " <> (P.group . P.blue $ P.string dir))
                ]
            )
        ]

pullBase :: ReadRemoteNamespace -> Input
pullBase ns = do 
  let 
    seg = NameSegment "base"
    rootPath = Path.Path { Path.toSeq = singleton seg }
    abs = Path.Absolute {Path.unabsolute = rootPath}
  PullRemoteBranchI (Just ns) (Path.Path' {Path.unPath' = Left abs}) SyncMode.Complete 

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

