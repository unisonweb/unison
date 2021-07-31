{-# LANGUAGE OverloadedStrings #-}

-- |
-- The Options module defines the command line options available when invoking unison
--
-- It is built using https://hackage.haskell.org/package/optparse-applicative
-- which has a pretty good guide that should explain everything in this module
module Unison.CommandLine.ArgParse where

import Control.Applicative ((<**>), (<|>), some)
import Options.Applicative
import Text.PrettyPrint.ANSI.Leijen ((<$$>), (<+>), (</>), Doc, bold, hardline)
import Data.Foldable

-- Unfortunately we can't use a global --codebase option so we have
-- to add it to all Commands that needs it
-- see https://github.com/pcapriotti/optparse-applicative/issues/294
type Codebase = Maybe FilePath

data Command
  = Launch Codebase
  | Version
  | Init Codebase
  | Run Codebase (Maybe FilePath) Stdin String
  | Transcript Codebase Fork SaveCodebase [FilePath]
  deriving (Show)

newtype Stdin = Stdin Bool
  deriving (Show)

newtype Fork = Fork Bool
  deriving (Show)

newtype SaveCodebase = SaveCodebase Bool
  deriving (Show)

options :: ParserInfo Command
options = info (options' <**> helper) (progDescDoc unisonHelp)

unisonHelp :: Maybe Doc
unisonHelp =
  Just $
    "Usage instructions for the Unison Codebase Manager"
      <> hardline
      <> hardline
      <> "To get started just run " <+> bold "unison" <+> "to enter interactive mode"
      <$$> mempty
      </> "Unison has various sub-commands (see Available commands) but by default will run in interactive mode"
      </> "Use" <+> bold "unison command --help" <+> "to show help for a command"
      <> hardline
      <> hardline
      <> "Most commands take the option" <+> bold "--codebase" <+> "which expects a path to a unison codebase"
      </> "If none is provided then the home directory is used"

codebaseHelp :: Maybe Doc
codebaseHelp = Just "The path to the codebase, defaults to the home directory"

options' :: Parser Command
options' =
  hsubparser commands <|> launchCommand
  where
    commands = 
      fold [ versionCommand
           , initCommand
           , runCommand
           , transcriptCommand
           ]

versionCommand :: Mod CommandFields Command
versionCommand = command "version" (info (pure Version) (progDesc "print the version of unison"))

initCommand :: Mod CommandFields Command
initCommand = command "init" (info (Init <$> optional (strOption (long "codebase" <> helpDoc codebaseHelp))) (progDesc initHelp))
  where
    initHelp = "Initialise a unison codebase"

runCommand :: Mod CommandFields Command
runCommand = command "run" (info run (progDescDoc runHelp))
  where
    runHelp = Just "Execute a definition from a file, stdin or the codebase"

transcriptCommand :: Mod CommandFields Command
transcriptCommand = command "transcript" (info transcript (progDesc transcriptHelp <> footerDoc transcriptFooter))
  where
    transcriptHelp = "Execute transcript markdown files"
    transcriptFooter =
      Just $
        "For each transcript file provided this executes the transcript and creates"
          <+> bold "mytranscript.output.md"
          <+> "if successful."
          </> "Exits after completion, and deletes the temporary directory created."
          </> "Multiple transcript files may be provided; they are processed in sequence"
          <+> "starting from the same codebase."

launchCommand :: Parser Command
launchCommand = (Launch <$> optional (strOption (long "codebase" <> helpDoc codebaseHelp)))

run :: Parser Command
run =
  Run <$> optional (strOption (long "codebase" <> helpDoc codebaseHelp))
    <*> optional (strOption (long "file" <> helpDoc fileHelp))
    <*> (Stdin <$> switch (long "stdin" <> helpDoc pipeHelp))
    <*> strArgument (help mainHelp <> metavar ".mylib.mymain")
  where
    fileHelp = Just $ "the file containing" <+> bold ".mylib.mymain" <+> "- if not provided then the codebase will be used"
    pipeHelp = Just $ "read the definition from stdin, usefull for piping to unison"
    mainHelp = "the main method"

transcript :: Parser Command
transcript =
  Transcript <$> optional (strOption (long "codebase" <> helpDoc codebaseHelp))
    <*> (Fork <$> switch (long "fork" <> helpDoc forkHelp))
    <*> (SaveCodebase <$> switch (long "save-codebase" <> helpDoc saveHelp))
    <*> some (strArgument (metavar "transcriptfiles..."))
  where
    forkHelp = Just "if set the transcript is executed in a copy of the current codebase"
    saveHelp = Just "if set the resulting codebase will be saved to a new directory, otherwise it will be deleted"
