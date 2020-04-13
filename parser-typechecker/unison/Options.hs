{-# LANGUAGE OverloadedStrings #-}

module Options where

import Control.Applicative ((<**>), (<|>), some)
import Data.Semigroup ((<>))
import Options.Applicative
  ( Parser,
    ParserInfo,
    command,
    footerDoc,
    help,
    helper,
    hsubparser,
    info,
    long,
    metavar,
    optional,
    progDesc,
    strArgument,
    strOption,
    switch,
  )
import Text.PrettyPrint.ANSI.Leijen ((<+>), (</>))

data Options
  = Options
      { codepath :: Maybe FilePath,
        cmd :: Command
      }
  deriving (Show)

data Command
  = Launch
  | Version
  | Init
  | Run (Maybe FilePath) Bool String
  | Transcript Bool Bool [FilePath]
  deriving (Show)

options :: ParserInfo Options
options = info (options' <**> helper) mempty

options' :: Parser Options
options' =
  Options <$> optional (strOption (long "codebase" <> help codebaseHelp))
    <*> ( hsubparser
            ( command "version" (info (pure Version) (progDesc "print the version of unison"))
                <> command "init" (info (pure Init) (progDesc initHelp))
                <> command "run" (info run (progDesc runHelp))
                <> command "transcript" (info transcript (progDesc transcriptHelp <> footerDoc transcriptFooter))
            )
            <|> (pure Launch)
        )
  where
    codebaseHelp = "The path to the codebase, defaults to the home directory"
    initHelp = "Initialise a unison codebase"
    runHelp = "Execute a definition"
    transcriptHelp = "Execute transcript markdown files"
    transcriptFooter =
      Just $
        "For each transcript file provided this executes the transcript and creates"
          <+> "`mytranscript.output.md` if successful. "
          </> "Exits after completion, and deletes the temporary directory created."
          </> "Multiple transcript files may be provided; they are processed in sequence"
          <+> "starting from the same codebase."

run :: Parser Command
run =
  Run <$> optional (strOption (long "file" <> help fileHelp))
    <*> switch (long "stdin" <> help pipeHelp)
    <*> strArgument (help mainHelp <> metavar ".mylib.mymain")
  where
    fileHelp = "the file containing .mylib.mymain - if not provided then the codebase codebase will be used"
    pipeHelp = "read the definition from stdin"
    mainHelp = "the main method"

transcript :: Parser Command
transcript =
  Transcript <$> switch (long "fork" <> help forkHelp)
    <*> switch (long "save-codebase" <> help saveHelp)
    <*> some (strArgument (metavar "transcriptfiles..."))
  where
    forkHelp = "if set the transcript is executed in a copy of the current codebase"
    saveHelp = "if set the resulting codebase will be saved to a new directory, otherwise it will be deleted"
