{-# LANGUAGE OverloadedStrings #-}

-- |
-- The Options module defines the command line options available when invoking unison
--
-- It is built using https://hackage.haskell.org/package/optparse-applicative
-- which has a pretty good guide that should explain everything in this module
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
options = info (options' <**> helper) mempty

codebaseHelp :: String
codebaseHelp = "The path to the codebase, defaults to the home directory"

options' :: Parser Command
options' =
  hsubparser
    ( command "version" (info (pure Version) (progDesc "print the version of unison"))
        <> command "init" (info (Init <$> optional (strOption (long "codebase" <> help codebaseHelp))) (progDesc initHelp))
        <> command "run" (info run (progDesc runHelp))
        <> command "transcript" (info transcript (progDesc transcriptHelp <> footerDoc transcriptFooter))
    )
    <|> (Launch <$> optional (strOption (long "codebase" <> help codebaseHelp)))
  where
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
  Run <$> optional (strOption (long "codebase" <> help codebaseHelp))
    <*> optional (strOption (long "file" <> help fileHelp))
    <*> (Stdin <$> switch (long "stdin" <> help pipeHelp))
    <*> strArgument (help mainHelp <> metavar ".mylib.mymain")
  where
    fileHelp = "the file containing .mylib.mymain - if not provided then the codebase codebase will be used"
    pipeHelp = "read the definition from stdin"
    mainHelp = "the main method"

transcript :: Parser Command
transcript =
  Transcript <$> optional (strOption (long "codebase" <> help codebaseHelp))
    <*> (Fork <$> switch (long "fork" <> help forkHelp))
    <*> (SaveCodebase <$> switch (long "save-codebase" <> help saveHelp))
    <*> some (strArgument (metavar "transcriptfiles..."))
  where
    forkHelp = "if set the transcript is executed in a copy of the current codebase"
    saveHelp = "if set the resulting codebase will be saved to a new directory, otherwise it will be deleted"
