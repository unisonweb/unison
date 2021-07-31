{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- | This module handles parsing CLI arguments into 'Command's.
-- See the excellent documentation at https://hackage.haskell.org/package/optparse-applicative
module ArgParse where

import Options.Applicative
    ( optional,
      (<**>),
      action,
      columns,
      command,
      flag,
      flag',
      footerDoc,
      help,
      info,
      long,
      metavar,
      prefs,
      progDesc,
      showHelpOnError,
      strArgument,
      strOption,
      customExecParser,
      helper,
      hsubparser,
      parserFailure,
      renderFailure,
      Alternative(some, (<|>)),
      CommandFields,
      Mod,
      ParseError(ShowHelpText),
      Parser,
      ParserInfo,
      ParserPrefs, fullDesc, headerDoc )
import Options.Applicative.Help ( (<+>), bold )
import Data.Foldable ( Foldable(fold) )
import qualified Options.Applicative.Help.Pretty as P
import qualified Unison.PrettyTerminal as PT
import qualified Data.List as List
import Unison.Util.Pretty (Width(..))

-- The name of a symbol to execute.
type SymbolName = String

-- | Valid ways to provide source code to the run command
data RunSource =
    RunFromPipe SymbolName
  | RunFromSymbol SymbolName
  | RunFromFile FilePath SymbolName
  deriving (Show)

data ShouldForkCodebase
    = UseFork
    | DontFork
    deriving (Show, Eq)

data ShouldSaveCodebase
    = SaveCodebase
    | DontSaveCodebase
    deriving (Show, Eq)

data IsHeadless = Headless | WithCLI
  deriving (Show, Eq)

data Command
  = Launch IsHeadless
  | PrintVersion
  | Init
  | Run RunSource
  | Transcript ShouldForkCodebase ShouldSaveCodebase [FilePath]
  | UpgradeCodebase
  deriving (Show)

data CodebaseFormat
    = V1
    | V2
    deriving (Show, Eq)

data GlobalOptions = GlobalOptions
  { codebasePath :: Maybe FilePath
  , codebaseFormat :: CodebaseFormat
  } deriving (Show)

rootParserInfo :: String -> String -> ParserInfo (GlobalOptions, Command)
rootParserInfo progName version =
    info ((,) <$> globalOptionsParser <*> commandParser <**> helper)
         (  fullDesc
         <> headerDoc (Just $ unisonHelp progName version))

type UsageRenderer =
    Maybe String -- ^ Optional sub-command to render help for
    -> String

parseCLIArgs :: String -> String -> IO (UsageRenderer, GlobalOptions, Command)
parseCLIArgs progName version = do
  (Width cols) <- PT.getAvailableWidth
  let parserInfo = rootParserInfo progName version
  let preferences = prefs $ showHelpOnError <> columns cols
  (globalOptions, command) <- customExecParser preferences parserInfo
  let usage = renderUsage progName parserInfo preferences
  pure $ (usage, globalOptions, command)

renderUsage :: String -> ParserInfo a -> ParserPrefs -> Maybe String -> String
renderUsage programName pInfo preferences subCommand =
    let showHelpFailure = parserFailure preferences pInfo (ShowHelpText subCommand) mempty
        (helpText, _exitCode) = renderFailure showHelpFailure programName
     in helpText

commandParser :: Parser Command
commandParser =
  hsubparser commands <|> launchParser
  where
    commands =
      fold [ versionCommand
           , initCommand
           , runSymbolCommand
           , runFileCommand
           , runPipeCommand
           , transcriptCommand
           , transcriptForkCommand
           , upgradeCodebaseCommand
           , launchHeadlessCommand
           ]

globalOptionsParser :: Parser GlobalOptions
globalOptionsParser = do -- ApplicativeDo
    codebasePath <- codebasePathParser
    codebaseFormat <- codebaseFormatParser
    pure GlobalOptions{..}

codebasePathParser :: Parser (Maybe FilePath)
codebasePathParser =
    optional . strOption $
         long "codebase"
      <> metavar "path/to/codebase"
      <> help "The path to the codebase, defaults to the home directory"

codebaseFormatParser :: Parser CodebaseFormat
codebaseFormatParser =
        flag' V1 (long "old-codebase" <> help "Use a v1 codebase on startup.")
    <|> flag' V2 (long "new-codebase" <> help "Use a v2 codebase on startup.")
    <|> pure V2

launchHeadlessCommand :: Mod CommandFields Command
launchHeadlessCommand =
    command "headless" (info launchHeadlessParser (progDesc headlessHelp))
  where
    headlessHelp = "Runs the codebase server without the command-line interface."

versionCommand :: Mod CommandFields Command
versionCommand = command "version" (info versionParser (fullDesc <> progDesc "Print the version of unison you're running"))

initCommand :: Mod CommandFields Command
initCommand = command "init" (info initParser (progDesc initHelp))
  where
    initHelp = "Initialise a unison codebase"

runSymbolCommand :: Mod CommandFields Command
runSymbolCommand =
    command "run" (info runSymbolParser (fullDesc <> progDesc "Execute a definition from the codebase"))

runFileCommand :: Mod CommandFields Command
runFileCommand =
    command "run.file" (info runFileParser (fullDesc <> progDesc "Execute a definition from a file"))

runPipeCommand :: Mod CommandFields Command
runPipeCommand =
    command "run.pipe" (info runPipeParser (fullDesc <> progDesc "Execute code from stdin"))

transcriptCommand :: Mod CommandFields Command
transcriptCommand =
    command "transcript" (info transcriptParser (fullDesc <> progDesc transcriptHelp <> footerDoc transcriptFooter))
  where
    transcriptHelp = "Execute transcript markdown files"
    transcriptFooter =
      Just . fold . List.intersperse P.line $
        [ "For each <transcript>.md file provided this executes the transcript and creates" <+> bold "<transcript>.output.md" <+> "if successful."
        , "Exits after completion, and deletes the temporary directory created, unless --save-codebase is provided"
        , "Multiple transcript files may be provided; they are processed in sequence" <+> "starting from the same codebase."
        ]

transcriptForkCommand :: Mod CommandFields Command
transcriptForkCommand =
    command "transcript.fork" (info transcriptForkParser (fullDesc <> progDesc transcriptHelp <> footerDoc transcriptFooter))
  where
    transcriptHelp = "Execute transcript markdown files in a sandboxed codebase"
    transcriptFooter =
      Just . fold . List.intersperse P.line $
        [ "For each <transcript>.md file provided this executes the transcript in a sandbox codebase and creates" <+> bold "<transcript>.output.md" <+> "if successful."
        , "Exits after completion, and deletes the temporary directory created, unless --save-codebase is provided"
        , "Multiple transcript files may be provided; they are processed in sequence" <+> "starting from the same codebase."
        ]

upgradeCodebaseCommand :: Mod CommandFields Command
upgradeCodebaseCommand =
    command "upgrade-codebase" (info (pure UpgradeCodebase) (fullDesc <> progDesc "Upgrades a v1 codebase to a v2 codebase"))

launchParser :: Parser Command
launchParser = pure (Launch WithCLI)

launchHeadlessParser :: Parser Command
launchHeadlessParser = pure (Launch Headless)

initParser :: Parser Command
initParser = pure Init

versionParser :: Parser Command
versionParser = pure PrintVersion

runSymbolParser :: Parser Command
runSymbolParser =
  Run . RunFromSymbol <$> strArgument (metavar "SYMBOL")

runFileParser :: Parser Command
runFileParser = do -- ApplicativeDo
  pathTofile <- fileArgument "path/to/file"
  symbolName <- strArgument (metavar "SYMBOL")
  pure $ Run (RunFromFile pathTofile symbolName)

runPipeParser :: Parser Command
runPipeParser =
  Run . RunFromPipe <$> strArgument (metavar "SYMBOL")

saveCodebaseFlag :: Parser ShouldSaveCodebase
saveCodebaseFlag = flag DontSaveCodebase SaveCodebase (long "save-codebase" <> help saveHelp)
  where
    saveHelp = "if set the resulting codebase will be saved to a new directory, otherwise it will be deleted"

fileArgument :: String -> Parser FilePath
fileArgument varName =
    strArgument (  metavar varName
                <> action "file" -- Autocomplete file names
                )

transcriptParser :: Parser Command
transcriptParser = do -- ApplicativeDo
  shouldSaveCodebase <- saveCodebaseFlag
  files <- some (fileArgument "FILES...")
  pure (Transcript DontFork shouldSaveCodebase files)

transcriptForkParser :: Parser Command
transcriptForkParser = do -- ApplicativeDo
  shouldSaveCodebase <- saveCodebaseFlag
  files <- some (fileArgument "FILES...")
  pure (Transcript UseFork shouldSaveCodebase files)

unisonHelp :: String -> String -> P.Doc
unisonHelp (P.text -> executable) (P.text -> version) =
  fold . List.intersperse P.line $
    [ P.empty
    , "🌻"
    , P.empty
    , P.bold "Usage instructions for the Unison Codebase Manager"
    , "You are running version:" <+> version
    , P.empty
    , "To get started just run" <+> P.bold executable
    , P.empty
    , "Use" <+> P.bold (executable <+> "[command] --help") <+> "to show help for a command."
    ]
