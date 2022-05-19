{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- | This module handles parsing CLI arguments into 'Command's.
-- See the excellent documentation at https://hackage.haskell.org/package/optparse-applicative
module ArgParse where

import Control.Applicative (Alternative (many, (<|>)), Applicative (liftA2), optional)
import Data.Foldable (Foldable (fold))
import Data.Functor ((<&>))
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Options.Applicative
  ( CommandFields,
    Mod,
    ParseError (ShowHelpText),
    Parser,
    ParserInfo,
    ParserPrefs,
    action,
    auto,
    columns,
    command,
    customExecParser,
    flag,
    footerDoc,
    fullDesc,
    headerDoc,
    help,
    helpShowGlobals,
    helper,
    hsubparser,
    info,
    infoOption,
    long,
    metavar,
    option,
    parserFailure,
    prefs,
    progDesc,
    renderFailure,
    short,
    showHelpOnError,
    strArgument,
    strOption,
  )
import Options.Applicative.Help (bold, (<+>))
import qualified Options.Applicative.Help.Pretty as P
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import qualified Unison.PrettyTerminal as PT
import Unison.Server.CodebaseServer (CodebaseServerOpts (..))
import qualified Unison.Server.CodebaseServer as Server
import Unison.Util.Pretty (Width (..))

-- The name of a symbol to execute.
type SymbolName = String

-- | Valid ways to provide source code to the run command
data RunSource
  = RunFromPipe SymbolName
  | RunFromSymbol SymbolName
  | RunFromFile FilePath SymbolName
  | RunCompiled FilePath
  deriving (Show, Eq)

data ShouldForkCodebase
  = UseFork
  | DontFork
  deriving (Show, Eq)

data ShouldDownloadBase
  = ShouldDownloadBase
  | ShouldNotDownloadBase
  deriving (Show, Eq)

data ShouldSaveCodebase
  = SaveCodebase
  | DontSaveCodebase
  deriving (Show, Eq)

data CodebasePathOption
  = CreateCodebaseWhenMissing FilePath
  | DontCreateCodebaseWhenMissing FilePath
  deriving (Show, Eq)

data IsHeadless = Headless | WithCLI
  deriving (Show, Eq)

-- | Represents commands the cli can run.
--
-- Note that this is not one-to-one with command-parsers since some are simple variants.
-- E.g. run, run.file, run.pipe
data Command
  = Launch IsHeadless CodebaseServerOpts ShouldDownloadBase
  | PrintVersion
  | -- @deprecated in trunk after M2g. Remove the Init command completely after M2h has been released
    Init
  | Run RunSource [String]
  | Transcript ShouldForkCodebase ShouldSaveCodebase (NonEmpty FilePath)
  deriving (Show, Eq)

-- | Options shared by sufficiently many subcommands.
data GlobalOptions = GlobalOptions
  { codebasePathOption :: Maybe CodebasePathOption
  }
  deriving (Show, Eq)

-- | The root-level 'ParserInfo'.
rootParserInfo :: String -> String -> CodebaseServerOpts -> ParserInfo (GlobalOptions, Command)
rootParserInfo progName version envOpts =
  info
    (helper <*> versionOptionParser progName version <*> ((,) <$> globalOptionsParser <*> commandParser envOpts))
    ( fullDesc
        <> headerDoc (Just $ unisonHelp progName version)
    )

type UsageRenderer =
  -- | Optional sub-command to render help for
  Maybe String ->
  String

-- | Parse the command description, options, and usage information from provided cli arguments.
parseCLIArgs :: String -> String -> IO (UsageRenderer, GlobalOptions, Command)
parseCLIArgs progName version = do
  (Width cols) <- PT.getAvailableWidth
  envOpts <- codebaseServerOptsFromEnv
  let parserInfo = rootParserInfo progName version envOpts
  let preferences = prefs $ showHelpOnError <> helpShowGlobals <> columns cols
  let usage = renderUsage progName parserInfo preferences
  (globalOptions, command) <- customExecParser preferences parserInfo
  pure $ (usage, globalOptions, command)

-- | Load default options from environment variables.
codebaseServerOptsFromEnv :: IO CodebaseServerOpts
codebaseServerOptsFromEnv = do
  token <- lookupEnv Server.ucmTokenVar
  host <- lookupEnv Server.ucmHostVar
  port <- lookupEnv Server.ucmPortVar <&> (>>= readMaybe)
  codebaseUIPath <- lookupEnv Server.ucmUIVar
  pure $ CodebaseServerOpts {..}

-- | Purely renders the full help summary for the CLI, or an optional subcommand.
renderUsage :: String -> ParserInfo a -> ParserPrefs -> Maybe String -> String
renderUsage programName pInfo preferences subCommand =
  let showHelpFailure = parserFailure preferences pInfo (ShowHelpText subCommand) mempty
      (helpText, _exitCode) = renderFailure showHelpFailure programName
   in helpText

versionCommand :: Mod CommandFields Command
versionCommand = command "version" (info versionParser (fullDesc <> progDesc "Print the version of unison you're running"))

initCommand :: Mod CommandFields Command
initCommand = command "init" (info initParser (progDesc initHelp))
  where
    initHelp =
      "This command is has been removed. Use --codebase-create instead to create a codebase in the specified directory when starting the UCM."

runDesc :: String -> String -> String
runDesc cmd location =
  "Execute a definition from " <> location <> ", passing on the provided arguments. "
    <> " To pass flags to your program, use `"
    <> cmd
    <> " -- --my-flag`"

runSymbolCommand :: Mod CommandFields Command
runSymbolCommand =
  command "run" (info runSymbolParser (fullDesc <> progDesc help))
  where
    help =
      "Execute a definition from the codebase, passing on the provided arguments. "
        <> " To pass flags to your program, use `run <symbol> -- --my-flag`"

runFileCommand :: Mod CommandFields Command
runFileCommand =
  command "run.file" (info runFileParser (fullDesc <> progDesc help))
  where
    help =
      "Execute a definition from a file, passing on the provided arguments. "
        <> " To pass flags to your program, use `run.file <file> -- --my-flag`"

runPipeCommand :: Mod CommandFields Command
runPipeCommand =
  command "run.pipe" (info runPipeParser (fullDesc <> progDesc help))
  where
    help =
      "Execute a definition from stdin, passing on the provided arguments. "
        <> " To pass flags to your program, use `run -- --my-flag`"

runCompiledCommand :: Mod CommandFields Command
runCompiledCommand =
  command "run.compiled" (info runCompiledParser (fullDesc <> progDesc help))
  where
    help =
      "Execute a definition from a previously compiled file, passing on the provided arguments. "
        <> " To pass flags to your program, use `run <file> -- --my-flag`"

transcriptCommand :: Mod CommandFields Command
transcriptCommand =
  command "transcript" (info transcriptParser (fullDesc <> progDesc transcriptHelp <> footerDoc transcriptFooter))
  where
    transcriptHelp = "Execute transcript markdown files"
    transcriptFooter =
      Just . fold . List.intersperse P.line $
        [ "For each <transcript>.md file provided this executes the transcript and creates" <+> bold "<transcript>.output.md" <+> "if successful.",
          "Exits after completion, and deletes the temporary directory created, unless --save-codebase is provided",
          "Multiple transcript files may be provided; they are processed in sequence" <+> "starting from the same codebase."
        ]

transcriptForkCommand :: Mod CommandFields Command
transcriptForkCommand =
  command "transcript.fork" (info transcriptForkParser (fullDesc <> progDesc transcriptHelp <> footerDoc transcriptFooter))
  where
    transcriptHelp = "Execute transcript markdown files in a sandboxed codebase"
    transcriptFooter =
      Just . fold . List.intersperse P.line $
        [ "For each <transcript>.md file provided this executes the transcript in a sandbox codebase and creates" <+> bold "<transcript>.output.md" <+> "if successful.",
          "Exits after completion, and deletes the temporary directory created, unless --save-codebase is provided",
          "Multiple transcript files may be provided; they are processed in sequence" <+> "starting from the same codebase."
        ]

commandParser :: CodebaseServerOpts -> Parser Command
commandParser envOpts =
  hsubparser commands <|> launchParser envOpts WithCLI
  where
    commands =
      fold
        [ versionCommand,
          initCommand,
          runSymbolCommand,
          runCompiledCommand,
          runFileCommand,
          runPipeCommand,
          transcriptCommand,
          transcriptForkCommand,
          launchHeadlessCommand envOpts
        ]

globalOptionsParser :: Parser GlobalOptions
globalOptionsParser = do
  -- ApplicativeDo
  codebasePathOption <- codebasePathParser <|> codebaseCreateParser

  pure GlobalOptions {codebasePathOption = codebasePathOption}

codebasePathParser :: Parser (Maybe CodebasePathOption)
codebasePathParser = do
  optString <-
    optional . strOption $
      long "codebase"
        <> short 'c'
        <> metavar "CODEBASE/PATH"
        <> help "The path to an existing codebase"
  pure (fmap DontCreateCodebaseWhenMissing optString)

codebaseCreateParser :: Parser (Maybe CodebasePathOption)
codebaseCreateParser = do
  path <-
    optional . strOption $
      long "codebase-create"
        <> short 'C'
        <> metavar "CODEBASE/PATH"
        <> help "The path to a new or existing codebase (one will be created if there isn't one)"
  pure (fmap CreateCodebaseWhenMissing path)

versionOptionParser :: String -> String -> Parser (a -> a)
versionOptionParser progName version =
  infoOption (progName <> " version: " <> version) (short 'v' <> long "version" <> help "Show version")

launchHeadlessCommand :: CodebaseServerOpts -> Mod CommandFields Command
launchHeadlessCommand envOpts =
  command "headless" (info (launchParser envOpts Headless) (progDesc headlessHelp))
  where
    headlessHelp = "Runs the codebase server without the command-line interface."

codebaseServerOptsParser :: CodebaseServerOpts -> Parser CodebaseServerOpts
codebaseServerOptsParser envOpts = do
  -- ApplicativeDo
  cliToken <- tokenFlag <|> pure (token envOpts)
  cliHost <- hostFlag <|> pure (host envOpts)
  cliPort <- portFlag <|> pure (port envOpts)
  cliCodebaseUIPath <- codebaseUIPathFlag <|> pure (codebaseUIPath envOpts)
  pure
    CodebaseServerOpts
      { token = cliToken <|> token envOpts,
        host = cliHost <|> host envOpts,
        port = cliPort <|> port envOpts,
        codebaseUIPath = cliCodebaseUIPath <|> codebaseUIPath envOpts
      }
  where
    tokenFlag =
      optional . strOption $
        long "token"
          <> metavar "STRING"
          <> help "API auth token"
    hostFlag =
      optional . strOption $
        long "host"
          <> metavar "STRING"
          <> help "Codebase server host"
    portFlag =
      optional . option auto $
        long "port"
          <> metavar "NUMBER"
          <> help "Codebase server port"
    codebaseUIPathFlag =
      optional . strOption $
        long "ui"
          <> metavar "DIR"
          <> help "Path to codebase ui root"

launchParser :: CodebaseServerOpts -> IsHeadless -> Parser Command
launchParser envOpts isHeadless = do
  -- ApplicativeDo
  codebaseServerOpts <- codebaseServerOptsParser envOpts
  downloadBase <- downloadBaseFlag
  pure (Launch isHeadless codebaseServerOpts downloadBase)

initParser :: Parser Command
initParser = pure Init

versionParser :: Parser Command
versionParser = pure PrintVersion

runArgumentParser :: Parser [String]
runArgumentParser = many (strArgument (metavar "RUN-ARGS"))

runSymbolParser :: Parser Command
runSymbolParser =
  Run . RunFromSymbol <$> strArgument (metavar "SYMBOL") <*> runArgumentParser

runFileParser :: Parser Command
runFileParser =
  Run
    <$> ( RunFromFile <$> fileArgument "path/to/file"
            <*> strArgument (metavar "SYMBOL")
        )
    <*> runArgumentParser

runPipeParser :: Parser Command
runPipeParser =
  Run . RunFromPipe <$> strArgument (metavar "SYMBOL") <*> runArgumentParser

runCompiledParser :: Parser Command
runCompiledParser =
  Run . RunCompiled <$> fileArgument "path/to/file" <*> runArgumentParser

saveCodebaseFlag :: Parser ShouldSaveCodebase
saveCodebaseFlag = flag DontSaveCodebase SaveCodebase (long "save-codebase" <> help saveHelp)
  where
    saveHelp = "if set the resulting codebase will be saved to a new directory, otherwise it will be deleted"

downloadBaseFlag :: Parser ShouldDownloadBase
downloadBaseFlag = flag ShouldDownloadBase ShouldNotDownloadBase (long "no-base" <> help downloadBaseHelp)
  where
    downloadBaseHelp = "if set, a new codebase will be created without downloading the base library, otherwise the new codebase will download base"

fileArgument :: String -> Parser FilePath
fileArgument varName =
  strArgument
    ( metavar varName
        <> action "file" -- Autocomplete file names
    )

transcriptParser :: Parser Command
transcriptParser = do
  -- ApplicativeDo
  shouldSaveCodebase <- saveCodebaseFlag
  files <- liftA2 (NE.:|) (fileArgument "FILE") (many (fileArgument "FILES..."))
  pure (Transcript DontFork shouldSaveCodebase files)

transcriptForkParser :: Parser Command
transcriptForkParser = do
  -- ApplicativeDo
  shouldSaveCodebase <- saveCodebaseFlag
  files <- liftA2 (NE.:|) (fileArgument "FILE") (many (fileArgument "FILES..."))
  pure (Transcript UseFork shouldSaveCodebase files)

unisonHelp :: String -> String -> P.Doc
unisonHelp (P.text -> executable) (P.text -> version) =
  fold . List.intersperse P.line $
    [ P.empty,
      "🌻",
      P.empty,
      P.bold "Usage instructions for the Unison Codebase Manager",
      "You are running version:" <+> version,
      P.empty,
      "To get started just run" <+> P.bold executable,
      P.empty,
      "Use" <+> P.bold (executable <+> "[command] --help") <+> "to show help for a command."
    ]
