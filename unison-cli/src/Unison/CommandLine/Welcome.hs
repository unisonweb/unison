module Unison.CommandLine.Welcome
  ( CodebaseInitStatus (..),
    Welcome (..),
    asciiartUnison,
    run,
    welcome,
  )
where

import System.Random (randomRIO)
import Unison.Codebase.Editor.Input
import Unison.CommandLine.Types (ShouldWatchFiles (..))
import Unison.Prelude
import Unison.Util.Pretty qualified as P
import Prelude hiding (readFile, writeFile)

data Welcome = Welcome
  { onboarding :: Onboarding, -- Onboarding States
    watchDir :: FilePath,
    unisonVersion :: Text,
    shouldWatchFiles :: ShouldWatchFiles
  }

-- Previously Created is different from Previously Onboarded because a user can
-- 1.) create a new codebase
-- 2.) decide not to go through the onboarding flow until later and exit
-- 3.) then reopen their blank codebase
data CodebaseInitStatus
  = NewlyCreatedCodebase -- Can transition to [Base, Author, Finished]
  | PreviouslyCreatedCodebase -- Can transition to [Base, Author, Finished, PreviouslyOnboarded].
  deriving (Show, Eq)

data Onboarding
  = Init CodebaseInitStatus -- Can transition to [Author, Finished, PreviouslyOnboarded]
  | Author -- Can transition to [Finished]
  -- End States
  | Finished
  | PreviouslyOnboarded
  deriving (Show, Eq)

welcome :: CodebaseInitStatus -> FilePath -> Text -> ShouldWatchFiles -> Welcome
welcome initStatus filePath unisonVersion shouldWatchFiles =
  Welcome (Init initStatus) filePath unisonVersion shouldWatchFiles

run :: Welcome -> IO [Either Event Input]
run Welcome {onboarding = onboarding, watchDir = dir, unisonVersion = version, shouldWatchFiles} = do
  go onboarding []
  where
    go :: Onboarding -> [Either Event Input] -> IO [Either Event Input]
    go onboarding acc =
      case onboarding of
        Init NewlyCreatedCodebase -> do
          go PreviouslyOnboarded (headerMsg : acc)
          where
            headerMsg = toInput (header version)
        Init PreviouslyCreatedCodebase -> do
          go PreviouslyOnboarded (headerMsg : acc)
          where
            headerMsg = toInput (header version)
        Author ->
          go Finished (authorMsg : acc)
          where
            authorMsg = toInput authorSuggestion
        -- These are our two terminal Welcome conditions, at the end we reverse the order of the desired input commands otherwise they come out backwards
        Finished -> do
          startMsg <- getStarted shouldWatchFiles dir
          pure $ reverse (toInput startMsg : acc)
        PreviouslyOnboarded -> do
          startMsg <- getStarted shouldWatchFiles dir
          pure $ reverse (toInput startMsg : acc)

toInput :: P.Pretty P.ColorText -> Either Event Input
toInput pretty =
  Right $ CreateMessage pretty

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

header :: Text -> P.Pretty P.ColorText
header version =
  asciiartUnison
    <> P.newline
    <> P.newline
    <> P.linesSpaced
      [ P.wrap "👋 Welcome to Unison!",
        P.wrap ("You are running version: " <> P.bold (P.text version))
      ]

authorSuggestion :: P.Pretty P.ColorText
authorSuggestion =
  P.newline
    <> P.lines
      [ P.wrap "📜🪶 You might want to set up your author information next.",
        P.wrap "Type" <> P.hiBlue " create.author" <> " to create an author for this codebase",
        P.group (P.newline <> P.wrap "Read about how to link your author to your code at"),
        P.wrap $ P.blue "https://www.unison-lang.org/learn/tooling/configuration/"
      ]

getStarted :: ShouldWatchFiles -> FilePath -> IO (P.Pretty P.ColorText)
getStarted shouldWatchFiles dir = do
  earth <- (["🌎", "🌍", "🌏"] !!) <$> randomRIO (0, 2)

  pure $
    P.linesSpaced
      [ P.wrap "Get started:",
        P.indentN 2 $
          P.column2
            ( [ ("📖", "Type " <> P.hiBlue "help" <> " to list all commands, or " <> P.hiBlue "help <cmd>" <> " to view help for one command"),
                ("🎨", "Type " <> P.hiBlue "ui" <> " to open the Codebase UI in your default browser"),
                ("📚", "Read the official docs at " <> P.blue "https://www.unison-lang.org/learn/"),
                (earth, "Visit Unison Share at " <> P.blue "https://share.unison-lang.org" <> " to discover libraries")
              ]
                <> case shouldWatchFiles of
                  ShouldWatchFiles -> [("👀", "I'm watching for changes to " <> P.bold ".u" <> " files under " <> (P.group . P.blue $ P.string dir))]
                  ShouldNotWatchFiles -> [("📝", "File watching is disabled, use the 'load' command to parse and typecheck unison files.")]
            )
      ]
