module Unison.CommandLine.Welcome
  ( CodebaseInitStatus (..),
    Welcome (..),
    asciiartUnison,
    run,
    welcome,
  )
where

import Unison.Codebase.Editor.Input
import Unison.Prelude
import Unison.Util.Pretty qualified as P
import Prelude hiding (readFile, writeFile)

data Welcome = Welcome
  { onboarding :: Onboarding, -- Onboarding States
    unisonVersion :: Text,
    showWelcomeHint :: Bool
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

welcome :: CodebaseInitStatus -> Text -> Bool -> Welcome
welcome initStatus unisonVersion showWelcomeHint =
  Welcome (Init initStatus) unisonVersion showWelcomeHint

run :: Welcome -> [Either Event Input]
run Welcome {onboarding = onboarding, unisonVersion = version, showWelcomeHint = showWelcomeHint} = do
  go onboarding []
  where
    go :: Onboarding -> [Either Event Input] -> [Either Event Input]
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
        Finished -> reverse (toInput (getStarted showWelcomeHint) : acc)
        PreviouslyOnboarded -> reverse (toInput (getStarted showWelcomeHint) : acc)

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
        P.wrap $ P.blue "https://www.unison-lang.org/docs/tooling/configuration/"
      ]

getStarted :: Bool -> P.Pretty P.ColorText
getStarted showWelcomeHint =
  P.wrap "📚 Read the official docs at https://www.unison-lang.org/docs/"
    <> P.newline
    <> P.newline
    <> P.wrap (if showWelcomeHint
              then "Hint: Type 'projects' to list all your projects, or 'project.create' to start something new."
              else "Type 'project.create' to get started.")

