{-# LANGUAGE OverloadedStrings   #-}

module Unison.Codebase.Editor.HelpTopics where

import           Data.ListLike                   (ListLike)
import           Data.String                     (IsString)
import qualified Unison.Codebase.Editor.SlurpResult as SR
import qualified Unison.Util.ColorText as CT
import qualified Unison.Util.Pretty as P

data HelpTopic = TestCache | FileStatus | NameSpaces | DisallowedAbsolute
  deriving (Eq)

instance Show HelpTopic where
  show = \case
    TestCache          -> "testcache"
    FileStatus         -> "filestatus"
    NameSpaces         -> "namespaces"
    DisallowedAbsolute -> "messages.disallowedAbsolute"

fromString :: String -> Either (P.Pretty CT.ColorText) HelpTopic
fromString = \case
    "testcache"                   -> Right TestCache
    "filestatus"                  -> Right FileStatus
    "namespaces"                  -> Right NameSpaces
    "messages.disallowedAbsolute" -> Right DisallowedAbsolute
    _ -> Left $ P.warnCallout "I don't know of that topic. Try `help-topics`."

helpTopics :: [String]
helpTopics = show <$> [TestCache, FileStatus, NameSpaces, DisallowedAbsolute]

-- hack: importing Codebase causes a cyclical import error
-- copy pasting for now to get things moving, will address
-- I suspect that this needs to be implemented in the Pretty module to avoid?
aside :: (ListLike s Char, IsString s) => P.Pretty s -> P.Pretty s -> P.Pretty s
aside a b = P.column2 [(a <> ":", b)]

knownTopics :: P.Pretty CT.ColorText
knownTopics = P.callout "🌻" $ P.lines [
  "Here's a list of topics I can tell you more about: ",
  "",
  P.indentN 2 $ P.sep "\n" (P.string <$> helpTopics),
  "",
  aside "Example" "use `help filestatus` to learn more about that topic."
  ]

toPretty :: HelpTopic -> P.Pretty CT.ColorText
toPretty = \case
  TestCache -> P.callout "🎈" . P.lines $ [
    P.wrap $ "Unison caches the results of " <> P.blue "test>"
          <> "watch expressions. Since these expressions are pure and"
          <> "always yield the same result when evaluated, there's no need"
          <> "to run them more than once!",
    "",
    P.wrap $ "A test is rerun only if it has changed, or if one"
          <> "of the definitions it depends on has changed."
    ]
  FileStatus -> P.callout "📓" . P.lines $ [
    P.wrap $ "Here's a list of possible status messages you might see"
          <> "for definitions in a .u file.", "",
    P.wrapColumn2 [
      (P.bold $ SR.prettyStatus SR.Collision,
       "A definition with the same name as an existing definition. Doing" <>
       "`update` instead of `add` will turn this failure into a successful" <>
       "update."),
      blankline,
      (P.bold $ SR.prettyStatus SR.Conflicted,
       "A definition with the same name as an existing definition." <>
       "Resolving the conflict and then trying an `update` again will" <>
       "turn this into a successful update."),
      blankline,
      (P.bold $ SR.prettyStatus SR.TermExistingConstructorCollision,
       "A definition with the same name as an existing constructor for " <>
       "some data type. Rename your definition or the data type before" <>
       "trying again to `add` or `update`."),
      blankline,
      (P.bold $ SR.prettyStatus SR.ConstructorExistingTermCollision,
       "A type defined in the file has a constructor that's named the" <>
       "same as an existing term. Rename that term or your constructor" <>
       "before trying again to `add` or `update`."),
      blankline,
      (P.bold $ SR.prettyStatus SR.BlockedDependency,
       "This definition was blocked because it dependended on " <>
       "a definition with a failed status."),
      blankline,
      (P.bold $ SR.prettyStatus SR.ExtraDefinition,
       "This definition was added because it was a dependency of" <>
       "a definition explicitly selected.")
      ]
   ]
   where blankline = ("","")
  NameSpaces -> P.callout "\129488" . P.lines $ [
    P.wrap $ "There are two kinds of namespaces," <> P.group (P.blue "absolute" <> ",")
          <> "such as" <> P.group ("(" <> P.blue ".foo.bar")
          <> "or" <> P.group (P.blue ".base.math.+" <> ")")
          <> "and" <> P.group (P.green "relative" <> ",")
          <> "such as" <> P.group ("(" <> P.green "math.sqrt")
          <> "or" <> P.group (P.green "util.List.++" <> ")."),
    "",
    P.wrap $ "Relative names are converted to absolute names by prepending the current namespace."
          <> "For example, if your Unison prompt reads:", "",
      P.indentN 2 $ P.blue ".foo.bar>", "",
    "and your .u file looks like:", "",
      P.indentN 2 $ P.green "x" <> " = 41", "",
    P.wrap $
      "then doing an" <> P.blue "add" <>
      "will create the definition with the absolute name" <>
      P.group (P.blue ".foo.bar.x" <> " = 41"),
    "",
    P.wrap $
      "and you can refer to" <> P.green "x" <> "by its absolute name " <>
      P.blue ".foo.bar.x" <> "elsewhere" <> "in your code. For instance:", "",
    P.indentN 2 $
      "answerToLifeTheUniverseAndEverything = " <> P.blue ".foo.bar.x" <> " + 1"
    ]
  DisallowedAbsolute -> P.callout "\129302" . P.lines $ [
    P.wrap $
      "Although I can understand absolute (ex: .foo.bar) or" <>
      "relative (ex: util.math.sqrt) references to existing definitions" <>
      P.group ("(" <> P.blue "help namespaces") <> "to learn more)," <>
      "I can't yet handle giving new definitions with absolute names in a .u file.",
    "",
    P.wrap $ "As a workaround, you can give definitions with a relative name"
          <> "temporarily (like `exports.blah.foo`) and then use `move.*` "
          <> "or `merge` commands to move stuff around afterwards."
    ]