-- | Projects.
--
-- The syntax-related parsing code (what makes a valid project name, etc) could conceivably be moved into a different
-- package, but for now we have just defined the one blessed project/branch name syntax that we allow.
module Unison.Project
  ( ProjectName,
    prependUserSlugToProjectName,
    ProjectBranchName,
    prependUserSlugToProjectBranchName,
    ProjectAndBranch (..),
  )
where

import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Text.Builder
import qualified Text.Builder as Text (Builder)
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec
import Unison.Prelude
import Witch

-- | The name of a project.
--
-- Convert to and from text with the 'From' and 'TryFrom' instances.
newtype ProjectName
  = ProjectName Text
  deriving stock (Eq, Ord, Show)

instance From ProjectName Text

instance TryFrom Text ProjectName where
  tryFrom =
    maybeTryFrom (Megaparsec.parseMaybe projectNameParser)

projectNameParser :: Megaparsec.Parsec Void Text ProjectName
projectNameParser = do
  userSlug <- userSlugParser <|> pure mempty
  projectSlug <- projectSlugParser
  pure (ProjectName (Text.Builder.run (userSlug <> projectSlug)))
  where
    projectSlugParser :: Megaparsec.Parsec Void Text Text.Builder
    projectSlugParser = do
      c0 <- Megaparsec.satisfy isStartChar
      c1 <- Megaparsec.takeWhileP Nothing (\c -> isStartChar c || c == '-')
      pure (Text.Builder.char c0 <> Text.Builder.text c1)
      where
        isStartChar :: Char -> Bool
        isStartChar c =
          Char.isAlpha c || c == '_'

-- | Prepend a user slug to a project name, if it doesn't already have one.
--
-- >>> prependUserSlugToProjectName "arya" "lens"
-- "@arya/lens"
--
-- >>> prependUserSlugToProjectName "runar" "@unison/base"
-- "@unison/base"
--
-- >>> prependUserSlugToProjectName "???invalid???" "@unison/base"
-- "@unison/base"
prependUserSlugToProjectName :: Text -> ProjectName -> ProjectName
prependUserSlugToProjectName userSlug (ProjectName projectName) =
  if Text.head projectName == '@'
    then ProjectName projectName
    else fromMaybe (ProjectName projectName) (Megaparsec.parseMaybe projectNameParser newProjectName)
  where
    newProjectName =
      Text.Builder.run $
        Text.Builder.char '@'
          <> Text.Builder.text userSlug
          <> Text.Builder.char '/'
          <> Text.Builder.text projectName

-- | The name of a branch of a project.
--
-- Convert to and from text with the 'From' and 'TryFrom' instances.
newtype ProjectBranchName
  = ProjectBranchName Text
  deriving stock (Eq, Ord, Show)

instance From ProjectBranchName Text

instance TryFrom Text ProjectBranchName where
  tryFrom =
    maybeTryFrom (Megaparsec.parseMaybe projectBranchNameParser)

projectBranchNameParser :: Megaparsec.Parsec Void Text ProjectBranchName
projectBranchNameParser = do
  userSlug <- userSlugParser <|> pure mempty
  branchSlug <- branchSlugParser
  pure (ProjectBranchName (Text.Builder.run (userSlug <> branchSlug)))
  where
    branchSlugParser :: Megaparsec.Parsec Void Text Text.Builder
    branchSlugParser = do
      c0 <- Megaparsec.satisfy isStartChar
      c1 <- Megaparsec.takeWhileP Nothing (\c -> isStartChar c || c == '-')
      pure (Text.Builder.char c0 <> Text.Builder.text c1)
      where
        isStartChar :: Char -> Bool
        isStartChar c =
          Char.isAlpha c || c == '_'

-- | Prepend a user slug to a project branch name, if it doesn't already have one.
--
-- >>> prependUserSlugToProjectBranchName "arya" "topic"
-- "@arya/topic"
--
-- >>> prependUserSlugToProjectBranchName "runar" "@unison/main"
-- "@unison/main"
--
-- >>> prependUserSlugToProjectBranchName "???invalid???" "@unison/main"
-- "@unison/main"
prependUserSlugToProjectBranchName :: Text -> ProjectBranchName -> ProjectBranchName
prependUserSlugToProjectBranchName userSlug (ProjectBranchName branchName) =
  if Text.head branchName == '@'
    then ProjectBranchName branchName
    else fromMaybe (ProjectBranchName branchName) (Megaparsec.parseMaybe projectBranchNameParser newBranchName)
  where
    newBranchName =
      Text.Builder.run $
        Text.Builder.char '@'
          <> Text.Builder.text userSlug
          <> Text.Builder.char '/'
          <> Text.Builder.text branchName

-- | A generic data structure that contains information about a project and a branch in that project.
data ProjectAndBranch a b = ProjectAndBranch
  { project :: a,
    branch :: b
  }
  deriving stock (Eq, Generic, Show)

-- | @project/branch@ syntax for project+branch pair, with both sides optional. Missing value means "the current one".
instance From (ProjectAndBranch (Maybe ProjectName) (Maybe ProjectBranchName)) Text where
  from ProjectAndBranch {project, branch} =
    Text.Builder.run (textify project <> Text.Builder.char '/' <> textify branch)
    where
      textify :: From thing Text => Maybe thing -> Text.Builder
      textify =
        maybe mempty (Text.Builder.text . into)

instance TryFrom Text (ProjectAndBranch (Maybe ProjectName) (Maybe ProjectBranchName)) where
  tryFrom =
    maybeTryFrom (Megaparsec.parseMaybe projectAndBranchNamesParser)

projectAndBranchNamesParser ::
  Megaparsec.Parsec
    Void
    Text
    (ProjectAndBranch (Maybe ProjectName) (Maybe ProjectBranchName))
projectAndBranchNamesParser = do
  project <- optional projectNameParser
  branch <- optional do
    _ <- Megaparsec.char '/'
    projectBranchNameParser
  pure ProjectAndBranch {project, branch}

------------------------------------------------------------------------------------------------------------------------

-- Projects and branches may begin with a "user slug", which looks like "@arya/".
--
-- slug       = @ start-char char* /
-- start-char = alpha | _
-- char       = start-char | -
userSlugParser :: Megaparsec.Parsec Void Text Text.Builder.Builder
userSlugParser = do
  c0 <- Megaparsec.char '@'
  c1 <- Megaparsec.satisfy isStartChar
  c2 <- Megaparsec.takeWhileP Nothing (\c -> isStartChar c || c == '-')
  c3 <- Megaparsec.char '/'
  pure (Text.Builder.char c0 <> Text.Builder.char c1 <> Text.Builder.text c2 <> Text.Builder.char c3)
  where
    isStartChar :: Char -> Bool
    isStartChar c =
      Char.isAlpha c || c == '_'
