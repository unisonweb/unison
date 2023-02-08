-- | Projects.
module Unison.Project
  ( ProjectName,
    ProjectBranchName,
    ProjectAndBranch (..),
  )
where

import qualified Data.Char as Char
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
  userPrefix <- userPrefixParser <|> pure mempty
  projectSlug <- projectSlugParser
  pure (ProjectName (Text.Builder.run (userPrefix <> projectSlug)))
  where
    userPrefixParser :: Megaparsec.Parsec Void Text Text.Builder
    userPrefixParser = do
      userSlug <- userSlugParser
      slash <- Megaparsec.char '/'
      pure (userSlug <> Text.Builder.char slash)

    projectSlugParser :: Megaparsec.Parsec Void Text Text.Builder
    projectSlugParser = do
      c0 <- Megaparsec.satisfy isStartChar
      c1 <- Megaparsec.takeWhileP Nothing (\c -> isStartChar c || c == '-')
      pure (Text.Builder.char c0 <> Text.Builder.text c1)
      where
        isStartChar :: Char -> Bool
        isStartChar c =
          Char.isAlpha c || c == '_'

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
  userPrefix <- userPrefixParser <|> pure mempty
  branchSlug <- branchSlugParser
  pure (ProjectBranchName (Text.Builder.run (userPrefix <> branchSlug)))
  where
    userPrefixParser :: Megaparsec.Parsec Void Text Text.Builder
    userPrefixParser = do
      userSlug <- userSlugParser
      colon <- Megaparsec.char ':'
      pure (userSlug <> Text.Builder.char colon)

    branchSlugParser :: Megaparsec.Parsec Void Text Text.Builder
    branchSlugParser = do
      c0 <- Megaparsec.satisfy isStartChar
      c1 <- Megaparsec.takeWhileP Nothing (\c -> isStartChar c || c == '-')
      pure (Text.Builder.char c0 <> Text.Builder.text c1)
      where
        isStartChar :: Char -> Bool
        isStartChar c =
          Char.isAlpha c || c == '_'

-- | A generic data structure that contains information about a project and a branch in that project.
data ProjectAndBranch a b = ProjectAndBranch
  { project :: a,
    branch :: b
  }
  deriving stock (Eq, Generic, Show)

------------------------------------------------------------------------------------------------------------------------

-- Projects and branches may begin with a "user slug", which looks like "@arya".
--
-- slug       = @ start-char char*
-- start-char = alpha | _
-- char       = start-char | -
userSlugParser :: Megaparsec.Parsec Void Text Text.Builder.Builder
userSlugParser = do
  c0 <- Megaparsec.char '@'
  c1 <- Megaparsec.satisfy isStartChar
  c2 <- Megaparsec.takeWhileP Nothing (\c -> isStartChar c || c == '-')
  pure (Text.Builder.char c0 <> Text.Builder.char c1 <> Text.Builder.text c2)
  where
    isStartChar :: Char -> Bool
    isStartChar c =
      Char.isAlpha c || c == '_'
