{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Projects.
--
-- The syntax-related parsing code (what makes a valid project name, etc) could conceivably be moved into a different
-- package, but for now we have just defined the one blessed project/branch name syntax that we allow.
module Unison.Project
  ( ProjectName,
    projectNameUserSlug,
    prependUserSlugToProjectName,
    ProjectBranchName,
    projectBranchNameUserSlug,
    prependUserSlugToProjectBranchName,
    ProjectAndBranch (..),
    projectAndBranchNamesParser,
  )
where

import qualified Data.Char as Char
import qualified Data.Text as Text
import Data.These (These (..))
import qualified Text.Builder
import qualified Text.Builder as Text (Builder)
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec
import Unison.Core.Project (ProjectAndBranch (..), ProjectBranchName (..), ProjectName (..))
import Unison.Prelude
import Witch

instance From ProjectName Text

instance TryFrom Text ProjectName where
  tryFrom =
    maybeTryFrom (Megaparsec.parseMaybe projectNameParser)

projectNameParser :: Megaparsec.Parsec Void Text ProjectName
projectNameParser = do
  userSlug <- userSlugParser <|> pure mempty
  projectSlug <- projectSlugParser
  pure (UnsafeProjectName (Text.Builder.run (userSlug <> projectSlug)))
  where
    projectSlugParser :: Megaparsec.Parsec Void Text Text.Builder
    projectSlugParser = do
      c0 <- Megaparsec.satisfy isStartChar
      c1 <- Megaparsec.takeWhileP Nothing (\c -> isStartChar c || Char.isDigit c || c == '-')
      pure (Text.Builder.char c0 <> Text.Builder.text c1)
      where
        isStartChar :: Char -> Bool
        isStartChar c =
          Char.isAlpha c || c == '_'

-- | Get the user slug at the beginning of a project name, if there is one.
--
-- >>> projectNameUserSlug "@arya/lens"
-- Just "arya"
--
-- >>> projectNameUserSlug "lens"
-- Nothing
projectNameUserSlug :: ProjectName -> Maybe Text
projectNameUserSlug (UnsafeProjectName projectName) =
  if Text.head projectName == '@'
    then Just (Text.takeWhile (/= '/') (Text.drop 1 projectName))
    else Nothing

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
prependUserSlugToProjectName userSlug (UnsafeProjectName projectName) =
  if Text.head projectName == '@'
    then UnsafeProjectName projectName
    else fromMaybe (UnsafeProjectName projectName) (Megaparsec.parseMaybe projectNameParser newProjectName)
  where
    newProjectName =
      Text.Builder.run $
        Text.Builder.char '@'
          <> Text.Builder.text userSlug
          <> Text.Builder.char '/'
          <> Text.Builder.text projectName

instance From ProjectBranchName Text

instance TryFrom Text ProjectBranchName where
  tryFrom =
    maybeTryFrom (Megaparsec.parseMaybe projectBranchNameParser)

projectBranchNameParser :: Megaparsec.Parsec Void Text ProjectBranchName
projectBranchNameParser = do
  userSlug <- userSlugParser <|> pure mempty
  branchSlug <- branchSlugParser
  pure (UnsafeProjectBranchName (Text.Builder.run (userSlug <> branchSlug)))
  where
    branchSlugParser :: Megaparsec.Parsec Void Text Text.Builder
    branchSlugParser = do
      c0 <- Megaparsec.satisfy isStartChar
      c1 <- Megaparsec.takeWhileP Nothing (\c -> isStartChar c || Char.isDigit c || c == '-')
      pure (Text.Builder.char c0 <> Text.Builder.text c1)
      where
        isStartChar :: Char -> Bool
        isStartChar c =
          Char.isAlpha c || c == '_'

-- | Get the user slug at the beginning of a project branch name, if there is one.
--
-- >>> projectBranchNameUserSlug "@arya/topic"
-- Just "arya"
--
-- >>> projectBranchNameUserSlug "topic"
-- Nothing
projectBranchNameUserSlug :: ProjectBranchName -> Maybe Text
projectBranchNameUserSlug (UnsafeProjectBranchName branchName) =
  if Text.head branchName == '@'
    then Just (Text.takeWhile (/= '/') (Text.drop 1 branchName))
    else Nothing

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
prependUserSlugToProjectBranchName userSlug (UnsafeProjectBranchName branchName) =
  if Text.head branchName == '@'
    then UnsafeProjectBranchName branchName
    else fromMaybe (UnsafeProjectBranchName branchName) (Megaparsec.parseMaybe projectBranchNameParser newBranchName)
  where
    newBranchName =
      Text.Builder.run $
        Text.Builder.char '@'
          <> Text.Builder.text userSlug
          <> Text.Builder.char '/'
          <> Text.Builder.text branchName

-- | @project/branch@ syntax for project+branch pair, with up to one
-- side optional. Missing value means "the current one".
instance From (These ProjectName ProjectBranchName) Text where
  from = \case
    This project1 -> into @Text project1
    That branch1 -> Text.Builder.run (Text.Builder.char '/' <> Text.Builder.text (into @Text branch1))
    These project1 branch1 ->
      Text.Builder.run $
        Text.Builder.text (into @Text project1)
          <> Text.Builder.char '/'
          <> Text.Builder.text (into @Text branch1)

instance TryFrom Text (These ProjectName ProjectBranchName) where
  tryFrom =
    maybeTryFrom (Megaparsec.parseMaybe projectAndBranchNamesParser)

-- Valid things:
--
--   1. project
--   2. project/branch
--   3. /branch
projectAndBranchNamesParser :: Megaparsec.Parsec Void Text (These ProjectName ProjectBranchName)
projectAndBranchNamesParser = do
  optional projectNameParser >>= \case
    Nothing -> That <$> branchParser
    Just prj ->
      optional branchParser <&> \case
        Nothing -> This prj
        Just br -> These prj br
  where
    branchParser = Megaparsec.char '/' >> projectBranchNameParser

-- | @project/branch@ syntax, where the project is optional. The branch can optionally be preceded by a forward slash.
instance From (ProjectAndBranch (Maybe ProjectName) ProjectBranchName) Text where
  from = \case
    ProjectAndBranch Nothing branch -> into @Text branch
    ProjectAndBranch (Just project) branch ->
      Text.Builder.run $
        Text.Builder.text (into @Text project)
          <> Text.Builder.char '/'
          <> Text.Builder.text (into @Text branch)

instance TryFrom Text (ProjectAndBranch (Maybe ProjectName) ProjectBranchName) where
  tryFrom =
    maybeTryFrom (Megaparsec.parseMaybe branchWithOptionalProjectParser)

-- Valid things:
--
--   1. branch
--   2. /branch
--   3. project/branch
branchWithOptionalProjectParser :: Megaparsec.Parsec Void Text (ProjectAndBranch (Maybe ProjectName) ProjectBranchName)
branchWithOptionalProjectParser =
  asum
    [ Megaparsec.try do
        project <- projectNameParser
        _ <- Megaparsec.char '/'
        branch <- projectBranchNameParser
        pure (ProjectAndBranch (Just project) branch),
      do
        _ <- Megaparsec.optional (Megaparsec.char '/')
        branch <- projectBranchNameParser
        pure (ProjectAndBranch Nothing branch)
    ]

------------------------------------------------------------------------------------------------------------------------

-- Projects and branches may begin with a "user slug", which looks like "@arya/".
--
-- slug       = @ start-char char* /
-- start-char = alpha
-- char       = alpha | digit | -
userSlugParser :: Megaparsec.Parsec Void Text Text.Builder.Builder
userSlugParser = do
  c0 <- Megaparsec.char '@'
  c1 <- Megaparsec.satisfy Char.isAlpha
  c2 <- Megaparsec.takeWhileP Nothing (\c -> Char.isAlpha c || Char.isDigit c || c == '-')
  c3 <- Megaparsec.char '/'
  pure (Text.Builder.char c0 <> Text.Builder.char c1 <> Text.Builder.text c2 <> Text.Builder.char c3)
