{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Projects.
--
-- The syntax-related parsing code (what makes a valid project name, etc) could conceivably be moved into a different
-- package, but for now we have just defined the one blessed project/branch name syntax that we allow.
module Unison.Project
  ( ProjectName,
    projectNameUserSlug,
    projectNameToUserProjectSlugs,
    prependUserSlugToProjectName,
    ProjectBranchName,
    projectBranchNameUserSlug,
    ProjectBranchNameKind (..),
    classifyProjectBranchName,
    ProjectBranchNameOrLatestRelease (..),
    ProjectBranchSpecifier (..),
    ProjectAndBranch (..),
    projectAndBranchNamesParser,
    fullyQualifiedProjectAndBranchNamesParser,
    projectAndOptionalBranchParser,
    branchWithOptionalProjectParser,
    ProjectAndBranchNames (..),
    projectAndBranchNamesParser2,
    projectNameParser,
    projectBranchNameParser,

    -- ** Semver
    Semver (..),
  )
where

import Data.Char qualified as Char
import Data.Kind (Type)
import Data.Text qualified as Text
import Data.Text.Read qualified as Text (decimal)
import Data.These (These (..))
import Text.Builder qualified
import Text.Builder qualified as Text (Builder)
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char qualified as Megaparsec
import Unison.Core.Project (ProjectAndBranch (..), ProjectBranchName (..), ProjectName (..))
import Unison.Prelude
import Witch

instance From ProjectName Text

instance TryFrom Text ProjectName where
  tryFrom =
    maybeTryFrom (fmap fst . Megaparsec.parseMaybe projectNameParser)

-- Parse a project name, and whether it ended in a forward slash (which is, of course, not part of the name)
projectNameParser :: Megaparsec.Parsec Void Text (ProjectName, Bool)
projectNameParser = do
  userSlug <-
    asum
      [ do
          user <- userSlugParser
          pure (Text.Builder.char '@' <> user <> Text.Builder.char '/'),
        pure mempty
      ]
  projectSlug <- projectSlugParser
  hasTrailingSlash <- isJust <$> optional (Megaparsec.char '/')
  pure (UnsafeProjectName (Text.Builder.run (userSlug <> projectSlug)), hasTrailingSlash)
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

-- | Parse a "@arya/lens" into the "arya" and "lens" parts.
--
-- If there's no "arya" part, returns the empty string there.
--
-- >>> projectNameToUserProjectSlugs (UnsafeProjectName "@arya/lens")
-- ("arya","lens")
--
-- >>> projectNameToUserProjectSlugs (UnsafeProjectName "lens")
-- ("","lens")
projectNameToUserProjectSlugs :: ProjectName -> (Text, Text)
projectNameToUserProjectSlugs (UnsafeProjectName name) =
  case Text.span (/= '/') name of
    (project, "") -> ("", project)
    (atUser, slashProject) -> (Text.drop 1 atUser, Text.drop 1 slashProject)

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
    else fromMaybe (UnsafeProjectName projectName) (fst <$> Megaparsec.parseMaybe projectNameParser newProjectName)
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
    maybeTryFrom (Megaparsec.parseMaybe (projectBranchNameParser True))

projectBranchNameParser :: Bool -> Megaparsec.Parsec Void Text ProjectBranchName
projectBranchNameParser allowLeadingSlash =
  unstructureStructuredProjectName <$> structuredProjectBranchNameParser allowLeadingSlash

-- An internal type that captures the structure of a project branch name after parsing. 'classifyProjectBranchName' is
-- how a user can recover this structure for the few cases it's relevant (e.g. during push)
data StructuredProjectBranchName
  = StructuredProjectBranchName'Contributor !Text.Builder !Text.Builder
  | StructuredProjectBranchName'DraftRelease !Semver
  | StructuredProjectBranchName'Release !Semver
  | StructuredProjectBranchName'NothingSpecial !Text.Builder

unstructureStructuredProjectName :: StructuredProjectBranchName -> ProjectBranchName
unstructureStructuredProjectName =
  UnsafeProjectBranchName . Text.Builder.run . \case
    StructuredProjectBranchName'Contributor user name ->
      Text.Builder.char '@' <> user <> Text.Builder.char '/' <> name
    StructuredProjectBranchName'DraftRelease ver -> "releases/drafts/" <> unstructureSemver ver
    StructuredProjectBranchName'Release ver -> "releases/" <> unstructureSemver ver
    StructuredProjectBranchName'NothingSpecial name -> name
  where
    unstructureSemver :: Semver -> Text.Builder
    unstructureSemver (Semver x y z) =
      Text.Builder.decimal x
        <> Text.Builder.char '.'
        <> Text.Builder.decimal y
        <> Text.Builder.char '.'
        <> Text.Builder.decimal z

structuredProjectBranchNameParser :: Bool -> Megaparsec.Parsec Void Text StructuredProjectBranchName
structuredProjectBranchNameParser allowLeadingSlash = do
  _ <- when allowLeadingSlash (void (optional (Megaparsec.char '/')))
  branch <-
    asum
      [ do
          _ <- Megaparsec.string "releases/drafts/"
          ver <- semverParser
          pure (StructuredProjectBranchName'DraftRelease ver),
        do
          _ <- Megaparsec.string "releases/"
          ver <- semverParser
          pure (StructuredProjectBranchName'Release ver),
        do
          user <- userSlugParser
          branch <- branchSlugParser
          pure (StructuredProjectBranchName'Contributor user branch),
        do
          branch <- branchSlugParser
          pure (StructuredProjectBranchName'NothingSpecial branch)
      ]
  -- Because our branch has a sort of /-delimited pseudo-structure, we fail on trailing forward slashes.
  --
  -- This (perhaps among other things) lets us successfully parse something like "releases/drafts/1.2.3" as a branch
  -- with an optional project component in a straightforward way, which might otherwise succeed with
  -- project="releases", branch="drafts", leftovers="/1.2.3" (as it did before this line was added).
  Megaparsec.notFollowedBy (Megaparsec.char '/')
  pure branch
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

data Semver
  = Semver !Int !Int !Int
  deriving stock (Eq, Show)

instance From Semver Text where
  from (Semver x y z) =
    (Text.Builder.run . fold)
      [ Text.Builder.decimal x,
        Text.Builder.char '.',
        Text.Builder.decimal y,
        Text.Builder.char '.',
        Text.Builder.decimal z
      ]

instance TryFrom Text Semver where
  tryFrom =
    maybeTryFrom (Megaparsec.parseMaybe semverParser)

semverParser :: Megaparsec.Parsec Void Text Semver
semverParser = do
  x <- decimalParser
  _ <- Megaparsec.char '.'
  y <- decimalParser
  _ <- Megaparsec.char '.'
  z <- decimalParser
  pure (Semver x y z)
  where
    decimalParser = do
      digits <- Megaparsec.takeWhile1P (Just "decimal") Char.isDigit
      pure case Text.decimal digits of
        Right (n, _) -> n
        Left _ -> 0 -- impossible

-- | Though a branch name is just a flat string, we have logic that handles certain strings specially.
--
-- A branch's name indicates it is exactly one of the following:
--
--   * A contributor branch like "@arya/topic"
--   * A draft release branch like "releases/drafts/1.2.3"
--   * A release branch like "releases/1.2.3"
--   * None of the above, like "topic"
--
-- Note these classifications are only tied to the branch's (mutable) name, and are not really otherwise indicative of
-- much.
--
-- For instance,
--
--   - The existence of a local "releases/1.2.3" branch does not necessarily imply the existence of some remote release
--     version "1.2.3".
--   - The existence of a local "@arya/topic@ branch does not necessarily imply the existence of some remote "arya"
--     user made some "topic" branch at some point.
--
-- That said, we do try to make the system mostly make sense by rejecting certain inputs (e.g. you should not be able
-- to easily create a local branch called "releases/1.2.3" out of thin air; you should have to clone it from
-- somewhere). But ultimately, again, branch names are best thought of as opaque, flat strings.
data ProjectBranchNameKind
  = ProjectBranchNameKind'Contributor !Text !ProjectBranchName
  | ProjectBranchNameKind'DraftRelease !Semver
  | ProjectBranchNameKind'Release !Semver
  | ProjectBranchNameKind'NothingSpecial

-- | Classify a project branch name.
--
-- >>> classifyProjectBranchName "@arya/topic"
-- Contributor "arya" "topic"
--
-- >>> classifyProjectBranchName "releases/drafts/1.2.3"
-- DraftRelease (Semver 1 2 3)
--
-- >>> classifyProjectBranchName "releases/1.2.3"
-- Release (Semver 1 2 3)
--
-- >>> classifyProjectBranchName "topic"
-- NothingSpecial
classifyProjectBranchName :: ProjectBranchName -> ProjectBranchNameKind
classifyProjectBranchName (UnsafeProjectBranchName branchName) =
  case Megaparsec.parseMaybe (structuredProjectBranchNameParser False) branchName of
    Just (StructuredProjectBranchName'Contributor user name) ->
      ProjectBranchNameKind'Contributor (Text.Builder.run user) (UnsafeProjectBranchName (Text.Builder.run name))
    Just (StructuredProjectBranchName'DraftRelease ver) -> ProjectBranchNameKind'DraftRelease ver
    Just (StructuredProjectBranchName'Release ver) -> ProjectBranchNameKind'Release ver
    Just (StructuredProjectBranchName'NothingSpecial _name) -> ProjectBranchNameKind'NothingSpecial
    Nothing -> error (reportBug "E800424" ("Invalid project branch name: " ++ Text.unpack branchName))

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

-- | A project branch name, or the latest release of its project.
data ProjectBranchNameOrLatestRelease
  = ProjectBranchNameOrLatestRelease'LatestRelease
  | ProjectBranchNameOrLatestRelease'Name !ProjectBranchName
  deriving stock (Eq, Show)

-- | How a project branch can be specified.
data ProjectBranchSpecifier :: Type -> Type where
  -- | By name.
  ProjectBranchSpecifier'Name :: ProjectBranchSpecifier ProjectBranchName
  -- | By name, or "the latest release"
  ProjectBranchSpecifier'NameOrLatestRelease :: ProjectBranchSpecifier ProjectBranchNameOrLatestRelease

projectBranchSpecifierParser :: ProjectBranchSpecifier branch -> Megaparsec.Parsec Void Text branch
projectBranchSpecifierParser = \case
  ProjectBranchSpecifier'Name -> projectBranchNameParser False
  ProjectBranchSpecifier'NameOrLatestRelease ->
    asum
      [ ProjectBranchNameOrLatestRelease'LatestRelease <$ "releases/latest",
        ProjectBranchNameOrLatestRelease'Name <$> projectBranchNameParser False
      ]

instance From (ProjectAndBranch ProjectName ProjectBranchName) Text where
  from (ProjectAndBranch project branch) =
    Text.Builder.run $
      Text.Builder.text (into @Text project)
        <> Text.Builder.char '/'
        <> Text.Builder.text (into @Text branch)

-- | Sometimes, it's convenient (to users) if we defer interpreting certain names (like "foo") as a project name or
-- branch name, instead leaving it up to a command handler to handle the ambiguity.
--
-- For example, we might want "switch foo" to switch to either the project "foo", or the branch "foo", or complain if
-- both exist.
--
-- This type is useful for those situtations.
data ProjectAndBranchNames
  = ProjectAndBranchNames'Ambiguous ProjectName ProjectBranchName
  | ProjectAndBranchNames'Unambiguous (These ProjectName ProjectBranchName)
  deriving stock (Eq, Show)

instance TryFrom Text ProjectAndBranchNames where
  tryFrom =
    maybeTryFrom (Megaparsec.parseMaybe projectAndBranchNamesParser2)

projectAndBranchNamesParser2 :: Megaparsec.Parsec Void Text ProjectAndBranchNames
projectAndBranchNamesParser2 = do
  optional (Megaparsec.char '/') >>= \case
    Nothing ->
      asum
        [ Megaparsec.try do
            (project, hasTrailingSlash) <- projectNameParser
            if hasTrailingSlash
              then do
                optional (Megaparsec.lookAhead Megaparsec.anySingle) >>= \case
                  Nothing -> pure (ProjectAndBranchNames'Unambiguous (This project))
                  Just nextChar
                    -- This project looks like "<name>/<digit>" so far... we want to fail here, and pick back up at
                    -- `unambiguousBranchParser` below, because a string like "releases/1.2.3" is a valid branch, and
                    -- we don't want to succeed with project name "releases" and leftovers "1.2.3"
                    --
                    -- Technically it's pointless to fall back on `unambiguousBranchParser` if the project name is not
                    -- exactly "releases", but oh well.
                    | Char.isDigit nextChar -> empty
                    -- If the character after "<name>/" is the valid start of a branch, then parse a branch.
                    | Char.isAlpha nextChar || nextChar == '@' || nextChar == '_' -> do
                        branch <- projectBranchNameParser False
                        pure (ProjectAndBranchNames'Unambiguous (These project branch))
                    -- Otherwise, some invalid start-of-branch character follows, like a close paren or something.
                    | otherwise -> pure (ProjectAndBranchNames'Unambiguous (This project))
              else pure case Megaparsec.parseMaybe (projectBranchNameParser False) (into @Text project) of
                Nothing -> ProjectAndBranchNames'Unambiguous (This project)
                Just branch -> ProjectAndBranchNames'Ambiguous project branch,
          unambiguousBranchParser
        ]
    Just _ -> unambiguousBranchParser
  where
    unambiguousBranchParser = do
      branch <- projectBranchNameParser False
      pure (ProjectAndBranchNames'Unambiguous (That branch))

-- TODO this should go away in favor of ProjectAndBranchNames
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
    maybeTryFrom (Megaparsec.parseMaybe (projectAndBranchNamesParser ProjectBranchSpecifier'Name))

-- Valid things:
--
--   1. project
--   2. project/
--   3. project/branch
--   4. /branch
projectAndBranchNamesParser ::
  forall branch.
  ProjectBranchSpecifier branch ->
  Megaparsec.Parsec Void Text (These ProjectName branch)
projectAndBranchNamesParser specifier = do
  optional projectNameParser >>= \case
    Nothing -> do
      _ <- Megaparsec.char '/'
      branch <- projectBranchSpecifierParser specifier
      pure (That branch)
    Just (project, hasTrailingSlash) ->
      if hasTrailingSlash
        then do
          optional (projectBranchSpecifierParser specifier) <&> \case
            Nothing -> This project
            Just branch -> These project branch
        else pure (This project)

-- | Parse a fully specified myproject/mybranch name.
--
-- >>> import Text.Megaparsec (parseMaybe)
-- >>> parseMaybe fullyQualifiedProjectAndBranchNamesParser ("myproject/mybranch" :: Text)
-- Just (ProjectAndBranch {project = UnsafeProjectName "myproject", branch = UnsafeProjectBranchName "mybranch"})
fullyQualifiedProjectAndBranchNamesParser :: Megaparsec.Parsec Void Text (ProjectAndBranch ProjectName ProjectBranchName)
fullyQualifiedProjectAndBranchNamesParser = do
  (project, hadSlash) <- projectNameParser
  if hadSlash
    then pure ()
    else void $ Megaparsec.char '/'
  branch <- projectBranchNameParser False
  pure (ProjectAndBranch project branch)

-- | @project/branch@ syntax, where the branch is optional.
instance From (ProjectAndBranch ProjectName (Maybe ProjectBranchName)) Text where
  from = \case
    ProjectAndBranch project Nothing -> into @Text project
    ProjectAndBranch project (Just branch) ->
      Text.Builder.run $
        Text.Builder.text (into @Text project)
          <> Text.Builder.char '/'
          <> Text.Builder.text (into @Text branch)

instance TryFrom Text (ProjectAndBranch ProjectName (Maybe ProjectBranchName)) where
  tryFrom =
    maybeTryFrom (Megaparsec.parseMaybe (projectAndOptionalBranchParser ProjectBranchSpecifier'Name))

-- | Attempt to parse a project and branch name from a string where both are required.
instance TryFrom Text (ProjectAndBranch ProjectName ProjectBranchName) where
  tryFrom =
    maybeTryFrom $ \txt -> do
      ProjectAndBranch projectName mayBranchName <- Megaparsec.parseMaybe (projectAndOptionalBranchParser ProjectBranchSpecifier'Name) txt
      ProjectAndBranch projectName <$> mayBranchName

instance TryFrom Text (ProjectAndBranch ProjectName (Maybe ProjectBranchNameOrLatestRelease)) where
  tryFrom =
    maybeTryFrom (Megaparsec.parseMaybe (projectAndOptionalBranchParser ProjectBranchSpecifier'NameOrLatestRelease))

-- Valid things:
--
--   1. project
--   2. project/
--   3. project/branch
projectAndOptionalBranchParser ::
  forall branch.
  ProjectBranchSpecifier branch ->
  Megaparsec.Parsec Void Text (ProjectAndBranch ProjectName (Maybe branch))
projectAndOptionalBranchParser specifier = do
  (project, hasTrailingSlash) <- projectNameParser
  fmap (ProjectAndBranch project) $
    if hasTrailingSlash
      then optional (projectBranchSpecifierParser specifier)
      else pure Nothing

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
        (project, hasTrailingSlash) <- projectNameParser
        guard hasTrailingSlash
        branch <- projectBranchNameParser False
        pure (ProjectAndBranch (Just project) branch),
      do
        branch <- projectBranchNameParser True
        pure (ProjectAndBranch Nothing branch)
    ]

------------------------------------------------------------------------------------------------------------------------

-- Projects and branches may begin with a "user slug", which looks like "@arya/". This parser parses such slugs,
-- returning just the username (e.g. "arya").
--
-- slug       = @ start-char char* /
-- start-char = alpha
-- char       = alpha | digit | -
userSlugParser :: Megaparsec.Parsec Void Text Text.Builder.Builder
userSlugParser = do
  _ <- Megaparsec.char '@'
  c0 <- Megaparsec.satisfy Char.isAlpha
  c1 <- Megaparsec.takeWhileP Nothing (\c -> Char.isAlpha c || Char.isDigit c || c == '-')
  _ <- Megaparsec.char '/'
  pure (Text.Builder.char c0 <> Text.Builder.text c1)
