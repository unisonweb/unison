module Unison.CommandLine.BranchRelativePath
  ( BranchRelativePath (..),
    parseBranchRelativePath,
    branchRelativePathParser,
    ResolvedBranchRelativePath (..),
    parseIncrementalBranchRelativePath,
    IncrementalBranchRelativePath (..),
  )
where

import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.These (These (..))
import Text.Builder qualified
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char qualified as Megaparsec
import U.Codebase.Sqlite.Project qualified as Sqlite
import U.Codebase.Sqlite.ProjectBranch qualified as Sqlite
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.Path.Parse qualified as Path
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchName, ProjectName)
import Unison.Project qualified as Project
import Unison.Util.ColorText qualified as CT
import Unison.Util.Pretty qualified as P

data BranchRelativePath
  = BranchRelative (These (Either ProjectBranchName (ProjectName, ProjectBranchName)) Path.Relative)
  | LoosePath Path.Path'
  deriving stock (Eq, Show)

-- | Strings without colons are parsed as loose code paths. A path with a colon may specify:
-- 1. A project and branch
-- 2. Only a branch, in which case the project is assumed to be the current project
-- 3. Only a path, in which case the path is rooted at the branch root
--
-- Specifying only a project is not allowed.
--
-- >>> parseBranchRelativePath "foo"
-- Right (LoosePath foo)
-- >>> parseBranchRelativePath "foo/bar:"
-- Right (BranchRelative (This (Right (UnsafeProjectName "foo",UnsafeProjectBranchName "bar"))))
-- >>> parseBranchRelativePath "foo/bar:some.path"
-- Right (BranchRelative (These (Right (UnsafeProjectName "foo",UnsafeProjectBranchName "bar")) some.path))
-- >>> parseBranchRelativePath "/bar:some.path"
-- Right (BranchRelative (These (Left (UnsafeProjectBranchName "bar")) some.path))
-- >>> parseBranchRelativePath ":some.path"
-- Right (BranchRelative (That some.path))
parseBranchRelativePath :: String -> Either (P.Pretty CT.ColorText) BranchRelativePath
parseBranchRelativePath str =
  case Megaparsec.parse branchRelativePathParser "<none>" (Text.pack str) of
    Left e -> Left (P.string (Megaparsec.errorBundlePretty e))
    Right x -> Right x

instance From BranchRelativePath Text where
  from = \case
    BranchRelative brArg -> case brArg of
      This eitherProj ->
        Text.Builder.run
          ( Text.Builder.text (eitherProjToText eitherProj)
              <> Text.Builder.char ':'
          )
      That path ->
        Text.Builder.run
          ( Text.Builder.char ':'
              <> Text.Builder.text (Path.toText' $ Path.RelativePath' path)
          )
      These eitherProj path ->
        Text.Builder.run
          ( Text.Builder.text (eitherProjToText eitherProj)
              <> Text.Builder.char ':'
              <> Text.Builder.text (Path.toText' $ Path.RelativePath' path)
          )
    LoosePath path -> Path.toText' path
    where
      eitherProjToText = \case
        Left branchName -> from @(These ProjectName ProjectBranchName) @Text (That branchName)
        Right (projName, branchName) -> into @Text (These projName branchName)

data ResolvedBranchRelativePath
  = ResolvedBranchRelative (ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch) (Maybe Path.Relative)
  | ResolvedLoosePath Path.Absolute

instance From ResolvedBranchRelativePath BranchRelativePath where
  from = \case
    ResolvedBranchRelative (ProjectAndBranch proj branch) mRel -> case mRel of
      Nothing -> BranchRelative (This (Right (view #name proj, view #name branch)))
      Just rel -> BranchRelative (These (Right (view #name proj, view #name branch)) rel)
    ResolvedLoosePath p -> LoosePath (Path.absoluteToPath' p)

instance From ResolvedBranchRelativePath Text where
  from = from . into @BranchRelativePath

data IncrementalBranchRelativePath
  = -- | no dots, slashes, or colons
    ProjectOrRelative Text Path.Path'
  | -- | dots, no slashes or colons
    LooseCode Path.Path'
  | -- | valid project, no slash
    IncompleteProject ProjectName
  | -- | valid project/branch, slash, no colon
    IncompleteBranch (Maybe ProjectName) (Maybe ProjectBranchName)
  | -- | valid project/branch, with colon
    IncompletePath (Either (ProjectAndBranch ProjectName ProjectBranchName) ProjectBranchName) (Maybe Path.Relative)
  | PathRelativeToCurrentBranch Path.Relative
  deriving stock (Show)

-- |
-- >>> parseIncrementalBranchRelativePath "foo"
-- Right (ProjectOrRelative "foo" foo)
--
-- >>> parseIncrementalBranchRelativePath "foo/bar:"
-- Right (IncompletePath (Left (ProjectAndBranch {project = UnsafeProjectName "foo", branch = UnsafeProjectBranchName "bar"})) Nothing)
--
-- >>> parseIncrementalBranchRelativePath "foo/bar:some.path"
-- Right (IncompletePath (Left (ProjectAndBranch {project = UnsafeProjectName "foo", branch = UnsafeProjectBranchName "bar"})) (Just some.path))
--
-- >>> parseIncrementalBranchRelativePath "/bar:some.path"
-- Right (IncompletePath (Right (UnsafeProjectBranchName "bar")) (Just some.path))
--
-- >>> parseIncrementalBranchRelativePath ":some.path"
-- Right (PathRelativeToCurrentBranch some.path)
--
-- >>> parseIncrementalBranchRelativePath "/branch"
-- Right (IncompleteBranch Nothing (Just (UnsafeProjectBranchName "branch")))
--
-- >>> parseIncrementalBranchRelativePath "/"
-- Right (IncompleteBranch Nothing Nothing)
parseIncrementalBranchRelativePath :: String -> Either (P.Pretty CT.ColorText) IncrementalBranchRelativePath
parseIncrementalBranchRelativePath str =
  case Megaparsec.parse incrementalBranchRelativePathParser "<none>" (Text.pack str) of
    Left e -> Left (P.string (Megaparsec.errorBundlePretty e))
    Right x -> Right x

incrementalBranchRelativePathParser :: Megaparsec.Parsec Void Text IncrementalBranchRelativePath
incrementalBranchRelativePathParser =
  asum
    [ startingAtSlash Nothing,
      pathRelativeToCurrentBranch,
      projectName
    ]
  where
    projectName = do
      -- Attempt to parse a project name from the string prefix, or a
      -- Path' cosuming the entire string, switch based on if we
      -- unambiguously parse one or the other.
      parseThese Project.projectNameParser path' >>= \case
        -- project name parser consumed the slash
        This (_, (projectName, True)) ->
          startingAtBranch (Just projectName)
        -- project name parser did not consume a slash
        --
        -- Either we are at the end of input or the next character
        -- is not a slash, so we have invalid input
        This (_, (projectName, False)) ->
          let end = do
                Megaparsec.eof
                pure (IncompleteProject projectName)
           in end <|> startingAtSlash (Just projectName)
        -- The string doesn't parse as a project name but does parse as a path
        That (_, path) -> pure (LooseCode path)
        -- The string parses both as a project name and a path
        These _ (_, path) -> ProjectOrRelative <$> Megaparsec.takeRest <*> pure path

    startingAtBranch :: Maybe ProjectName -> Megaparsec.Parsec Void Text IncrementalBranchRelativePath
    startingAtBranch mproj =
      optionalBranch >>= \case
        Nothing -> pure (IncompleteBranch mproj Nothing)
        Just branch ->
          startingAtColon (maybe (Right branch) (\proj -> Left (ProjectAndBranch proj branch)) mproj)
            <|> pure (IncompleteBranch mproj (Just branch))

    startingAtSlash ::
      Maybe ProjectName ->
      Megaparsec.Parsec Void Text IncrementalBranchRelativePath
    startingAtSlash mproj = Megaparsec.char '/' *> startingAtBranch mproj

    startingAtColon ::
      (Either (ProjectAndBranch ProjectName ProjectBranchName) ProjectBranchName) ->
      Megaparsec.Parsec Void Text IncrementalBranchRelativePath
    startingAtColon projStuff = do
      _ <- Megaparsec.char ':'
      p <- optionalEof relPath
      pure (IncompletePath projStuff p)

    pathRelativeToCurrentBranch :: Megaparsec.Parsec Void Text IncrementalBranchRelativePath
    pathRelativeToCurrentBranch = do
      _ <- Megaparsec.char ':'
      p <- relPath
      pure (PathRelativeToCurrentBranch p)

    optionalEof :: Megaparsec.Parsec Void Text a -> Megaparsec.Parsec Void Text (Maybe a)
    optionalEof pa = Just <$> pa <|> Nothing <$ Megaparsec.eof

    optionalBranch :: Megaparsec.Parsec Void Text (Maybe ProjectBranchName)
    optionalBranch = optionalEof branchNameParser

    branchNameParser = Project.projectBranchNameParser False

    relPath = do
      offset <- Megaparsec.getOffset
      path' >>= \(Path.Path' inner) -> case inner of
        Left _ -> failureAt offset "Expected a relative path but found an absolute path"
        Right x -> pure x
    path' = Megaparsec.try do
      offset <- Megaparsec.getOffset
      pathStr <- Megaparsec.takeRest
      case Path.parsePath' (Text.unpack pathStr) of
        Left err -> failureAt offset err
        Right x -> pure x

    failureAt :: forall a. Int -> Text -> Megaparsec.Parsec Void Text a
    failureAt offset str = Megaparsec.parseError (Megaparsec.FancyError offset (Set.singleton (Megaparsec.ErrorFail (Text.unpack str))))

    parseThese ::
      forall a b.
      Megaparsec.Parsec Void Text a ->
      Megaparsec.Parsec Void Text b ->
      Megaparsec.Parsec Void Text (These (Int, a) (Int, b))
    parseThese pa pb = do
      ea <- Megaparsec.observing $ Megaparsec.lookAhead $ Megaparsec.try $ first Text.length <$> Megaparsec.match pa
      eb <- Megaparsec.observing $ Megaparsec.lookAhead $ Megaparsec.try $ first Text.length <$> Megaparsec.match pb
      case (ea, eb) of
        (Left aerr, Left berr) ->
          Megaparsec.parseError (aerr <> berr)
        (Left _, Right (blen, b)) -> do
          Megaparsec.takeP Nothing blen
          pure (That (blen, b))
        (Right (alen, a), Left _) -> do
          Megaparsec.takeP Nothing alen
          pure (This (alen, a))
        (Right a, Right b) -> pure (These a b)

branchRelativePathParser :: Megaparsec.Parsec Void Text BranchRelativePath
branchRelativePathParser =
  incrementalBranchRelativePathParser >>= \case
    ProjectOrRelative _txt path -> pure (LoosePath path)
    LooseCode path -> pure (LoosePath path)
    IncompleteProject _proj -> fail "Branch relative paths require a branch. Expected `/` here."
    IncompleteBranch _mproj _mbranch -> fail "Branch relative paths require a colon. Expected `:` here."
    PathRelativeToCurrentBranch p -> pure (BranchRelative (That p))
    IncompletePath projStuff mpath ->
      case projStuff of
        Left (ProjectAndBranch projName branchName) -> case mpath of
          Nothing -> pure (BranchRelative (This (Right (projName, branchName))))
          Just path -> pure (BranchRelative (These (Right (projName, branchName)) path))
        Right branch -> case mpath of
          Nothing -> pure (BranchRelative (This (Left branch)))
          Just path -> pure (BranchRelative (These (Left branch) path))
