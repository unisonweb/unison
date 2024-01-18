module Unison.CommandLine.BranchRelativePath
  ( BranchRelativePath (..),
    parseBranchRelativePath,
    branchRelativePathParser,
    ResolvedBranchRelativePath (..),
  )
where

import Control.Lens (view)
import Data.Char (isSpace)
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
import Unison.Project (ProjectBranchName, ProjectBranchSpecifier (..), ProjectName)
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
              <> Text.Builder.text (Path.convert path)
          )
      These eitherProj path ->
        Text.Builder.run
          ( Text.Builder.text (eitherProjToText eitherProj)
              <> Text.Builder.char ':'
              <> Text.Builder.text (Path.convert path)
          )
    LoosePath path -> Path.toText' path
    where
      eitherProjToText = \case
        Left branchName -> from @(These ProjectName ProjectBranchName) @Text (That branchName)
        Right (projName, branchName) -> into @Text (These projName branchName)

data ResolvedBranchRelativePath
  = ResolvedBranchRelative (Project.ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch) (Maybe Path.Relative)
  | ResolvedLoosePath Path.Absolute

instance From ResolvedBranchRelativePath BranchRelativePath where
  from = \case
    ResolvedBranchRelative (Project.ProjectAndBranch proj branch) mRel -> case mRel of
      Nothing -> BranchRelative (This (Right (view #name proj, view #name branch)))
      Just rel -> BranchRelative (These (Right (view #name proj, view #name branch)) rel)
    ResolvedLoosePath p -> LoosePath (Path.absoluteToPath' p)

instance From ResolvedBranchRelativePath Text where
  from = from . into @BranchRelativePath

branchRelativePathParser :: Megaparsec.Parsec Void Text BranchRelativePath
branchRelativePathParser =
  asum
    [ LoosePath <$> path',
      BranchRelative <$> branchRelative
    ]
  where
    branchRelative :: Megaparsec.Parsec Void Text (These (Either ProjectBranchName (ProjectName, ProjectBranchName)) Path.Relative)
    branchRelative = asum [fullPath, currentBranchRootPath]

    path' = Megaparsec.try do
      offset <- Megaparsec.getOffset
      pathStr <- Megaparsec.takeWhile1P (Just "path char") (not . isSpace)
      case Path.parsePath' (Text.unpack pathStr) of
        Left err -> failureAt offset err
        Right x -> pure x

    relPath = do
      offset <- Megaparsec.getOffset
      path' >>= \(Path.Path' inner) -> case inner of
        Left _ -> failureAt offset "Expected a relative path but found an absolute path"
        Right x -> pure x

    fullPath = do
      projectAndBranchNames <- do
        projectBranch <- Project.projectAndBranchNamesParser ProjectBranchSpecifier'Name
        offset <- Megaparsec.getOffset
        _ <- Megaparsec.char ':'
        case projectBranch of
          This _ -> failureAt offset "Expected a project and branch before the colon (e.g. project/branch:a.path)"
          That pbn -> pure (Left pbn)
          These pn pbn -> pure (Right (pn, pbn))
      optional relPath <&> \case
        Nothing -> This projectAndBranchNames
        Just rp -> These projectAndBranchNames rp

    currentBranchRootPath = do
      _ <- Megaparsec.char ':'
      That <$> relPath

    failureAt :: forall a. Int -> Text -> Megaparsec.Parsec Void Text a
    failureAt offset str =
      Megaparsec.parseError (Megaparsec.FancyError offset (Set.singleton (Megaparsec.ErrorFail (Text.unpack str))))
