module Unison.CommandLine.BranchRelativePath
  ( BranchRelativePath (..),
    parseBranchRelativePath,
    branchRelativePathParser,
  )
where

import Data.Char (isSpace)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.These (These (..))
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char qualified as Megaparsec
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

parseBranchRelativePath :: String -> Either (P.Pretty CT.ColorText) BranchRelativePath
parseBranchRelativePath str =
  case Megaparsec.parse branchRelativePathParser "<none>" (Text.pack str) of
    Left e -> Left (P.string (Megaparsec.errorBundlePretty e))
    Right x -> Right x

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

    failureAt :: forall a. Int -> String -> Megaparsec.Parsec Void Text a
    failureAt offset str = Megaparsec.parseError (Megaparsec.FancyError offset (Set.singleton (Megaparsec.ErrorFail str)))
