-- | Helpers for working with various kinds of reflogs.
module Unison.Codebase.Editor.HandleInput.Reflogs
  ( showProjectBranchReflog,
    showProjectReflog,
    showGlobalReflog,
  )
where

import Control.Monad.Reader
import Data.Time (getCurrentTime)
import U.Codebase.HashTags (CausalHash)
import U.Codebase.Sqlite.Project (Project)
import U.Codebase.Sqlite.ProjectBranch (ProjectBranch)
import U.Codebase.Sqlite.ProjectReflog qualified as ProjectReflog
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.ProjectUtils qualified as ProjectUtils
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.ShortCausalHash qualified as SCH
import Unison.Core.Project (ProjectAndBranch (..), ProjectBranchName, ProjectName)
import Unison.Prelude
import Unison.Sqlite qualified as Sqlite

showProjectBranchReflog :: Maybe (ProjectAndBranch (Maybe ProjectName) ProjectBranchName) -> Cli ()
showProjectBranchReflog mayProjectAndBranch = do
  ProjectAndBranch _project branch <- case mayProjectAndBranch of
    Nothing -> Cli.getCurrentProjectAndBranch
    Just pab -> ProjectUtils.resolveProjectBranch (second Just pab)
  reflogHelper (\n -> Codebase.getProjectBranchReflog n (branch ^. #branchId))

showProjectReflog :: Maybe ProjectName -> Cli ()
showProjectReflog mayProject = do
  ProjectAndBranch project _ <- ProjectUtils.resolveProjectBranch (ProjectAndBranch mayProject Nothing)
  reflogHelper (\n -> Codebase.getProjectReflog n (project ^. #projectId))

showGlobalReflog :: Cli ()
showGlobalReflog = do
  reflogHelper Codebase.getGlobalReflog

reflogHelper :: (Int -> Sqlite.Transaction [ProjectReflog.Entry Project ProjectBranch CausalHash]) -> Cli ()
reflogHelper getEntries = do
  let numEntriesToShow = 500
  entries <-
    Cli.runTransaction $ do
      schLength <- Codebase.branchHashLength
      entries <- getEntries numEntriesToShow
      entries
        & (fmap . fmap) (\ch -> (ch, SCH.fromHash schLength ch))
        & pure
  let moreEntriesToLoad =
        if length entries == numEntriesToShow
          then Output.MoreEntriesThanShown
          else Output.AllEntriesShown
  mayNow <-
    asks Cli.isTranscriptTest >>= \case
      True -> pure Nothing
      False -> Just <$> liftIO getCurrentTime
  Cli.respondNumbered $ Output.ShowProjectBranchReflog mayNow moreEntriesToLoad entries
