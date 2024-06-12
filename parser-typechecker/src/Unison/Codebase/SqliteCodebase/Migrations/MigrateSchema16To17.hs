{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema16To17 (migrateSchema16To17) where

import U.Codebase.Sqlite.ProjectBranch (ProjectBranch (..))
import U.Codebase.Sqlite.Queries qualified as Q
import Unison.Codebase qualified as Codebase
import Unison.Codebase.SqliteCodebase.Operations qualified as Ops
import Unison.Core.Project (ProjectBranchName (UnsafeProjectBranchName), ProjectName (UnsafeProjectName))
import Unison.Sqlite qualified as Sqlite

-- | This migration adds a new table to the schema, `currentProjectPath`, and sets it to contain the path to the scratch project.
migrateSchema16To17 :: Sqlite.Transaction ()
migrateSchema16To17 = do
  Q.expectSchemaVersion 16
  scratchMain <-
    Q.loadProjectBranchByNames scratchProjectName scratchBranchName >>= \case
      Just pb -> pure pb
      Nothing -> do
        (_, emptyCausalHashId) <- Codebase.emptyCausalHash
        (_proj, pb) <- Ops.insertProjectAndBranch scratchProjectName scratchBranchName emptyCausalHashId
        pure pb
  Q.addCurrentProjectPathTable
  Q.setCurrentProjectPath scratchMain.projectId scratchMain.branchId []
  Q.setSchemaVersion 17
  where
    scratchProjectName = UnsafeProjectName "scratch"
    scratchBranchName = UnsafeProjectBranchName "main"
