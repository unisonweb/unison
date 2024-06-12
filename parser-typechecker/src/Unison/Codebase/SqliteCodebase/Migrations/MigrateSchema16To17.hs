{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema16To17 (migrateSchema16To17) where

import Control.Lens
import Data.Text qualified as Text
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import U.Codebase.Branch.Type qualified as V2Branch
import U.Codebase.Causal qualified as V2Causal
import U.Codebase.Sqlite.DbId (ProjectBranchId (..), ProjectId (..))
import U.Codebase.Sqlite.ProjectBranch (ProjectBranch (..))
import U.Codebase.Sqlite.Queries qualified as Q
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.SqliteCodebase.Operations qualified as Ops
import Unison.Core.Project (ProjectBranchName (UnsafeProjectBranchName), ProjectName (UnsafeProjectName))
import Unison.NameSegment (NameSegment)
import Unison.NameSegment.Internal (NameSegment (NameSegment))
import Unison.NameSegment.Internal qualified as NameSegment
import Unison.Prelude
import Unison.Sqlite qualified as Sqlite
import Unison.Syntax.NameSegment qualified as NameSegment
import UnliftIO qualified
import UnliftIO qualified as UnsafeIO

-- | This migration converts the codebase from having all projects in a single codebase root to having separate causal
-- roots for each project branch.
-- It:
-- * adds a new table to the schema, `currentProjectPath`, and sets it to contain the path to the scratch project.
-- * Adds the causal_hash_id column to the project_branch table.
--
-- It requires a Connection argument rather than working inside a Transaction because it needs to temporarily disable
-- foreign key checking, and the foreign_key pragma cannot be set within a transaction.
migrateSchema16To17 :: Sqlite.Connection -> IO ()
migrateSchema16To17 conn = withDisabledForeignKeys $ do
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
  addCausalHashesToProjectBranches
  -- TODO: Add causal hash id to project branch table and migrate existing project branches somehow
  Q.setSchemaVersion 17
  where
    scratchProjectName = UnsafeProjectName "scratch"
    scratchBranchName = UnsafeProjectBranchName "main"
    withDisabledForeignKeys :: Sqlite.Transaction r -> IO r
    withDisabledForeignKeys m = do
      let disable = Sqlite.runWriteTransaction conn \run -> run $ Sqlite.execute [Sqlite.sql| PRAGMA foreign_keys=OFF |]
      let enable = Sqlite.runWriteTransaction conn \run -> run $ Sqlite.execute [Sqlite.sql| PRAGMA foreign_keys=ON |]
      let action = Sqlite.runWriteTransaction conn \run -> run $ m
      UnsafeIO.bracket disable (const enable) (const action)

newtype ForeignKeyFailureException
  = ForeignKeyFailureException
      -- We leave the data as raw as possible to ensure we can display it properly rather than get decoding errors while
      -- trying to display some other error.
      [[Sqlite.SQLData]]
  deriving stock (Show)
  deriving anyclass (Exception)

addCausalHashesToProjectBranches :: Sqlite.Transaction ()
addCausalHashesToProjectBranches = do
  -- Create the new version of the project_branch table with the causal_hash_id column.
  Sqlite.execute
    [Sqlite.sql|
CREATE TABLE new_project_branch (
  project_id uuid NOT NULL REFERENCES project (id),
  branch_id uuid NOT NULL,
  name text NOT NULL,
  causal_hash_id integer NOT NULL REFERENCES causal(self_hash_id),

  primary key (project_id, branch_id),

  unique (project_id, name)
)
without rowid;
|]
  rootCausalHashId <- Q.expectNamespaceRoot
  rootCh <- Q.expectCausalHash rootCausalHashId
  projectsRoot <- Codebase.getShallowCausalAtPathFromRootHash rootCh (Path.singleton $ NameSegment.unsafeParseText "__projects") >>= V2Causal.value
  ifor_ (V2Branch.children projectsRoot) \projectIdNS projectsCausal -> do
    projectId <- case projectIdNS of
      UUIDNameSegment projectIdUUID -> pure $ ProjectId projectIdUUID
      _ -> error $ "Invalid Project Id NameSegment:" <> show projectIdNS
    projectsBranch <- V2Causal.value projectsCausal
    ifor_ (V2Branch.children projectsBranch) \branchIdNS projectBranchCausal -> void . runMaybeT $ do
      projectBranchId <- case branchIdNS of
        UUIDNameSegment branchIdUUID -> pure $ ProjectBranchId branchIdUUID
        _ -> error $ "Invalid Branch Id NameSegment:" <> show branchIdNS
      let branchCausalHash = V2Causal.causalHash projectBranchCausal
      causalHashId <- lift $ Q.expectCausalHashIdByCausalHash branchCausalHash
      ProjectBranch {name = branchName} <- MaybeT $ Q.loadProjectBranch projectId projectBranchId
      -- Insert the full project branch with HEAD into the new table
      lift $
        Sqlite.execute
          [Sqlite.sql|
          INSERT INTO new_project_branch (project_id, branch_id, name, causal_hash_id)
            VALUES (:projectId, :projectBranchId, :branchName, :causalHashId)
        |]

  -- Delete any project branch data that don't have a matching branch in the current root.
  -- This is to make sure any old or invalid project branches get cleared out and won't cause problems when we rewrite
  -- foreign key references.
  -- We have to do this manually since we had to disable foreign key checks to add the new column.
  Sqlite.execute
    [Sqlite.sql| DELETE FROM project_branch_parent pbp
      WHERE NOT EXISTS(SELECT 1 FROM new_project_branch npb WHERE npb.project_id = pbp.project_id AND npb.branch_id = pbp.branch_id)
    |]
  Sqlite.execute
    [Sqlite.sql| DELETE FROM project_branch_remote_mapping pbrp
      WHERE NOT EXISTS(SELECT 1 FROM new_project_branch npb WHERE npb.project_id = pbrp.local_project_id AND npb.branch_id = pbrp.local_branch_id)
    |]

  -- Drop the old project_branch table and rename the new one to take its place.
  Sqlite.execute [Sqlite.sql| DROP TABLE project_branch |]
  Sqlite.execute [Sqlite.sql| ALTER TABLE new_project_branch RENAME TO project_branch |]
  foreignKeyErrs <- Sqlite.queryListRow [Sqlite.sql| PRAGMA foreign_key_check |]
  when (not . null $ foreignKeyErrs) . Sqlite.unsafeIO . UnliftIO.throwIO $ ForeignKeyFailureException foreignKeyErrs

-- migrateLooseCodeIntoLegacyProject :: Sqlite.Transaction ()
-- migrateLooseCodeIntoLegacyProject = do ()

pattern UUIDNameSegment :: UUID -> NameSegment
pattern UUIDNameSegment uuid <-
  ( NameSegment.toUnescapedText ->
      (Text.uncons -> Just ('_', UUID.fromText . Text.map (\c -> if c == '_' then '-' else c) -> Just uuid))
    )
  where
    UUIDNameSegment uuid =
      NameSegment (Text.cons '_' (Text.map (\c -> if c == '-' then '_' else c) (UUID.toText uuid)))
