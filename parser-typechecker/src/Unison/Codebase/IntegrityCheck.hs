{-# LANGUAGE QuasiQuotes #-}

module Unison.Codebase.IntegrityCheck (integrityCheckAllBranches, integrityCheckAllCausals) where

import Control.Lens
import Data.String.Here.Uninterpolated (here)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import Data.Void
import Text.Pretty.Simple
import qualified U.Codebase.Sqlite.Branch.Full as DBBranch
import qualified U.Codebase.Sqlite.DbId as DB
import qualified U.Codebase.Sqlite.Operations as Ops
import qualified U.Codebase.Sqlite.Queries as Q
import qualified Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema1To2.DbHelpers as Helpers
import qualified Unison.Debug as Debug
import Unison.Prelude
import qualified Unison.Sqlite as Sqlite
import Prelude hiding (log)

debugLog :: TL.Text -> Sqlite.Transaction ()
debugLog msg = Debug.whenDebug Debug.Integrity $ logInfo msg

logInfo :: TL.Text -> Sqlite.Transaction ()
logInfo msg = Sqlite.unsafeIO $ TL.putStrLn msg

logError :: TL.Text -> Sqlite.Transaction ()
logError msg = logInfo $ "  ⚠️ " <> msg

integrityCheckAllCausals :: Sqlite.Transaction ()
integrityCheckAllCausals = do
  logInfo "Checking Causals..."
  Sqlite.queryListRow_ @(DB.CausalHashId, DB.BranchHashId) causalsWithMissingBranchObjects >>= \case
    [] -> pure ()
    badCausals -> do
      logError $ "Detected " <> pShow (length badCausals) <> " causals witch missing branch objects."
      debugLog . pShow $ badCausals
  where
    causalsWithMissingBranchObjects :: Sqlite.Sql
    causalsWithMissingBranchObjects =
      [here|
          SELECT c.self_hash_id, c.value_hash_id
            FROM causal c
            WHERE NOT EXISTS (SELECT 1 from object o WHERE o.primary_hash_id = c.value_hash_id);
          |]

integrityCheckAllBranches :: Sqlite.Transaction ()
integrityCheckAllBranches = do
  logInfo "Checking Namespaces..."
  branchObjIds <- Sqlite.queryListCol_ allBranchObjectIdsSql
  for_ branchObjIds integrityCheckBranch
  where
    allBranchObjectIdsSql :: Sqlite.Sql
    allBranchObjectIdsSql =
      [here|
          SELECT id FROM object WHERE type_id = 2;
          |]

    doesCausalExistForCausalHashId :: Sqlite.Sql
    doesCausalExistForCausalHashId =
      [here|
          SELECT EXISTS (SELECT 1 FROM causal WHERE self_hash_id = ?)
      |]

    integrityCheckBranch :: DB.BranchObjectId -> Sqlite.Transaction ()
    integrityCheckBranch objId = do
      dbBranch <- Ops.expectDbBranch objId
      expectedBranchHash <- Helpers.dbBranchHash dbBranch
      actualBranchHash <- Q.expectPrimaryHashByObjectId (DB.unBranchObjectId objId)
      when (expectedBranchHash /= actualBranchHash) $ do
        failure $ "Expected hash for namespace doesn't match actual hash for namespace: " <> pShow (expectedBranchHash, actualBranchHash)
      forOf_ DBBranch.childrenHashes_ dbBranch $ \(childObjId, childCausalHashId) -> do
        -- Assert the child branch object exists
        Q.loadNamespaceObject @Void (DB.unBranchObjectId childObjId) (const $ Right ()) >>= \case
          Nothing -> failure $ "Expected namespace object for object ID: " <> pShow childObjId
          Just _ -> pure ()
        Sqlite.queryOneCol doesCausalExistForCausalHashId (Sqlite.Only childCausalHashId) >>= \case
          True -> pure ()
          False -> failure $ "Expected causal for causal hash ID, but none was found: " <> pShow childCausalHashId
        -- Assert the object for the causal hash ID matches the given object Id.
        Q.loadBranchObjectIdByCausalHashId childCausalHashId >>= \case
          Nothing -> failure $ "Expected branch object for causal hash ID: " <> pShow childCausalHashId
          Just foundBranchId
            | foundBranchId /= childObjId -> do
              failure $ "Expected child branch object to match canonical object ID for causal hash's namespace: " <> pShow (childCausalHashId, foundBranchId, childObjId)
            | otherwise -> pure ()
        pure ()
      where
        failure :: TL.Text -> Sqlite.Transaction ()
        failure msg = do
          -- error msg
          logError msg
