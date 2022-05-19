{-# LANGUAGE QuasiQuotes #-}

-- | There are many invariants we expect to hold in our sqlite database and on codebase
-- objects which we can't maintain using database checks. This module performs checks for some
-- of these invariants, which can be useful to run after performing potentially dangerous
-- operations like migrations.
module Unison.Codebase.IntegrityCheck
  ( integrityCheckFullCodebase,
    IntegrityResult (..),
  )
where

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
import Unison.Util.Monoid (foldMapM)
import Prelude hiding (log)

debugLog :: TL.Text -> Sqlite.Transaction ()
debugLog msg = Debug.whenDebug Debug.Integrity $ logInfo msg

logInfo :: TL.Text -> Sqlite.Transaction ()
logInfo msg = Sqlite.unsafeIO $ TL.putStrLn msg

logError :: TL.Text -> Sqlite.Transaction ()
logError msg = logInfo $ "  ⚠️ " <> msg

data IntegrityResult = IntegrityErrorDetected | NoIntegrityErrors
  deriving (Show, Eq)

instance Semigroup IntegrityResult where
  IntegrityErrorDetected <> _ = IntegrityErrorDetected
  _ <> IntegrityErrorDetected = IntegrityErrorDetected
  NoIntegrityErrors <> NoIntegrityErrors = NoIntegrityErrors

instance Monoid IntegrityResult where
  mempty = NoIntegrityErrors

-- | Performs a bevy of checks on causals.
integrityCheckAllCausals :: Sqlite.Transaction IntegrityResult
integrityCheckAllCausals = do
  logInfo "Checking Causal Integrity..."
  Sqlite.queryListRow_ @(DB.CausalHashId, DB.BranchHashId) causalsWithMissingBranchObjects >>= \case
    [] -> pure NoIntegrityErrors
    badCausals -> do
      logError $ "Detected " <> pShow (length badCausals) <> " causals with missing branch objects."
      debugLog . pShow $ badCausals
      pure IntegrityErrorDetected
  where
    causalsWithMissingBranchObjects :: Sqlite.Sql
    causalsWithMissingBranchObjects =
      [here|
          SELECT c.self_hash_id, c.value_hash_id
            FROM causal c
            WHERE NOT EXISTS (SELECT 1 from object o WHERE o.primary_hash_id = c.value_hash_id);
          |]

-- | Performs a bevy of checks on branch objects and their relation to causals.
integrityCheckAllBranches :: Sqlite.Transaction IntegrityResult
integrityCheckAllBranches = do
  logInfo "Checking Namespace Integrity..."
  branchObjIds <- Sqlite.queryListCol_ allBranchObjectIdsSql
  flip foldMapM branchObjIds integrityCheckBranch
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

    integrityCheckBranch :: DB.BranchObjectId -> Sqlite.Transaction IntegrityResult
    integrityCheckBranch objId = do
      dbBranch <- Ops.expectDbBranch objId
      expectedBranchHash <- Helpers.dbBranchHash dbBranch
      actualBranchHash <- Q.expectPrimaryHashByObjectId (DB.unBranchObjectId objId)
      when (expectedBranchHash /= actualBranchHash) $ do
        failure $ "Expected hash for namespace doesn't match actual hash for namespace: " <> pShow (expectedBranchHash, actualBranchHash)
      flip foldMapM (toListOf DBBranch.childrenHashes_ dbBranch) $ \(childObjId, childCausalHashId) -> do
        let checks =
              [ assertBranchObjExists childObjId,
                assertCausalExists childCausalHashId,
                assertCausalValueMatchesObject childCausalHashId childObjId
              ]
        fold <$> sequenceA checks
      where
        assertBranchObjExists branchObjId = do
          Q.loadNamespaceObject @Void (DB.unBranchObjectId branchObjId) (const $ Right ()) >>= \case
            Just _ -> pure NoIntegrityErrors
            Nothing -> do
              failure $ "Expected namespace object for object ID: " <> pShow branchObjId
              pure IntegrityErrorDetected
        assertCausalExists causalHashId = do
          Sqlite.queryOneCol doesCausalExistForCausalHashId (Sqlite.Only causalHashId) >>= \case
            True -> pure NoIntegrityErrors
            False -> do
              failure $ "Expected causal for causal hash ID, but none was found: " <> pShow causalHashId
              pure IntegrityErrorDetected
        assertCausalValueMatchesObject causalHashId branchObjId = do
          -- Assert the object for the causal hash ID matches the given object Id.
          Q.loadBranchObjectIdByCausalHashId causalHashId >>= \case
            Nothing -> do
              failure $ "Expected branch object for causal hash ID: " <> pShow causalHashId
              pure IntegrityErrorDetected
            Just foundBranchId
              | foundBranchId /= branchObjId -> do
                failure $ "Expected child branch object to match canonical object ID for causal hash's namespace: " <> pShow (causalHashId, foundBranchId, branchObjId)
                pure IntegrityErrorDetected
              | otherwise -> pure NoIntegrityErrors
        failure :: TL.Text -> Sqlite.Transaction ()
        failure msg = do
          -- error msg
          logError msg

-- | Performs all available integrity checks.
integrityCheckFullCodebase :: Sqlite.Transaction IntegrityResult
integrityCheckFullCodebase = do
  fmap fold . sequenceA $
    [ integrityCheckAllBranches,
      integrityCheckAllCausals
    ]
