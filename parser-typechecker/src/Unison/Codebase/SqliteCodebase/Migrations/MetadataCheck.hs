{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Unison.Codebase.SqliteCodebase.Migrations.MetadataCheck (metadataCheck) where

import Control.Lens
import Data.Map qualified as Map
import Data.Set qualified as Set
import U.Codebase.Branch qualified as Branch
import U.Codebase.Causal qualified as Causal
import U.Codebase.Reference (Reference)
import U.Codebase.Sqlite.DbId (BranchHashId)
import U.Codebase.Sqlite.Operations qualified as Ops
import Unison.Codebase.SqliteCodebase.Conversions qualified as Cv
import Unison.Debug qualified as Debug
import Unison.Name (libSegment)
import Unison.NameSegment (NameSegment)
import Unison.Prelude
import Unison.Runtime.IOSource qualified as IOSource
import Unison.Sqlite qualified as Sqlite
import Unison.Util.Monoid (foldMapM)

data MDStuff = MDStuff
  { authors :: Set Reference,
    copyrightHolders :: Set Reference,
    unknownMDTypes :: Set Reference
  }
  deriving (Show)

instance Semigroup MDStuff where
  MDStuff a1 c1 u1 <> MDStuff a2 c2 u2 =
    MDStuff (a1 <> a2) (c1 <> c2) (u1 <> u2)

instance Monoid MDStuff where
  mempty = MDStuff mempty mempty mempty

-- | Adds a table for tracking namespace statistics
-- Adds stats for all existing namespaces, even though missing stats are computed on-demand if missing.
metadataCheck :: Sqlite.Transaction ()
metadataCheck = do
  bhIds <- liveBranchHashIds
  for_ bhIds $ \bhId -> do
    branch <- Ops.expectBranchByBranchHashId bhId
    MDStuff {authors, copyrightHolders, unknownMDTypes} <- allMetadataForBranch analyse branch
    when (Set.size authors > 1) $ Debug.debugM Debug.Migration "Many authors" authors
    when (Set.size copyrightHolders > 1) $ Debug.debugM Debug.Migration "Many holders" authors
    when (Set.size unknownMDTypes > 0) $ Debug.debugM Debug.Migration "Unknown types" unknownMDTypes
  where
    analyse :: Branch.MdValues -> Sqlite.Transaction MDStuff
    analyse (Branch.MdValues mdValues) = do
      mdValues
        & Map.toList
        & foldMapM \(value, typ) ->
          case typ of
            _
              | typ == Cv.reference1to2 IOSource.authorRef -> mempty {authors = Set.singleton value}
              | typ == Cv.reference1to2 IOSource.copyrightHolderRef -> mempty {copyrightHolders = Set.singleton value}
              | typ == Cv.reference1to2 IOSource.guidRef -> mempty
              | typ == Cv.reference1to2 IOSource.isPropagatedReference -> mempty
              | typ == Cv.reference1to2 IOSource.isTestReference -> mempty
              | otherwise -> mempty {unknownMDTypes = Set.singleton typ}
            & pure

-- | Find all causal hashes which have (at some point) been a branch head or loose code
-- root, but are not dependencies.
liveBranchHashIds :: Sqlite.Transaction [BranchHashId]
liveBranchHashIds =
  Sqlite.queryListCol
    [Sqlite.sql|
    SELECT DISTINCT root_branch_hash_id FROM (SELECT root_branch_hash_id FROM name_lookups WHERE NOT EXISTS (SELECT 1 FROM  name_lookup_mounts WHERE mounted_root_branch_hash_id = root_branch_hash_id)) AS live_roots
  |]

allMetadataForBranch :: Monoid r => (Branch.MdValues -> Sqlite.Transaction r) -> Branch.Branch Sqlite.Transaction -> Sqlite.Transaction r
allMetadataForBranch f branch = do
  let mdValuesM =
        (Branch.terms branch, Branch.types branch)
          & toListOf (beside (folded . folded) (folded . folded))
  acc <-
    mdValuesM
      & foldMapM \mdM -> mdM >>= f
  childAcc <-
    (onlyNonLib <$> Branch.nonEmptyChildren branch) >>= foldMapM \cb -> do
      Causal.value cb >>= allMetadataForBranch f
  pure $ acc <> childAcc
  where
    onlyNonLib :: (Map NameSegment b) -> (Map NameSegment b)
    onlyNonLib = Map.filterWithKey \k _ -> k /= libSegment
