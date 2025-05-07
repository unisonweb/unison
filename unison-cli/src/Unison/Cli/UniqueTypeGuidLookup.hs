-- | This module contains functionality related to computing a "unique type guid lookup" function, which resolves a
-- name to a unique type's GUID to reuse.
module Unison.Cli.UniqueTypeGuidLookup
  ( loadUniqueTypeGuid,
  )
where

import Data.Map.Strict qualified as Map
import U.Codebase.Branch qualified as Codebase.Branch
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ProjectPath (ProjectPath)
import Unison.Codebase.ProjectPath qualified as PP
import Unison.Codebase.UniqueTypeGuidLookup qualified as Codebase
import Unison.Name (Name)
import Unison.Prelude
import Unison.Sqlite qualified as Sqlite

loadUniqueTypeGuid :: ProjectPath -> Name -> Sqlite.Transaction (Maybe Text)
loadUniqueTypeGuid pp name0 = do
  let (namePath, finalSegment) = Path.splitFromName name0
  let fullPP = pp & over PP.path_ (<> namePath)

  -- Define an operation to load a branch by its full path from the root namespace.
  --
  -- This ought to probably lean somewhat on a cache (so long as the caller is aware of the cache, and discrads it at
  -- an appropriate time, such as after the current unison file finishes parsing).
  let loadBranchAtPath :: ProjectPath -> Sqlite.Transaction (Maybe (Codebase.Branch.Branch Sqlite.Transaction))
      loadBranchAtPath = Codebase.getMaybeShallowBranchAtProjectPath

  Codebase.loadUniqueTypeGuid loadBranchAtPath fullPP finalSegment >>= \case
    Nothing ->
      Queries.loadMergeBranchParents pp.project.projectId pp.branch.branchId >>= \case
        Nothing -> pure Nothing
        Just (bobMaybeBranchId, bobCausalHashId, aliceMaybeBranchId, aliceCausalHashId) -> do
          aliceNamespaceHashId <- Queries.expectCausalValueHashId aliceCausalHashId
          bobNamespaceHashId <- Queries.expectCausalValueHashId bobCausalHashId

          aliceUniqueTypeGuids <- Queries.loadNamespaceUniqueTypeGuids aliceNamespaceHashId
          bobUniqueTypeGuids <- Queries.loadNamespaceUniqueTypeGuids bobNamespaceHashId

          case (Map.lookup name0 aliceUniqueTypeGuids, Map.lookup name0 bobUniqueTypeGuids) of
            -- A few simple cases – reuse a GUID if it is sensible to do so
            (Just aliceGuid, Just bobGuid) | aliceGuid == bobGuid -> pure (Just aliceGuid)
            (Just aliceGuid, Nothing) -> pure (Just aliceGuid)
            (Nothing, Just bobGuid) -> pure (Just bobGuid)
            (Nothing, Nothing) -> pure Nothing
            -- If alice and bob have different guids, and there is a parent-child relationship between them (i.e. alice
            -- was directly branched off of bob or vice versa), then prefer the parent's GUID. Otherwise, just make up
            -- a new GUID because it's not clear whether alice's or bob's should be preferred.
            (Just aliceGuid, Just bobGuid) -> do
              case (aliceMaybeBranchId, bobMaybeBranchId) of
                (Just aliceBranchId, Just bobBranchId) ->
                  Queries.loadProjectBranchParent pp.project.projectId aliceBranchId >>= \case
                    Just aliceParentBranchId ->
                      if aliceParentBranchId == bobBranchId
                        then pure (Just bobGuid)
                        else
                          Queries.loadProjectBranchParent pp.project.projectId bobBranchId <&> \case
                            Just bobParentBranchId | bobParentBranchId == aliceBranchId -> Just aliceGuid
                            _ -> Nothing
                    Nothing -> pure Nothing
                _ -> pure Nothing
    Just guid -> pure (Just guid)
