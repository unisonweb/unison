-- | This module contains functionality related to computing a "unique type guid lookup" function, which resolves a
-- name to a unique type's GUID to reuse.
module Unison.Codebase.UniqueTypeGuidLookup
  ( loadUniqueTypeGuid,
  )
where

import Data.Map.Strict qualified as Map
import U.Codebase.Branch qualified as Codebase.Branch
import U.Codebase.Decl qualified as Codebase.Decl
import U.Codebase.Reference qualified as Codebase.Reference
import U.Codebase.Sqlite.Operations qualified as Operations
import Unison.Codebase.ProjectPath (ProjectPath)
import Unison.NameSegment (NameSegment)
import Unison.Prelude
import Unison.Sqlite qualified as Sqlite
import Witherable (witherM)

-- | @loadUniqueTypeGuid loadNamespaceAtPath path name@ looks up the GUID associated with the unique type named @name@
-- at child namespace @path@ in the root namespace. If there are multiple such types, an arbitrary one is chosen.
--
-- For (potential) efficiency, this function accepts an argument that loads a namespace at a path, which may be backed
-- by a cache.
loadUniqueTypeGuid ::
  (ProjectPath -> Sqlite.Transaction (Maybe (Codebase.Branch.Branch Sqlite.Transaction))) ->
  ProjectPath ->
  NameSegment ->
  Sqlite.Transaction (Maybe Text)
loadUniqueTypeGuid loadNamespaceAtPath path name =
  loadNamespaceAtPath path >>= \case
    Nothing -> pure Nothing
    Just branch ->
      case Map.lookup name (Codebase.Branch.types branch) of
        Nothing -> pure Nothing
        Just refs0 -> do
          guids <-
            Map.keys refs0 & witherM \case
              Codebase.Reference.ReferenceBuiltin _ -> pure Nothing
              Codebase.Reference.ReferenceDerived id -> do
                decl <- Operations.expectDeclByReference id
                pure case Codebase.Decl.modifier decl of
                  Codebase.Decl.Structural -> Nothing
                  Codebase.Decl.Unique guid -> Just guid
          pure case guids of
            [] -> Nothing
            guid : _ -> Just guid
