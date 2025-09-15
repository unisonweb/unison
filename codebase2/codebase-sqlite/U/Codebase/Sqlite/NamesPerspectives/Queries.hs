{-# OPTIONS_GHC -Wno-deprecations #-}

module U.Codebase.Sqlite.NamesPerspectives.Queries {-# DEPRECATED "See module doc" #-} where

import Data.List qualified as List
import Data.List.Extra qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import GHC.Stack (callStack)
import U.Codebase.Sqlite.DbId
  ( BranchHashId (..),
    HashId (..),
  )
import U.Codebase.Sqlite.NameLookups
import U.Codebase.Sqlite.NamedRef (NamedRef)
import U.Codebase.Sqlite.NamedRef qualified as NamedRef
import U.Codebase.Sqlite.Orphans ()
import U.Codebase.Sqlite.Reference qualified as S
import U.Codebase.Sqlite.Referent qualified as S (TextReferent)
import Unison.Debug qualified as Debug
import Unison.Hash32.Orphans.Sqlite ()
import Unison.Prelude
import Unison.Sqlite

-- | Copies existing name lookup rows but replaces their branch hash id;
-- This is a low-level operation used as part of deriving a new name lookup index
-- from an existing one as performantly as possible.
copyScopedNameLookup :: BranchHashId -> BranchHashId -> Transaction ()
copyScopedNameLookup fromBHId toBHId = do
  execute termsCopySql
  execute typesCopySql
  where
    termsCopySql =
      [sql|
        INSERT INTO scoped_term_name_lookup(root_branch_hash_id, reversed_name, last_name_segment, namespace, referent_builtin, referent_component_hash, referent_component_index, referent_constructor_index, referent_constructor_type)
        SELECT :toBHId, reversed_name, last_name_segment, namespace, referent_builtin, referent_component_hash, referent_component_index, referent_constructor_index, referent_constructor_type
        FROM scoped_term_name_lookup
        WHERE root_branch_hash_id = :fromBHId
      |]
    typesCopySql =
      [sql|
        INSERT INTO scoped_type_name_lookup(root_branch_hash_id, reversed_name, last_name_segment, namespace, reference_builtin, reference_component_hash, reference_component_index)
        SELECT :toBHId, reversed_name, last_name_segment, namespace, reference_builtin, reference_component_hash, reference_component_index
        FROM scoped_type_name_lookup
        WHERE root_branch_hash_id = :fromBHId
      |]

-- | Delete the specified name lookup.
-- This should only be used if you're sure it's unused, or if you're going to re-create it in
-- the same transaction.
deleteNameLookup :: BranchHashId -> Transaction ()
deleteNameLookup bhId = do
  execute
    [sql|
      DELETE FROM name_lookups
      WHERE root_branch_hash_id = :bhId
    |]

-- | Inserts a new record into the name_lookups table
trackNewBranchHashNameLookup :: BranchHashId -> Transaction ()
trackNewBranchHashNameLookup bhId = do
  execute
    [sql|
      INSERT INTO name_lookups (root_branch_hash_id)
      VALUES (:bhId)
    |]

-- | Check if we've already got an index for the desired root branch hash.
checkBranchHashNameLookupExists :: BranchHashId -> Transaction Bool
checkBranchHashNameLookupExists hashId = do
  queryOneCol
    [sql|
      SELECT EXISTS (
        SELECT 1
        FROM name_lookups
        WHERE root_branch_hash_id = :hashId
        LIMIT 1
      )
    |]

-- | Delete any name lookup that's not in the provided list.
--
-- This can be used to garbage collect unreachable name lookups.
deleteNameLookupsExceptFor :: [BranchHashId] -> Transaction ()
deleteNameLookupsExceptFor hashIds = do
  case hashIds of
    [] -> execute [sql| DELETE FROM name_lookups |]
    (x : xs) -> do
      let hashIdValues :: NonEmpty (Only BranchHashId)
          hashIdValues = coerce (x NonEmpty.:| xs)
      execute
        [sql|
          WITH RECURSIVE reachable(branch_hash_id) AS (
            VALUES :hashIdValues
            -- Any name lookup that's mounted on a reachable name lookup is also reachable
            UNION ALL
            SELECT mounted_root_branch_hash_id FROM name_lookup_mounts JOIN reachable ON branch_hash_id = parent_root_branch_hash_id
          )
          DELETE FROM name_lookups
            WHERE root_branch_hash_id NOT IN (SELECT branch_hash_id FROM reachable);
        |]

-- | Insert the given set of term names into the name lookup table
insertScopedTermNames :: BranchHashId -> [NamedRef (S.TextReferent, Maybe NamedRef.ConstructorType)] -> Transaction ()
insertScopedTermNames bhId = do
  traverse_ \name0 -> do
    let name = NamedRef.ScopedRow (refToRow <$> name0)
    execute
      [sql|
        INSERT INTO scoped_term_name_lookup (
          root_branch_hash_id,
          reversed_name,
          namespace,
          last_name_segment,
          referent_builtin,
          referent_component_hash,
          referent_component_index,
          referent_constructor_index,
          referent_constructor_type
        )
        VALUES (:bhId, @name, @, @, @, @, @, @, @)
      |]
  where
    refToRow :: (S.TextReferent, Maybe NamedRef.ConstructorType) -> (S.TextReferent :. Only (Maybe NamedRef.ConstructorType))
    refToRow (ref, ct) = ref :. Only ct

-- | Insert the given set of type names into the name lookup table
insertScopedTypeNames :: BranchHashId -> [NamedRef S.TextReference] -> Transaction ()
insertScopedTypeNames bhId =
  traverse_ \name0 -> do
    let name = NamedRef.ScopedRow name0
    execute
      [sql|
        INSERT INTO scoped_type_name_lookup (
          root_branch_hash_id,
          reversed_name,
          namespace,
          last_name_segment,
          reference_builtin,
          reference_component_hash,
          reference_component_index
        )
        VALUES (:bhId, @name, @, @, @, @, @)
      |]

-- | Remove the given set of term names into the name lookup table
removeScopedTermNames :: BranchHashId -> [NamedRef S.TextReferent] -> Transaction ()
removeScopedTermNames bhId names = do
  for_ names \name ->
    execute
      [sql|
        DELETE FROM scoped_term_name_lookup
        WHERE root_branch_hash_id IS :bhId
          AND reversed_name IS @name
          AND referent_builtin IS @
          AND referent_component_hash IS @
          AND referent_component_index IS @
          AND referent_constructor_index IS @
      |]

-- | Remove the given set of term names into the name lookup table
removeScopedTypeNames :: BranchHashId -> [NamedRef S.TextReference] -> Transaction ()
removeScopedTypeNames bhId names = do
  for_ names \name ->
    execute
      [sql|
        DELETE FROM scoped_type_name_lookup
        WHERE root_branch_hash_id IS :bhId
          AND reversed_name IS @name
          AND reference_builtin IS @
          AND reference_component_hash IS @
          AND reference_component_index IS @
      |]

-- | NOTE: requires that the codebase has an up-to-date name lookup index. As of writing, this
-- is only true on Share.
--
-- Get the list of a term names in the provided name lookup and relative namespace.
-- Includes dependencies, but not transitive dependencies.
termNamesWithinNamespace :: BranchHashId -> PathSegments -> Transaction [NamedRef (S.TextReferent, Maybe NamedRef.ConstructorType)]
termNamesWithinNamespace bhId namespace = do
  results :: [NamedRef (S.TextReferent :. Only (Maybe NamedRef.ConstructorType))] <-
    queryListRow
      [sql|
        SELECT reversed_name, referent_builtin, referent_component_hash, referent_component_index, referent_constructor_index, referent_constructor_type
        FROM scoped_term_name_lookup
        WHERE
          root_branch_hash_id = :bhId
          AND namespace GLOB :namespaceGlob

        UNION ALL

        SELECT (names.reversed_name || mount.reversed_mount_path) AS reversed_name, referent_builtin, referent_component_hash, referent_component_index, referent_constructor_index, referent_constructor_type
        FROM name_lookup_mounts mount
          INNER JOIN scoped_term_name_lookup names ON names.root_branch_hash_id = mount.mounted_root_branch_hash_id
        WHERE
          mount.parent_root_branch_hash_id = :bhId
          -- We have a pre-condition that the namespace must not be within any of the mounts,
          -- so this is sufficient to determine whether the entire sub-index is within the
          -- required namespace prefix.
          AND mount.mount_path GLOB :namespaceGlob
      |]
  pure (fmap unRow <$> results)
  where
    namespaceGlob = toNamespaceGlob namespace
    unRow (a :. Only b) = (a, b)

-- | NOTE: requires that the codebase has an up-to-date name lookup index. As of writing, this
-- is only true on Share.
--
-- Get the list of a type names in the provided name lookup and relative namespace.
-- Includes dependencies, but not transitive dependencies.
typeNamesWithinNamespace :: BranchHashId -> PathSegments -> Transaction [NamedRef S.TextReference]
typeNamesWithinNamespace bhId namespace =
  queryListRow
    [sql|
      SELECT reversed_name, reference_builtin, reference_component_hash, reference_component_index
      FROM scoped_type_name_lookup
      WHERE
        root_branch_hash_id = :bhId
        AND namespace GLOB :namespaceGlob

      UNION ALL

      SELECT (names.reversed_name || mount.reversed_mount_path) AS reversed_name, reference_builtin, reference_component_hash, reference_component_index
      FROM name_lookup_mounts mount
        INNER JOIN scoped_type_name_lookup names ON names.root_branch_hash_id = mount.mounted_root_branch_hash_id
      WHERE
        mount.parent_root_branch_hash_id = :bhId
        -- We have a pre-condition that the namespace must not be within any of the mounts,
        -- so this is sufficient to determine whether the entire sub-index is within the
        -- required namespace prefix.
        AND mount.mount_path GLOB :namespaceGlob
    |]
  where
    namespaceGlob = toNamespaceGlob namespace

-- | NOTE: requires that the codebase has an up-to-date name lookup index. As of writing, this
-- is only true on Share.
--
-- Get the list of term names within a given namespace which have the given suffix.
termNamesBySuffix :: BranchHashId -> PathSegments -> ReversedName -> Transaction [NamedRef (S.TextReferent, Maybe NamedRef.ConstructorType)]
termNamesBySuffix bhId namespaceRoot suffix = do
  Debug.debugM Debug.Server "termNamesBySuffix" (namespaceRoot, suffix)
  let namespaceGlob = toNamespaceGlob namespaceRoot
  let lastSegment = NonEmpty.head . into @(NonEmpty Text) $ suffix
  let reversedNameGlob = toSuffixGlob suffix
  results :: [NamedRef (S.TextReferent :. Only (Maybe NamedRef.ConstructorType))] <-
    -- Note: It may seem strange that we do a last_name_segment constraint AND a reversed_name
    -- GLOB, but this helps improve query performance.
    -- The SQLite query optimizer is smart enough to do a prefix-search on globs, but will
    -- ONLY do a single prefix-search, meaning we use the index for `namespace`, but not for
    -- `reversed_name`. By adding the `last_name_segment` constraint, we can cull a ton of
    -- names which couldn't possibly match before we then manually filter the remaining names
    -- using the `reversed_name` glob which can't be optimized with an index.
    queryListRow
      [sql|
        SELECT reversed_name, referent_builtin, referent_component_hash, referent_component_index, referent_constructor_index, referent_constructor_type
        FROM scoped_term_name_lookup
        WHERE root_branch_hash_id = :bhId
              AND last_name_segment IS :lastSegment
              AND namespace GLOB :namespaceGlob
              AND reversed_name GLOB :reversedNameGlob
        UNION ALL
        SELECT (names.reversed_name || mount.reversed_mount_path) AS reversed_name, referent_builtin, referent_component_hash, referent_component_index, referent_constructor_index, referent_constructor_type
        FROM name_lookup_mounts mount
          INNER JOIN scoped_term_name_lookup names ON names.root_branch_hash_id = mount.mounted_root_branch_hash_id
        WHERE mount.parent_root_branch_hash_id = :bhId
              AND mount.mount_path GLOB :namespaceGlob
              AND last_name_segment IS :lastSegment
              AND reversed_name GLOB :reversedNameGlob
      |]
  pure (fmap unRow <$> results)
  where
    unRow (a :. Only b) = (a, b)

-- | NOTE: requires that the codebase has an up-to-date name lookup index. As of writing, this
-- is only true on Share.
--
-- Get the list of type names within a given namespace which have the given suffix.
typeNamesBySuffix :: BranchHashId -> PathSegments -> ReversedName -> Transaction [NamedRef S.TextReference]
typeNamesBySuffix bhId namespaceRoot suffix = do
  Debug.debugM Debug.Server "typeNamesBySuffix" (namespaceRoot, suffix)
  let namespaceGlob = toNamespaceGlob namespaceRoot
  let lastNameSegment = NonEmpty.head . into @(NonEmpty Text) $ suffix
  let reversedNameGlob = toSuffixGlob suffix
  -- Note: It may seem strange that we do a last_name_segment constraint AND a reversed_name
  -- GLOB, but this helps improve query performance.
  -- The SQLite query optimizer is smart enough to do a prefix-search on globs, but will
  -- ONLY do a single prefix-search, meaning we use the index for `namespace`, but not for
  -- `reversed_name`. By adding the `last_name_segment` constraint, we can cull a ton of
  -- names which couldn't possibly match before we then manually filter the remaining names
  -- using the `reversed_name` glob which can't be optimized with an index.
  queryListRow
    [sql|
      SELECT reversed_name, reference_builtin, reference_component_hash, reference_component_index
      FROM scoped_type_name_lookup
      WHERE     root_branch_hash_id = :bhId
            AND last_name_segment IS :lastNameSegment
            AND namespace GLOB :namespaceGlob
            AND reversed_name GLOB :reversedNameGlob
      UNION ALL
      SELECT (names.reversed_name || mount.reversed_mount_path) AS reversed_name, reference_builtin, reference_component_hash, reference_component_index
      FROM name_lookup_mounts mount
        INNER JOIN scoped_type_name_lookup names ON names.root_branch_hash_id = mount.mounted_root_branch_hash_id
      WHERE mount.parent_root_branch_hash_id = :bhId
            AND mount.mount_path GLOB :namespaceGlob
            AND last_name_segment IS :lastNameSegment
            AND reversed_name GLOB :reversedNameGlob
    |]

-- | NOTE: requires that the codebase has an up-to-date name lookup index. As of writing, this
-- is only true on Share.
--
-- Get the set of refs for an exact name.
-- This will only return results which are within the name lookup for the provided branch hash
-- id. It's the caller's job to select the correct name lookup for your exact name.
--
-- See termRefsForExactName in U.Codebase.Sqlite.Operations
termRefsForExactName :: BranchHashId -> ReversedName -> Transaction [NamedRef (S.TextReferent, Maybe NamedRef.ConstructorType)]
termRefsForExactName bhId reversedSegments = do
  let reversedName = toReversedName reversedSegments
  results :: [NamedRef (S.TextReferent :. Only (Maybe NamedRef.ConstructorType))] <-
    queryListRow
      [sql|
        SELECT reversed_name, referent_builtin, referent_component_hash, referent_component_index, referent_constructor_index, referent_constructor_type
        FROM scoped_term_name_lookup
        WHERE root_branch_hash_id = :bhId
              AND reversed_name = :reversedName
      |]
  pure (fmap unRow <$> results)
  where
    unRow (a :. Only b) = (a, b)

-- | NOTE: requires that the codebase has an up-to-date name lookup index. As of writing, this
-- is only true on Share.
--
-- Get the set of refs for an exact name.
-- This will only return results which are within the name lookup for the provided branch hash
-- id. It's the caller's job to select the correct name lookup for your exact name.
--
-- See termRefsForExactName in U.Codebase.Sqlite.Operations
typeRefsForExactName :: BranchHashId -> ReversedName -> Transaction [NamedRef S.TextReference]
typeRefsForExactName bhId reversedSegments = do
  let reversedName = toReversedName reversedSegments
  queryListRow
    [sql|
      SELECT reversed_name, reference_builtin, reference_component_hash, reference_component_index
      FROM scoped_type_name_lookup
      WHERE root_branch_hash_id = :bhId
            AND reversed_name = :reversedName
    |]

-- | NOTE: requires that the codebase has an up-to-date name lookup index. As of writing, this
-- is only true on Share.
--
-- Get the list of term names for a given Referent within a given namespace.
-- Considers one level of dependencies, but not transitive dependencies.
termNamesForRefWithinNamespace :: BranchHashId -> PathSegments -> S.TextReferent -> Maybe ReversedName -> Transaction [ReversedName]
termNamesForRefWithinNamespace bhId namespaceRoot ref maySuffix = do
  let namespaceGlob = toNamespaceGlob namespaceRoot
  let suffixGlob = case maySuffix of
        Just suffix -> toSuffixGlob suffix
        Nothing -> "*"
  directNames <- queryListColCheck
    [sql|
        SELECT reversed_name FROM scoped_term_name_lookup
        WHERE root_branch_hash_id = :bhId
              AND referent_builtin IS @ref AND referent_component_hash IS @ AND referent_component_index IS @ AND referent_constructor_index IS @
              AND namespace GLOB :namespaceGlob
              AND reversed_name GLOB :suffixGlob
        UNION ALL
        SELECT (names.reversed_name || mount.reversed_mount_path) AS reversed_name
        FROM name_lookup_mounts mount
          INNER JOIN scoped_term_name_lookup names ON names.root_branch_hash_id = mount.mounted_root_branch_hash_id
        WHERE mount.parent_root_branch_hash_id = :bhId
              AND mount.mount_path GLOB :namespaceGlob
              AND referent_builtin IS @ref AND referent_component_hash IS @ AND referent_component_index IS @ AND referent_constructor_index IS @
              AND reversed_name GLOB :suffixGlob
        |]
    \reversedNames -> for reversedNames reversedNameToReversedSegments
  -- If we don't find a name in the name lookup, expand the search to recursively include transitive deps
  -- and just return the first one we find.
  if null directNames
    then do
      toList
        <$> queryMaybeColCheck
          [sql|
        $transitive_dependency_mounts
        SELECT (reversed_name || reversed_mount_path) AS reversed_name
          FROM transitive_dependency_mounts
            INNER JOIN scoped_term_name_lookup
            ON scoped_term_name_lookup.root_branch_hash_id = transitive_dependency_mounts.root_branch_hash_id
        WHERE referent_builtin IS @ref AND referent_component_hash IS @ AND referent_component_index IS @ AND referent_constructor_index IS @
              AND reversed_name GLOB :suffixGlob
        LIMIT 1
      |]
          (\reversedName -> reversedNameToReversedSegments reversedName)
    else pure directNames
  where
    transitive_dependency_mounts = transitiveDependenciesSql bhId

-- | NOTE: requires that the codebase has an up-to-date name lookup index. As of writing, this
-- is only true on Share.
--
-- Get the list of type names for a given Reference within a given namespace.
-- Considers one level of dependencies, but not transitive dependencies.
typeNamesForRefWithinNamespace :: BranchHashId -> PathSegments -> S.TextReference -> Maybe ReversedName -> Transaction [ReversedName]
typeNamesForRefWithinNamespace bhId namespaceRoot ref maySuffix = do
  let namespaceGlob = toNamespaceGlob namespaceRoot
  let suffixGlob = case maySuffix of
        Just suffix -> toSuffixGlob suffix
        Nothing -> "*"
  directNames <- queryListColCheck
    [sql|
        SELECT reversed_name FROM scoped_type_name_lookup
        WHERE root_branch_hash_id = :bhId
              AND reference_builtin IS @ref AND reference_component_hash IS @ AND reference_component_index IS @
              AND namespace GLOB :namespaceGlob
              AND reversed_name GLOB :suffixGlob
        UNION ALL
        SELECT (names.reversed_name || mount.reversed_mount_path) AS reversed_name
        FROM name_lookup_mounts mount
          INNER JOIN scoped_type_name_lookup names ON names.root_branch_hash_id = mount.mounted_root_branch_hash_id
        WHERE mount.parent_root_branch_hash_id = :bhId
              AND mount.mount_path GLOB :namespaceGlob
              AND reference_builtin IS @ref AND reference_component_hash IS @ AND reference_component_index IS @
              AND reversed_name GLOB :suffixGlob
        |]
    \reversedNames -> for reversedNames reversedNameToReversedSegments
  -- If we don't find a name in the name lookup, expand the search to recursively include transitive deps
  -- and just return the first one we find.
  if null directNames
    then
      toList
        <$> queryMaybeColCheck
          [sql|
        $transitive_dependency_mounts
        SELECT (reversed_name || reversed_mount_path) AS reversed_name
          FROM transitive_dependency_mounts
            INNER JOIN scoped_type_name_lookup
            ON scoped_type_name_lookup.root_branch_hash_id = transitive_dependency_mounts.root_branch_hash_id
        WHERE reference_builtin IS @ref AND reference_component_hash IS @ AND reference_component_index IS @
              AND reversed_name GLOB :suffixGlob
        LIMIT 1
          |]
          (\reversedName -> reversedNameToReversedSegments reversedName)
    else pure directNames
  where
    transitive_dependency_mounts = transitiveDependenciesSql bhId

-- | Brings into scope the transitive_dependency_mounts CTE table, which contains all transitive deps of the given root, but does NOT include the direct dependencies.
-- @transitive_dependency_mounts(root_branch_hash_id, reversed_mount_path)@
-- Where @reversed_mount_path@ is the reversed path from the provided root to the mounted
-- dependency's root.
transitiveDependenciesSql :: BranchHashId -> Sql
transitiveDependenciesSql rootBranchHashId =
  [sql|
        -- Recursive table containing all transitive deps
        WITH RECURSIVE
          transitive_dependency_mounts(root_branch_hash_id, reversed_mount_path) AS (
            -- We've already searched direct deps above, so start with children of direct deps
            SELECT transitive.mounted_root_branch_hash_id, transitive.reversed_mount_path || direct.reversed_mount_path
            FROM name_lookup_mounts direct
                 JOIN name_lookup_mounts transitive on direct.mounted_root_branch_hash_id = transitive.parent_root_branch_hash_id
            WHERE direct.parent_root_branch_hash_id = :rootBranchHashId
            UNION ALL
            SELECT mount.mounted_root_branch_hash_id, mount.reversed_mount_path || rec.reversed_mount_path
            FROM name_lookup_mounts mount
              INNER JOIN transitive_dependency_mounts rec ON mount.parent_root_branch_hash_id = rec.root_branch_hash_id
          )
          |]

-- | NOTE: requires that the codebase has an up-to-date name lookup index. As of writing, this
-- is only true on Share.
--
-- Searches all dependencies transitively looking for the provided referent.
-- Prefer 'termNamesForRefWithinNamespace' in most cases.
-- This is slower and only necessary when resolving the name of references when you don't know which
-- dependency it may exist in.
--
-- Searching transitive dependencies is exponential so we want to replace this with a more
-- efficient approach as soon as possible.
--
-- Note: this returns the first name it finds by searching in order of:
-- Names in the current namespace, then names in the current namespace's dependencies, then
-- through the current namespace's dependencies' dependencies, etc.
recursiveTermNameSearch :: BranchHashId -> S.TextReferent -> Transaction (Maybe ReversedName)
recursiveTermNameSearch bhId ref = do
  queryMaybeColCheck
    [sql|
        -- Recursive table containing all transitive deps
        WITH RECURSIVE
          all_in_scope_roots(root_branch_hash_id, reversed_mount_path) AS (
            -- Include the primary root
            SELECT :bhId, ""
            UNION ALL
            SELECT mount.mounted_root_branch_hash_id, mount.reversed_mount_path || rec.reversed_mount_path
            FROM name_lookup_mounts mount
              INNER JOIN all_in_scope_roots rec ON mount.parent_root_branch_hash_id = rec.root_branch_hash_id
          )
        SELECT (reversed_name || reversed_mount_path) AS reversed_name
          FROM all_in_scope_roots
            INNER JOIN scoped_term_name_lookup
            ON scoped_term_name_lookup.root_branch_hash_id = all_in_scope_roots.root_branch_hash_id
        WHERE referent_builtin IS @ref AND referent_component_hash IS @ AND referent_component_index IS @ AND referent_constructor_index IS @
        LIMIT 1
        |]
    (\reversedName -> reversedNameToReversedSegments reversedName)

-- | NOTE: requires that the codebase has an up-to-date name lookup index. As of writing, this
-- is only true on Share.
--
-- Searches all dependencies transitively looking for the provided referent.
-- Prefer 'typeNamesForRefWithinNamespace' in most cases.
-- This is slower and only necessary when resolving the name of references when you don't know which
-- dependency it may exist in.
--
-- Searching transitive dependencies is exponential so we want to replace this with a more
-- efficient approach as soon as possible.
--
-- Note: this returns the first name it finds by searching in order of:
-- Names in the current namespace, then names in the current namespace's dependencies, then
-- through the current namespace's dependencies' dependencies, etc.
recursiveTypeNameSearch :: BranchHashId -> S.TextReference -> Transaction (Maybe ReversedName)
recursiveTypeNameSearch bhId ref = do
  queryMaybeColCheck
    [sql|
        -- Recursive table containing all transitive deps
        WITH RECURSIVE
          all_in_scope_roots(root_branch_hash_id, reversed_mount_path) AS (
            -- Include the primary root
            SELECT :bhId, ""
            UNION ALL
            SELECT mount.mounted_root_branch_hash_id, mount.reversed_mount_path || rec.reversed_mount_path
            FROM name_lookup_mounts mount
              INNER JOIN all_in_scope_roots rec ON mount.parent_root_branch_hash_id = rec.root_branch_hash_id
          )
        SELECT (reversed_name || reversed_mount_path) AS reversed_name
          FROM all_in_scope_roots
            INNER JOIN scoped_type_name_lookup
            ON scoped_type_name_lookup.root_branch_hash_id = all_in_scope_roots.root_branch_hash_id
        WHERE reference_builtin IS @ref AND reference_component_hash IS @ AND reference_component_index IS @
        LIMIT 1
        |]
    (\reversedName -> reversedNameToReversedSegments reversedName)

-- | NOTE: requires that the codebase has an up-to-date name lookup index. As of writing, this
-- is only true on Share.
--
-- The goal of this query is to search the codebase for the single name which has a different
-- hash from the provided name, but shares longest matching suffix for for that name.
--
-- Including this name in the pretty-printer object causes it to suffixify the name so that it
-- is unambiguous from other names in scope.
--
-- Sqlite doesn't provide enough functionality to do this query in a single query, so we do
-- it iteratively, querying for longer and longer suffixes we no longer find matches.
-- Then we return the name with longest matching suffix.
--
-- This is still relatively efficient because we can use an index and LIMIT 1 to make each
-- individual query fast, and in the common case we'll only need two or three queries to find
-- the longest matching suffix.
--
-- Considers one level of dependencies, but not transitive dependencies.
longestMatchingTermNameForSuffixification :: BranchHashId -> PathSegments -> NamedRef S.TextReferent -> Transaction (Maybe (NamedRef (S.TextReferent, Maybe NamedRef.ConstructorType)))
longestMatchingTermNameForSuffixification bhId namespaceRoot (NamedRef.NamedRef {reversedSegments = revSuffix@(ReversedName (lastSegment NonEmpty.:| _)), ref}) = do
  let namespaceGlob = toNamespaceGlob namespaceRoot <> ".*"
  let loop :: [Text] -> MaybeT Transaction (NamedRef (S.TextReferent, Maybe NamedRef.ConstructorType))
      loop [] = empty
      loop (suffGlob : rest) = do
        result :: Maybe (NamedRef (S.TextReferent :. Only (Maybe NamedRef.ConstructorType))) <-
          lift $
            queryMaybeRow
              -- Note: It may seem strange that we do a last_name_segment constraint AND a reversed_name
              -- GLOB, but this helps improve query performance.
              -- The SQLite query optimizer is smart enough to do a prefix-search on globs, but will
              -- ONLY do a single prefix-search, meaning we use the index for `namespace`, but not for
              -- `reversed_name`. By adding the `last_name_segment` constraint, we can cull a ton of
              -- names which couldn't possibly match before we then manually filter the remaining names
              -- using the `reversed_name` glob which can't be optimized with an index.
              [sql|
              SELECT reversed_name, referent_builtin, referent_component_hash, referent_component_index, referent_constructor_index, referent_constructor_type FROM scoped_term_name_lookup
              WHERE root_branch_hash_id = :bhId
                    AND last_name_segment IS :lastSegment
                    AND namespace GLOB :namespaceGlob
                    AND reversed_name GLOB :suffGlob
                    -- We don't need to consider names for the same definition when suffixifying, so
                    -- we filter those out. Importantly this also avoids matching the name we're trying to suffixify.
                    AND NOT (referent_builtin IS @ref AND referent_component_hash IS @ AND referent_component_index IS @ AND referent_constructor_index IS @)
              UNION ALL
              SELECT (names.reversed_name || mount.reversed_mount_path) AS reversed_name, names.referent_builtin, names.referent_component_hash, names.referent_component_index, names.referent_constructor_index, names.referent_constructor_type
              FROM name_lookup_mounts mount
                INNER JOIN scoped_term_name_lookup names ON names.root_branch_hash_id = mount.mounted_root_branch_hash_id
              WHERE mount.parent_root_branch_hash_id = :bhId
                    AND mount.mount_path GLOB :namespaceGlob
                    AND last_name_segment IS :lastSegment
                    AND reversed_name GLOB :suffGlob
                    -- We don't need to consider names for the same definition when suffixifying, so
                    -- we filter those out. Importantly this also avoids matching the name we're trying to suffixify.
                    AND NOT (names.referent_builtin IS @ref AND names.referent_component_hash IS @ AND names.referent_component_index IS @ AND names.referent_constructor_index IS @)
              LIMIT 1
            |]
        case result of
          Just namedRef ->
            -- We want to find matches for the _longest_ possible suffix, so we keep going until we
            -- don't find any more matches.
            pure (unRow <$> namedRef) <|> loop rest
          Nothing ->
            -- If we don't find a match for a suffix, there's no way we could match on an even
            -- longer suffix, so we bail.
            empty
  let suffixes =
        revSuffix
          & into @[Text]
          & List.inits
          & mapMaybe NonEmpty.nonEmpty
          & map (toSuffixGlob . into @ReversedName)
  runMaybeT $ loop suffixes
  where
    unRow (a :. Only b) = (a, b)

-- | NOTE: requires that the codebase has an up-to-date name lookup index. As of writing, this
-- is only true on Share.
--
-- The goal of this query is to search the codebase for the single name which has a different
-- hash from the provided name, but shares longest matching suffix for for that name.
--
-- Including this name in the pretty-printer object causes it to suffixify the name so that it
-- is unambiguous from other names in scope.
--
-- Sqlite doesn't provide enough functionality to do this query in a single query, so we do
-- it iteratively, querying for longer and longer suffixes we no longer find matches.
-- Then we return the name with longest matching suffix.
--
-- This is still relatively efficient because we can use an index and LIMIT 1 to make each
-- individual query fast, and in the common case we'll only need two or three queries to find
-- the longest matching suffix.
--
-- Considers one level of dependencies, but not transitive dependencies.
longestMatchingTypeNameForSuffixification :: BranchHashId -> PathSegments -> NamedRef S.TextReference -> Transaction (Maybe (NamedRef S.TextReference))
longestMatchingTypeNameForSuffixification bhId namespaceRoot (NamedRef.NamedRef {reversedSegments = revSuffix@(ReversedName (lastSegment NonEmpty.:| _)), ref}) = do
  let namespaceGlob = toNamespaceGlob namespaceRoot <> ".*"
  let loop :: [Text] -> MaybeT Transaction (NamedRef S.TextReference)
      loop [] = empty
      loop (suffGlob : rest) = do
        result :: Maybe (NamedRef (S.TextReference)) <-
          lift $
            queryMaybeRow
              -- Note: It may seem strange that we do a last_name_segment constraint AND a reversed_name
              -- GLOB, but this helps improve query performance.
              -- The SQLite query optimizer is smart enough to do a prefix-search on globs, but will
              -- ONLY do a single prefix-search, meaning we use the index for `namespace`, but not for
              -- `reversed_name`. By adding the `last_name_segment` constraint, we can cull a ton of
              -- names which couldn't possibly match before we then manually filter the remaining names
              -- using the `reversed_name` glob which can't be optimized with an index.
              [sql|
              SELECT reversed_name, reference_builtin, reference_component_hash, reference_component_index FROM scoped_type_name_lookup
              WHERE root_branch_hash_id = :bhId
                    AND last_name_segment IS :lastSegment
                    AND namespace GLOB :namespaceGlob
                    AND reversed_name GLOB :suffGlob
                    -- We don't need to consider names for the same definition when suffixifying, so
                    -- we filter those out. Importantly this also avoids matching the name we're trying to suffixify.
                    AND NOT (reference_builtin IS @ref AND reference_component_hash IS @ AND reference_component_index IS @)
              UNION ALL
              SELECT (names.reversed_name || mount.reversed_mount_path) AS reversed_name, names.reference_builtin, names.reference_component_hash, names.reference_component_index
              FROM name_lookup_mounts mount
                INNER JOIN scoped_type_name_lookup names ON names.root_branch_hash_id = mount.mounted_root_branch_hash_id
              WHERE mount.parent_root_branch_hash_id = :bhId
                    AND mount.mount_path GLOB :namespaceGlob
                    AND last_name_segment IS :lastSegment
                    AND reversed_name GLOB :suffGlob
                    -- We don't need to consider names for the same definition when suffixifying, so
                    -- we filter those out. Importantly this also avoids matching the name we're trying to suffixify.
                    AND NOT (names.reference_builtin IS @ref AND names.reference_component_hash IS @ AND names.reference_component_index IS @)
              LIMIT 1
            |]
        case result of
          Just namedRef ->
            -- We want to find matches for the _longest_ possible suffix, so we keep going until we
            -- don't find any more matches.
            pure namedRef <|> loop rest
          Nothing ->
            -- If we don't find a match for a suffix, there's no way we could match on an even
            -- longer suffix, so we bail.
            empty
  let suffixes =
        revSuffix
          & into @[Text]
          & List.inits
          & mapMaybe NonEmpty.nonEmpty
          & map (toSuffixGlob . into @ReversedName)
  runMaybeT $ loop suffixes

-- | Associate name lookup indexes for dependencies to specific mounting points within another name lookup.
associateNameLookupMounts :: BranchHashId -> [(PathSegments, BranchHashId)] -> Transaction ()
associateNameLookupMounts rootBranchHashId mounts = do
  for_ mounts \(mountPath, mountedBranchHashId) -> do
    let mountPathText = pathSegmentsToText mountPath <> "."
        reversedMountPathText = pathSegmentsToText (PathSegments . reverse . coerce $ mountPath) <> "."

    execute
      [sql|
          INSERT INTO name_lookup_mounts (parent_root_branch_hash_id, mounted_root_branch_hash_id, mount_path, reversed_mount_path)
          VALUES (:rootBranchHashId, :mountedBranchHashId, :mountPathText, :reversedMountPathText)
        |]

-- | Fetch the name lookup mounts for a given name lookup index.
listNameLookupMounts :: BranchHashId -> Transaction [(PathSegments, BranchHashId)]
listNameLookupMounts rootBranchHashId =
  do
    queryListRow
      [sql|
        SELECT mount_path, mounted_root_branch_hash_id
        FROM name_lookup_mounts
        WHERE parent_root_branch_hash_id = :rootBranchHashId
      |]
    <&> fmap
      \(mountPathText, mountedRootBranchHashId) ->
        let mountPath = textToPathSegments (Text.init mountPathText)
         in (mountPath, mountedRootBranchHashId)

-- | Searches for all names within the given name lookup which contain the provided list of segments
-- in order.
-- Search is case insensitive.
fuzzySearchTerms :: Bool -> BranchHashId -> Int -> PathSegments -> [Text] -> Transaction [(NamedRef (S.TextReferent, Maybe NamedRef.ConstructorType))]
fuzzySearchTerms includeDependencies bhId limit namespace querySegments = do
  -- Union in the dependencies if required.
  let dependenciesSql =
        if includeDependencies
          then
            [sql|
      UNION ALL
        SELECT (names.reversed_name || mount.reversed_mount_path) AS reversed_name, referent_builtin, referent_component_hash, referent_component_index, referent_constructor_index, referent_constructor_type
        FROM name_lookup_mounts mount
          INNER JOIN scoped_term_name_lookup names ON names.root_branch_hash_id = mount.mounted_root_branch_hash_id
        WHERE
          mount.parent_root_branch_hash_id = :bhId
          -- We have a pre-condition that the namespace must not be within any of the mounts,
          -- so this is sufficient to determine whether the entire sub-index is within the
          -- required namespace prefix.
          AND mount.mount_path GLOB :namespaceGlob
          AND (mount.mount_path || namespace || last_name_segment) LIKE :preparedQuery ESCAPE '\'
          |]
          else [sql||]
  fmap unRow
    <$> queryListRow
      [sql|
      SELECT reversed_name, referent_builtin, referent_component_hash, referent_component_index, referent_constructor_index, referent_constructor_type
        FROM scoped_term_name_lookup
      WHERE
        root_branch_hash_id = :bhId
        AND namespace GLOB :namespaceGlob
        AND (namespace || last_name_segment) LIKE :preparedQuery ESCAPE '\'
      $dependenciesSql
        LIMIT :limit
    |]
  where
    namespaceGlob = toNamespaceGlob namespace
    preparedQuery = prepareFuzzyQuery '\\' querySegments
    unRow :: NamedRef (S.TextReferent :. Only (Maybe NamedRef.ConstructorType)) -> NamedRef (S.TextReferent, Maybe NamedRef.ConstructorType)
    unRow = fmap \(a :. Only b) -> (a, b)

-- | Searches for all names within the given name lookup which contain the provided list of segments
-- in order.
--
-- Search is case insensitive.
fuzzySearchTypes :: Bool -> BranchHashId -> Int -> PathSegments -> [Text] -> Transaction [(NamedRef S.TextReference)]
fuzzySearchTypes includeDependencies bhId limit namespace querySegments = do
  -- Union in the dependencies if required.
  let dependenciesSql =
        if includeDependencies
          then
            [sql|
      UNION ALL
        SELECT (names.reversed_name || mount.reversed_mount_path) AS reversed_name, reference_builtin, reference_component_hash, reference_component_index
        FROM name_lookup_mounts mount
          INNER JOIN scoped_type_name_lookup names ON names.root_branch_hash_id = mount.mounted_root_branch_hash_id
        WHERE
          mount.parent_root_branch_hash_id = :bhId
          -- We have a pre-condition that the namespace must not be within any of the mounts,
          -- so this is sufficient to determine whether the entire sub-index is within the
          -- required namespace prefix.
          AND mount.mount_path GLOB :namespaceGlob
          AND (mount.mount_path || namespace || last_name_segment) LIKE :preparedQuery ESCAPE '\'
          |]
          else [sql||]
  queryListRow
    [sql|
      SELECT reversed_name, reference_builtin, reference_component_hash, reference_component_index
        FROM scoped_type_name_lookup
      WHERE
        root_branch_hash_id = :bhId
        AND namespace GLOB :namespaceGlob
        AND (namespace || last_name_segment) LIKE :preparedQuery ESCAPE '\'

      $dependenciesSql

        LIMIT :limit
    |]
  where
    namespaceGlob = toNamespaceGlob namespace
    preparedQuery = prepareFuzzyQuery '\\' querySegments

-- | >>> prepareFuzzyQuery ["foo", "bar"]
-- "%foo%bar%"
--
-- >>> prepareFuzzyQuery ["foo", "", "bar"]
-- "%foo%bar%"
--
-- >>> prepareFuzzyQuery ["foo%", "bar "]
-- "%foo\\%%bar%"
prepareFuzzyQuery :: Char -> [Text] -> Text
prepareFuzzyQuery escapeChar query =
  query
    & filter (not . Text.null)
    & map (likeEscape escapeChar . Text.strip)
    & \q -> "%" <> Text.intercalate "%" q <> "%"

-- fuzzySearchTypes :: Text -> Transaction [NamedRef Reference.TextReference]

-- | We need to escape any special characters for globbing.
--
-- >>> globEscape "Nat.*.doc"
-- "Nat.[*].doc"
globEscape :: Text -> Text
globEscape =
  -- We can't use Text.replace, since we'd end up replacing either "[" or "]" multiple
  -- times.
  Text.concatMap \case
    '*' -> "[*]"
    '?' -> "[?]"
    '[' -> "[[]"
    ']' -> "[]]"
    c -> Text.singleton c

-- | Convert reversed name segments into glob for searching based on suffix
--
-- >>> toSuffixGlob ("foo" NonEmpty.:| ["bar"])
-- "foo.bar.*"
toSuffixGlob :: ReversedName -> Text
toSuffixGlob suffix = globEscape (Text.intercalate "." (into @[Text] suffix)) <> ".*"

-- | Convert reversed segments into the DB representation of a reversed_name.
--
-- >>> toReversedName (NonEmpty.fromList ["foo", "bar"])
-- "foo.bar."
toReversedName :: ReversedName -> Text
toReversedName revSegs = Text.intercalate "." (into @[Text] revSegs) <> "."

-- | Convert a namespace into the appropriate glob for searching within that namespace
--
-- >>> toNamespaceGlob "foo.bar"
-- "foo.bar.*"
--
-- >>> toNamespaceGlob ""
-- "*"
toNamespaceGlob :: PathSegments -> Text
toNamespaceGlob = \case
  PathSegments [] -> "*"
  namespace -> globEscape (pathSegmentsToText namespace) <> ".*"

-- | Thrown if we try to get the segments of an empty name, shouldn't ever happen since empty names
-- are invalid.
data EmptyName = EmptyName String
  deriving stock (Eq, Show)
  deriving anyclass (SqliteExceptionReason)

-- | Convert a reversed name into reversed segments.
--
-- >>> reversedNameToReversedSegments "foo.bar."
-- Right ("foo" :| ["bar"])
reversedNameToReversedSegments :: (HasCallStack) => Text -> Either EmptyName ReversedName
reversedNameToReversedSegments txt =
  txt
    & Text.splitOn "."
    -- Names have a trailing dot, so we need to drop the last empty segment
    & List.dropEnd1
    & NonEmpty.nonEmpty
    & maybe (Left (EmptyName $ show callStack)) (Right . into @ReversedName)
