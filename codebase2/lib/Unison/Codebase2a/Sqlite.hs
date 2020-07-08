{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Unison.Codebase2a.Sqlite where

import Unison.Prelude
import Data.String.Here
import Database.SQLite.Simple

create :: Connection -> IO ()
create conn = liftIO do
  execute_ conn [hereFile|sql/create.sql|]
  execute_ conn [hereFile|sql/create-index.sql|]

-- -- It looks like we need to refactor Reference, etc.
-- -- as in `topic/hash-parameterized-references`,
-- -- to take a type parameter in place of `Hash`.
-- --
-- -- But how many different kinds of type parameters do we need?
-- -- I think we should avoid leaking too many.  Maybe the one is fine.
--
-- getObjectsByHashPrefix ::
--   Connection -> Base32Prefix -> m [(Base32, Base32, ObjectType, ByteString)]
-- getObjectsByHashPrefix conn prefix = query conn sql [prefix]
--   where
--   sql = [here|
--     SELECT query.base32, primary.base32, object.type_id, object.bytes
--     FROM object
--     INNER JOIN hash AS primary ON object.primary_hash_id = primary.id
--     INNER JOIN hash AS query ON hash_object.hash_id = query.id
--     INNER JOIN hash_object ON hash_object.object_id = object.id
--     WHERE query.base32 LIKE ?
--   |]
--
-- getTransitiveDependents ::
--   Connection -> Reference -> m [Reference]
-- getTransitiveDependents conn prefix = query conn sql [prefix]
--   where
--   sql = [here|
--     WITH RECURSIVE transitive_dependents(id)
--     AS (VALUES(?) UNION ALL SELECT dependent_id
--         FROM transitive_dependents
--           INNER JOIN dependents_index ON dependency_id = id)
--     SELECT id from transitive_dependents
--   |]
--
-- getTransitiveDependencies ::
--   Connection -> Reference -> m [Reference]
-- getTransitiveDependencies conn prefix = query conn sql prefix
--   where
--   sql = [here|
--     WITH RECURSIVE transitive_dependencies(id)
--     AS (VALUES(?) UNION ALL SELECT dependency_id
--         FROM transitive_dependents
--           INNER JOIN dependents_index ON dependent_id = id)
--     SELECT id from transitive_dependencies
--   |]

-- todo: where does type of term go?
  -- option 1: prepended to term?
  -- option 2: type-of-term table?
  -- note: they are included in the data decl, why not included in the term?

-- getTerm            :: Reference.Id -> m (Maybe (Term v a)),
-- getTerm =
-- getTypeOfTermImpl  :: Reference.Id -> m (Maybe (Type v a)),
-- getTypeDeclaration :: Reference.Id -> m (Maybe (Decl v a)),
--
-- putTerm            :: Reference.Id -> Term v a -> Type v a -> m (),
-- putTypeDeclaration :: Reference.Id -> Decl v a -> m (),
--
-- getBranch           :: Branch.Hash -> m (Maybe (Branch m)),
-- getRootBranch      :: m (Either GetRootBranchError (Branch m)),
-- putRootBranch      :: Branch m -> m (),
--
-- rootBranchUpdates  :: m (m (), m (Set Branch.Hash)),
-- getBranchForCausal :: Branch.CausalHash -> m (Maybe (Branch m)),
--
-- -- |Supports syncing from a current or older codebase format
-- syncFromDirectory  :: CodebasePath -> SyncMode -> Branch m -> m (),
-- -- |Only writes the latest codebase format
-- syncToDirectory    :: CodebasePath -> SyncMode -> Branch m -> m (),
-- -- ^ maybe return type needs to reflect failure if remote codebase has an old version
--
-- -- |Watch expressions are part of the codebase, the `Reference.Id` is
-- -- the hash of the source of the watch expression, and the `Term v a`
-- -- is the evaluated result of the expression, decompiled to a term.
-- watches  :: UF.WatchKind -> m [Reference.Id],
-- getWatch :: UF.WatchKind -> Reference.Id -> m (Maybe (Term v a)),
-- putWatch :: UF.WatchKind -> Reference.Id -> Term v a -> m (),
--
-- getReflog    :: m [Reflog.Entry],
-- appendReflog :: Text -> Branch m -> Branch m -> m (),
--
-- -- |the nicely-named versions will utilize these, and add builtins to the result set
-- termsHavingType_impl     :: Reference -> m (Set Referent.Id),
-- termsMentioningType_impl :: Reference -> m (Set Referent.Id),
--
-- -- |number of base58 characters needed to distinguish any two hashes in the codebase;
-- -- we don't have to compute it separately for different namespaces
-- hashLength             :: m Int,
-- termReferencesByPrefix :: ShortHash -> m (Set Reference.Id),
-- typeReferencesByPrefix :: ShortHash -> m (Set Reference.Id),
-- termReferentsByPrefix  :: ShortHash -> m (Set Referent.Id),
-- branchHashesByPrefix   :: ShortBranchHash -> m (Set Branch.Hash),
--
-- --
-- lca              :: [Causal m Branch.Raw e] -> m (Maybe Branch.Hash)
-- dependentsImpl   :: Reference -> m (Maybe (Set Reference.Id))
-- termDependencies :: Reference.Id -> m (Maybe (Set Reference.Id))
-- declDependencies :: Reference.Id -> m (Maybe (Set Reference.Id))
-- -- |terms, types, patches, and branches
-- branchDependencies :: Branch.Hash -> m (Maybe (Branch.CausalHash,
--                                                BD.Dependencies))
-- -- |the "new" terms and types mentioned in a patch
-- patchDependencies :: EditHash -> m (Set Reference, Set Reference)
