{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Code for working with NamesPerspectives in SQLite.
--
-- NOTE:
-- These implementations are from when we used SQLite in Share. Now they're unused, but there's a non-zero
-- chance we'll use these indexes in UCM in the future. However, we don't currently maintain the required indexes, so
-- they won't work as expected.
module U.Codebase.Sqlite.NamesPerspectives.Operations {-# DEPRECATED "See module doc" #-} where

import Control.Lens hiding (children)
import Data.List.Extra qualified as List
import Data.List.NonEmpty.Extra qualified as NonEmpty
import Data.Set qualified as Set
import Data.Tuple.Extra ((***))
import U.Codebase.HashTags (BranchHash (..))
import U.Codebase.Reference qualified as C
import U.Codebase.Referent qualified as C
import U.Codebase.Sqlite.DbId qualified as Db
import U.Codebase.Sqlite.NameLookups (PathSegments (..))
import U.Codebase.Sqlite.NameLookups qualified as NameLookups
import U.Codebase.Sqlite.NameLookups qualified as S
import U.Codebase.Sqlite.NamedRef qualified as S
import U.Codebase.Sqlite.NamesPerspectives.Queries qualified as Q
import U.Codebase.Sqlite.Operations
import U.Codebase.Sqlite.Queries qualified as Q
import Unison.Prelude
import Unison.Sqlite
import Unison.Util.List qualified as List

-- | Apply a set of name updates to an existing index.
buildNameLookupForBranchHash ::
  -- The existing name lookup index to copy before applying the diff.
  -- If Nothing, run the diff against an empty index.
  -- If Just, the name lookup must exist or an error will be thrown.
  Maybe BranchHash ->
  BranchHash ->
  ( ( -- (add terms, remove terms)
      ([S.NamedRef (C.Referent, Maybe C.ConstructorType)], [S.NamedRef C.Referent]) ->
      --  (add types, remove types)
      ([S.NamedRef C.Reference], [S.NamedRef C.Reference]) ->
      Transaction ()
    ) ->
    Transaction ()
  ) ->
  Transaction ()
buildNameLookupForBranchHash mayExistingBranchIndex newBranchHash callback = do
  newBranchHashId <- Q.expectBranchHashId newBranchHash
  Q.trackNewBranchHashNameLookup newBranchHashId
  case mayExistingBranchIndex of
    Nothing -> pure ()
    Just existingBranchIndex -> do
      unlessM (checkBranchHashNameLookupExists existingBranchIndex) $ error "buildNameLookupForBranchHash: existingBranchIndex was provided, but no index was found for that branch hash."
      existingBranchHashId <- Q.expectBranchHashId existingBranchIndex
      Q.copyScopedNameLookup existingBranchHashId newBranchHashId
  callback \(newTermNames, removedTermNames) (newTypeNames, removedTypeNames) -> do
    Q.removeScopedTermNames newBranchHashId ((fmap c2sTextReferent <$> removedTermNames))
    Q.removeScopedTypeNames newBranchHashId ((fmap c2sTextReference <$> removedTypeNames))
    Q.insertScopedTermNames newBranchHashId (fmap (c2sTextReferent *** fmap c2sConstructorType) <$> newTermNames)
    Q.insertScopedTypeNames newBranchHashId (fmap c2sTextReference <$> newTypeNames)

-- | Save a list of (mount-path, branch hash) mounts for the provided name lookup index branch
-- hash.
--
-- E.g. associateNameLookupMounts #roothash [(["lib", "base"], #basehash)]
associateNameLookupMounts :: BranchHash -> [(PathSegments, BranchHash)] -> Transaction ()
associateNameLookupMounts rootBh dependencyMounts = do
  rootBhId <- Q.expectBranchHashId rootBh
  depMounts <- for dependencyMounts \(path, branchHash) -> do
    branchHashId <- Q.expectBranchHashId branchHash
    pure (path, branchHashId)
  Q.associateNameLookupMounts rootBhId depMounts

-- | Any time we need to lookup or search names we need to know what the scope of that search
-- should be. This can be complicated to keep track of, so this is a helper type to make it
-- easy to pass around.
--
-- You should use 'namesPerspectiveForRootAndPath' to construct this type.
--
-- E.g. if we're in loose code, we need to search the correct name lookup for the
-- user's perspective. If their perspective is "myprojects.json.latest.lib.base.data.List",
-- we need to search names using the name index mounted at "myprojects.json.latest.lib.base".
--
-- The NamesPerspective representing this viewpoint would be:
--
-- @@
-- NamesPerspective
--  { nameLookupBranchHashId = #libbasehash
--  , pathToMountedNameLookup = ["myprojects.json", "latest", "lib", "base"]
--  , relativePerspective = ["data", "List"]
--  }
-- @@
data NamesPerspective = NamesPerspective
  { -- | The branch hash of the name lookup we'll use for queries
    nameLookupBranchHashId :: Db.BranchHashId,
    -- | Where the name lookup is mounted relative to the root branch
    pathToMountedNameLookup :: PathSegments,
    -- | The path to the perspective relative to the current name lookup
    relativePerspective :: PathSegments
  }
  deriving (Eq, Show)

-- | Determine which nameLookup is the closest parent of the provided perspective.
--
-- Returns (rootBranchId of the closest parent index, namespace that index is mounted at, location of the perspective within the mounted namespace)
--
-- E.g.
-- If your namespace is "lib.distributed.lib.base.data.List", you'd get back
-- (rootBranchId of the lib.distributed.lib.base name lookup, "lib.distributed.lib.base", "data.List")
--
-- Or if your namespace is "subnamespace.user", you'd get back
-- (the rootBranchId you provided, "", "subnamespace.user")
--
-- These implementations are from when we used SQLite in Share. Now they're unused, but there's a non-zero
-- chance we'll use these indexes in UCM in the future. However, we don't currently maintain the required indexes, so
-- they won't work as expected.
namesPerspectiveForRootAndPath :: BranchHash -> PathSegments -> Transaction NamesPerspective
namesPerspectiveForRootAndPath rootBh namespace = do
  rootBhId <- Q.expectBranchHashId rootBh
  namesPerspectiveForRootAndPathHelper rootBhId namespace
  where
    namesPerspectiveForRootAndPathHelper :: Db.BranchHashId -> PathSegments -> Transaction NamesPerspective
    namesPerspectiveForRootAndPathHelper rootBhId pathSegments = do
      let defaultPerspective =
            NamesPerspective
              { nameLookupBranchHashId = rootBhId,
                pathToMountedNameLookup = (PathSegments []),
                relativePerspective = pathSegments
              }
      fmap (fromMaybe defaultPerspective) . runMaybeT $
        do
          mounts <- lift $ Q.listNameLookupMounts rootBhId
          mounts
            & altMap \(mountPathSegments, mountBranchHash) -> do
              case List.splitOnLongestCommonPrefix (into @[Text] pathSegments) (into @[Text] mountPathSegments) of
                -- The path is within this mount:
                (_, remainingPath, []) ->
                  lift $
                    namesPerspectiveForRootAndPathHelper mountBranchHash (into @PathSegments remainingPath)
                      <&> \(NamesPerspective {nameLookupBranchHashId, pathToMountedNameLookup = mountLocation, relativePerspective}) ->
                        NamesPerspective
                          { nameLookupBranchHashId,
                            -- Ensure we return the correct mount location even if the mount is
                            -- several levels deep
                            pathToMountedNameLookup = mountPathSegments <> mountLocation,
                            relativePerspective
                          }
                -- The path is not within this mount:
                _ -> empty

-- | Check whether we've already got an index for a given branch hash.
checkBranchHashNameLookupExists :: BranchHash -> Transaction Bool
checkBranchHashNameLookupExists bh = do
  bhId <- Q.expectBranchHashId bh
  Q.checkBranchHashNameLookupExists bhId

data NamesInPerspective = NamesInPerspective
  { termNamesInPerspective :: [S.NamedRef (C.Referent, Maybe C.ConstructorType)],
    typeNamesInPerspective :: [S.NamedRef C.Reference]
  }

-- | Get all the term and type names for the given namespace from the lookup table.
-- Requires that an index for this branch hash already exists, which is currently
-- only true on Share.
allNamesInPerspective ::
  NamesPerspective ->
  Transaction NamesInPerspective
allNamesInPerspective NamesPerspective {nameLookupBranchHashId, pathToMountedNameLookup} = do
  termNamesInPerspective <- Q.termNamesWithinNamespace nameLookupBranchHashId mempty
  typeNamesInPerspective <- Q.typeNamesWithinNamespace nameLookupBranchHashId mempty
  let convertTerms = prefixNamedRef pathToMountedNameLookup . fmap (bimap s2cTextReferent (fmap s2cConstructorType))
  let convertTypes = prefixNamedRef pathToMountedNameLookup . fmap s2cTextReference
  pure $
    NamesInPerspective
      { termNamesInPerspective = convertTerms <$> termNamesInPerspective,
        typeNamesInPerspective = convertTypes <$> typeNamesInPerspective
      }

-- | NOTE: requires that the codebase has an up-to-date name lookup index. As of writing, this
-- is only true on Share.
--
-- Get the list of a names for a given Referent.
termNamesForRefWithinNamespace :: NamesPerspective -> C.Referent -> Maybe S.ReversedName -> Transaction [S.ReversedName]
termNamesForRefWithinNamespace NamesPerspective {nameLookupBranchHashId, pathToMountedNameLookup} ref maySuffix = do
  Q.termNamesForRefWithinNamespace nameLookupBranchHashId mempty (c2sTextReferent ref) maySuffix
    <&> fmap (prefixReversedName pathToMountedNameLookup)

-- | NOTE: requires that the codebase has an up-to-date name lookup index. As of writing, this
-- is only true on Share.
--
-- Get the list of a names for a given Reference, with an optional required suffix.
typeNamesForRefWithinNamespace :: NamesPerspective -> C.Reference -> Maybe S.ReversedName -> Transaction [S.ReversedName]
typeNamesForRefWithinNamespace NamesPerspective {nameLookupBranchHashId, pathToMountedNameLookup} ref maySuffix = do
  Q.typeNamesForRefWithinNamespace nameLookupBranchHashId mempty (c2sTextReference ref) maySuffix
    <&> fmap (prefixReversedName pathToMountedNameLookup)

termNamesBySuffix :: NamesPerspective -> S.ReversedName -> Transaction [S.NamedRef (C.Referent, Maybe C.ConstructorType)]
termNamesBySuffix NamesPerspective {nameLookupBranchHashId, pathToMountedNameLookup} suffix = do
  Q.termNamesBySuffix nameLookupBranchHashId mempty suffix
    <&> fmap (prefixNamedRef pathToMountedNameLookup >>> fmap (bimap s2cTextReferent (fmap s2cConstructorType)))

typeNamesBySuffix :: NamesPerspective -> S.ReversedName -> Transaction [S.NamedRef C.Reference]
typeNamesBySuffix NamesPerspective {nameLookupBranchHashId, pathToMountedNameLookup} suffix = do
  Q.typeNamesBySuffix nameLookupBranchHashId mempty suffix
    <&> fmap (prefixNamedRef pathToMountedNameLookup >>> fmap s2cTextReference)

-- | Helper for findings refs by name within the correct mounted indexes.
refsForExactName ::
  (Db.BranchHashId -> S.ReversedName -> Transaction [S.NamedRef ref]) ->
  NamesPerspective ->
  S.ReversedName ->
  Transaction [S.NamedRef ref]
refsForExactName query NamesPerspective {nameLookupBranchHashId, pathToMountedNameLookup} name = do
  namedRefs <- query nameLookupBranchHashId name
  pure $
    namedRefs
      <&> prefixNamedRef pathToMountedNameLookup

-- | Requalifies a NamedRef to some namespace prefix.
prefixNamedRef :: NameLookups.PathSegments -> S.NamedRef ref -> S.NamedRef ref
prefixNamedRef prefix S.NamedRef {reversedSegments, ref} =
  S.NamedRef {reversedSegments = prefixReversedName prefix reversedSegments, ref}

-- | Requalifies a ReversedName to some namespace prefix.
prefixReversedName :: PathSegments -> S.ReversedName -> S.ReversedName
prefixReversedName (S.PathSegments prefix) (S.ReversedName reversedSegments) =
  S.ReversedName $ NonEmpty.appendl reversedSegments (reverse prefix)

termRefsForExactName :: NamesPerspective -> S.ReversedName -> Transaction [S.NamedRef (C.Referent, Maybe C.ConstructorType)]
termRefsForExactName namesPerspective reversedName = do
  refsForExactName Q.termRefsForExactName namesPerspective reversedName
    <&> fmap (fmap (bimap s2cTextReferent (fmap s2cConstructorType)))

typeRefsForExactName :: NamesPerspective -> S.ReversedName -> Transaction [S.NamedRef C.Reference]
typeRefsForExactName namesPerspective reversedName = do
  refsForExactName Q.typeRefsForExactName namesPerspective reversedName <&> fmap (fmap s2cTextReference)

-- | Get the name within the provided namespace that has the longest matching suffix
-- with the provided name, but a different ref.
-- This is a bit of a hack but allows us to shortcut suffixification.
-- We can clean this up if we make a custom PPE type just for sqlite pretty printing, but
-- for now this works fine.
longestMatchingTermNameForSuffixification :: NamesPerspective -> S.NamedRef C.Referent -> Transaction (Maybe (S.NamedRef (C.Referent, Maybe C.ConstructorType)))
longestMatchingTermNameForSuffixification NamesPerspective {nameLookupBranchHashId, pathToMountedNameLookup} namedRef = do
  Q.longestMatchingTermNameForSuffixification nameLookupBranchHashId mempty (c2sTextReferent <$> namedRef)
    <&> fmap (prefixNamedRef pathToMountedNameLookup >>> fmap (bimap s2cTextReferent (fmap s2cConstructorType)))

-- | Get the name within the provided namespace that has the longest matching suffix
-- with the provided name, but a different ref.
-- This is a bit of a hack but allows us to shortcut suffixification.
-- We can clean this up if we make a custom PPE type just for sqlite pretty printing, but
-- for now this works fine.
longestMatchingTypeNameForSuffixification :: NamesPerspective -> S.NamedRef C.Reference -> Transaction (Maybe (S.NamedRef C.Reference))
longestMatchingTypeNameForSuffixification NamesPerspective {nameLookupBranchHashId, pathToMountedNameLookup} namedRef = do
  Q.longestMatchingTypeNameForSuffixification nameLookupBranchHashId mempty (c2sTextReference <$> namedRef)
    <&> fmap (prefixNamedRef pathToMountedNameLookup >>> fmap s2cTextReference)

-- | Searches all dependencies transitively looking for the provided ref within the
-- provided namespace.
-- Prefer 'termNamesForRefWithinNamespace' in most cases.
-- This is slower and only necessary when resolving the name of refs when you don't know which
-- dependency it may exist in.
--
-- Searching transitive dependencies is exponential so we want to replace this with a more
-- efficient approach as soon as possible.
--
-- Note: this returns the first name it finds by searching in order of:
-- Names in the current namespace, then names in the current namespace's dependencies, then
-- through the current namespace's dependencies' dependencies, etc.
recursiveTermNameSearch :: NamesPerspective -> C.Referent -> Transaction (Maybe S.ReversedName)
recursiveTermNameSearch NamesPerspective {nameLookupBranchHashId} r = do
  Q.recursiveTermNameSearch nameLookupBranchHashId (c2sTextReferent r)

-- | Searches all dependencies transitively looking for the provided ref within the provided
-- namespace.
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
recursiveTypeNameSearch :: NamesPerspective -> C.Reference -> Transaction (Maybe S.ReversedName)
recursiveTypeNameSearch NamesPerspective {nameLookupBranchHashId} r = do
  Q.recursiveTypeNameSearch nameLookupBranchHashId (c2sTextReference r)

-- | Search for term or type names which contain the provided list of segments in order.
-- Search is case insensitive.
fuzzySearchDefinitions ::
  Bool ->
  NamesPerspective ->
  -- | Will return at most n terms and n types; i.e. max number of results is 2n
  Int ->
  [Text] ->
  Transaction ([S.NamedRef (C.Referent, Maybe C.ConstructorType)], [S.NamedRef C.Reference])
fuzzySearchDefinitions includeDependencies NamesPerspective {nameLookupBranchHashId, relativePerspective} limit querySegments = do
  termNames <-
    Q.fuzzySearchTerms includeDependencies nameLookupBranchHashId limit relativePerspective querySegments
      <&> fmap \termName ->
        termName
          & (fmap (bimap s2cTextReferent (fmap s2cConstructorType)))
          & stripPrefixFromNamedRef relativePerspective
  typeNames <-
    Q.fuzzySearchTypes includeDependencies nameLookupBranchHashId limit relativePerspective querySegments
      <&> fmap (fmap s2cTextReference)
      <&> fmap \typeName ->
        typeName
          & stripPrefixFromNamedRef relativePerspective
  pure (termNames, typeNames)

-- | Delete any name lookup that's not in the provided list.
--
-- This can be used to garbage collect unreachable name lookups.
deleteNameLookupsExceptFor :: Set BranchHash -> Transaction ()
deleteNameLookupsExceptFor reachable = do
  bhIds <- for (Set.toList reachable) Q.expectBranchHashId
  Q.deleteNameLookupsExceptFor bhIds

-- | Strips a prefix path from a named ref. No-op if the prefix doesn't match.
--
-- >>> stripPrefixFromNamedRef (PathSegments ["foo", "bar"]) (S.NamedRef (S.ReversedName ("baz" NonEmpty.:| ["bar", "foo"])) ())
-- NamedRef {reversedSegments = ReversedName ("baz" :| []), ref = ()}
--
-- >>> stripPrefixFromNamedRef (PathSegments ["no", "match"]) (S.NamedRef (S.ReversedName ("baz" NonEmpty.:| ["bar", "foo"])) ())
-- NamedRef {reversedSegments = ReversedName ("baz" :| ["bar","foo"]), ref = ()}
stripPrefixFromNamedRef :: PathSegments -> S.NamedRef r -> S.NamedRef r
stripPrefixFromNamedRef (PathSegments prefix) namedRef =
  let newReversedName =
        S.reversedSegments namedRef
          & \case
            reversedName@(S.ReversedName (name NonEmpty.:| reversedPath)) ->
              case List.stripSuffix (reverse prefix) reversedPath of
                Nothing -> reversedName
                Just strippedReversedPath -> S.ReversedName (name NonEmpty.:| strippedReversedPath)
   in namedRef {S.reversedSegments = newReversedName}
