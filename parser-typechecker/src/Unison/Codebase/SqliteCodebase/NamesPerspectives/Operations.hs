{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Code for working with NamesPerspectives in SQLite.
--
-- NOTE:
-- These implementations are from when we used SQLite in Share. Now they're unused, but there's a non-zero
-- chance we'll use these indexes in UCM in the future. However, we don't currently maintain the required indexes, so
-- they won't work as expected.
module Unison.Codebase.SqliteCodebase.NamesPerspectives.Operations {-# DEPRECATED "See module doc" #-} where

import Control.Comonad.Cofree qualified as Cofree
import Data.Either.Extra ()
import Data.Functor.Compose (Compose (..))
import Data.Map qualified as Map
import U.Codebase.Branch qualified as V2Branch
import U.Codebase.Branch.Diff (TreeDiff (TreeDiff))
import U.Codebase.Branch.Diff qualified as BranchDiff
import U.Codebase.HashTags (BranchHash)
import U.Codebase.Projects qualified as Projects
import U.Codebase.Reference qualified as C.Reference
import U.Codebase.Referent qualified as C.Referent
import U.Codebase.Sqlite.NameLookups (PathSegments (..), ReversedName (..))
import U.Codebase.Sqlite.NamedRef qualified as S
import U.Codebase.Sqlite.NamesPerspectives.Operations qualified as Ops
import U.Codebase.Sqlite.NamesPerspectives.Queries qualified as Q
import U.Codebase.Sqlite.Operations qualified as Ops
import U.Codebase.Sqlite.Queries qualified as Q
import Unison.Codebase.Path (Path)
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.SqliteCodebase.Conversions qualified as Cv
import Unison.ConstructorType qualified as CT
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment.Internal (NameSegment (NameSegment))
import Unison.Names (Names (Names))
import Unison.Names qualified as Names
import Unison.Prelude
import Unison.Sqlite (Transaction)
import Unison.Sqlite qualified as Sqlite
import Unison.Util.Recursion (XNor (Both, Neither), project)
import Unison.Util.Relation qualified as Rel

-- | Construct a 'ScopedNames' which can produce names which are relative to the provided
-- Path.
--
-- NOTE: this method requires an up-to-date name lookup index
namesAtPath ::
  BranchHash ->
  -- Include names from the project which contains this path.
  Path ->
  Transaction Names
namesAtPath bh path = do
  let namesRoot = PathSegments . coerce . Path.toList $ path
  namesPerspective@Ops.NamesPerspective {relativePerspective} <- Ops.namesPerspectiveForRootAndPath bh namesRoot
  let relativePath = Path.fromList $ coerce relativePerspective
  Ops.NamesInPerspective {termNamesInPerspective, typeNamesInPerspective} <- Ops.allNamesInPerspective namesPerspective
  let termsInPath = convertTerms termNamesInPerspective
  let typesInPath = convertTypes typeNamesInPerspective
  let relativeScopedNames =
        if relativePath == mempty
          then Names {terms = Rel.fromList termsInPath, types = Rel.fromList typesInPath}
          else
            let discardPathsNotUnder = mapMaybe . stripPathPrefix . reverse . Path.toList
                relativeTerms = discardPathsNotUnder relativePath termsInPath
                relativeTypes = discardPathsNotUnder relativePath typesInPath
             in Names {terms = Rel.fromList relativeTerms, types = Rel.fromList relativeTypes}
  pure $ relativeScopedNames
  where
    convertTypes names =
      names <&> \(S.NamedRef {reversedSegments, ref}) ->
        (Name.fromReverseSegments (coerce reversedSegments), Cv.reference2to1 ref)
    convertTerms names =
      names <&> \(S.NamedRef {reversedSegments, ref = (ref, ct)}) ->
        let v1ref = Cv.referent2to1UsingCT (fromMaybe (error "Required constructor type for constructor but it was null") ct) ref
         in (Name.fromReverseSegments (coerce reversedSegments), v1ref)

    -- If the given prefix matches the given name, the prefix is stripped and it's collected
    -- on the left, otherwise it's left as-is and collected on the right.
    -- >>> stripPathPrefix ["b", "a"] ("a.b.c", ())
    -- ([(c,())])
    stripPathPrefix :: [NameSegment] -> (Name, r) -> Maybe (Name, r)
    stripPathPrefix reversedPathSegments (n, ref) =
      case Name.stripReversedPrefix n reversedPathSegments of
        Nothing -> Nothing
        Just stripped -> Just (Name.makeRelative stripped, ref)

-- | Add an index for the provided branch hash if one doesn't already exist.
ensureNameLookupForBranchHash ::
  (C.Reference.Reference -> Sqlite.Transaction CT.ConstructorType) ->
  -- | An optional branch which we may already have an index for.
  -- This should be a branch which is relatively similar to the branch we're creating a name
  -- lookup for, e.g. a recent ancestor of the new branch. The more similar it is, the faster
  -- the less work we'll need to do.
  Maybe BranchHash ->
  BranchHash ->
  Sqlite.Transaction ()
ensureNameLookupForBranchHash getDeclType mayFromBranchHash toBranchHash = do
  Ops.checkBranchHashNameLookupExists toBranchHash >>= \case
    True -> pure ()
    False -> do
      (fromBranch, mayExistingLookupBH) <- case mayFromBranchHash of
        Nothing -> pure (V2Branch.empty, Nothing)
        Just fromBH -> do
          Ops.checkBranchHashNameLookupExists fromBH >>= \case
            True -> (,Just fromBH) <$> Ops.expectBranchByBranchHash fromBH
            False -> do
              -- TODO: We can probably infer a good starting branch by crawling through
              -- history looking for a Branch Hash we already have an index for.
              pure (V2Branch.empty, Nothing)
      toBranch <- Ops.expectBranchByBranchHash toBranchHash
      depMounts <- Projects.inferDependencyMounts toBranch <&> fmap (first (coerce @_ @PathSegments . Path.toList))
      let depMountPaths = (Path.fromList . coerce) . fst <$> depMounts
      treeDiff <- ignoreDepMounts depMountPaths <$> BranchDiff.diffBranches fromBranch toBranch
      let namePrefix = Nothing
      Ops.buildNameLookupForBranchHash
        mayExistingLookupBH
        toBranchHash
        ( \save -> do
            BranchDiff.streamNameChanges namePrefix treeDiff \_prefix (BranchDiff.NameChanges {termNameAdds, termNameRemovals, typeNameAdds, typeNameRemovals}) -> do
              termNameAddsWithCT <- do
                for termNameAdds \(name, ref) -> do
                  refWithCT <- addReferentCT ref
                  pure $ toNamedRef (name, refWithCT)
              save (termNameAddsWithCT, toNamedRef <$> termNameRemovals) (toNamedRef <$> typeNameAdds, toNamedRef <$> typeNameRemovals)
        )
      -- Ensure all of our dependencies have name lookups too.
      for_ depMounts \(_path, depBranchHash) -> do
        -- TODO: see if we can find a way to infer a good fromHash for dependencies
        ensureNameLookupForBranchHash getDeclType Nothing depBranchHash
      Ops.associateNameLookupMounts toBranchHash depMounts
  where
    alterTreeDiffAtPath :: (Functor m) => Path -> (TreeDiff m -> TreeDiff m) -> TreeDiff m -> TreeDiff m
    alterTreeDiffAtPath path f (TreeDiff cfr) =
      case project path of
        Neither -> f (TreeDiff cfr)
        Both segment rest ->
          let (a Cofree.:< (Compose rest')) = cfr
           in TreeDiff (a Cofree.:< Compose (Map.adjust (fmap (coerce $ alterTreeDiffAtPath rest f)) segment rest'))
    -- Delete portions of the diff which are covered by dependency mounts.
    ignoreDepMounts :: (Applicative m) => [Path] -> TreeDiff m -> TreeDiff m
    ignoreDepMounts depMounts treeDiff =
      foldl' (\acc path -> alterTreeDiffAtPath path (const mempty) acc) treeDiff depMounts
    toNamedRef :: (Name, ref) -> S.NamedRef ref
    toNamedRef (name, ref) = S.NamedRef {reversedSegments = coerce $ Name.reverseSegments name, ref = ref}
    addReferentCT :: C.Referent.Referent -> Transaction (C.Referent.Referent, Maybe C.Referent.ConstructorType)
    addReferentCT referent = case referent of
      C.Referent.Ref {} -> pure (referent, Nothing)
      C.Referent.Con ref _conId -> do
        ct <- getDeclType ref
        pure (referent, Just $ Cv.constructorType1to2 ct)

-- | Regenerate the name lookup index for the given branch hash from scratch.
-- This shouldn't be necessary in normal operation, but it's useful to fix name lookups if
-- they somehow get corrupt, or during local testing and debugging.
regenerateNameLookup ::
  (C.Reference.Reference -> Sqlite.Transaction CT.ConstructorType) ->
  BranchHash ->
  Sqlite.Transaction ()
regenerateNameLookup getDeclType bh = do
  Ops.checkBranchHashNameLookupExists bh >>= \case
    True -> do
      bhId <- Q.expectBranchHashId bh
      Q.deleteNameLookup bhId
      ensureNameLookupForBranchHash getDeclType Nothing bh
    False -> ensureNameLookupForBranchHash getDeclType Nothing bh
