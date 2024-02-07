{-# LANGUAGE RecordWildCards #-}

module Unison.Merge3
  (
  )
where

-- import Data.List.NonEmpty
-- import Data.Map (Map)
-- import Data.Map.NonEmpty (NEMap)
-- import Unison.Core.Project (ProjectBranchName)
-- import Unison.Merge2 (Defns (Defns))
-- import Unison.Name (Name)
-- import Unison.Prelude
-- import Unison.Reference (TermReferenceId, TypeReference, TypeReferenceId)
-- import Unison.Referent (Referent)
-- import Unison.Sqlite (Transaction)
-- import Unison.UnisonFile (TypecheckedUnisonFile, UnisonFile)

-- type Namespace = Defns (Map Name Referent) (Map Name TypeReference)

-- type NamespaceIds = Defns (Map Name TermReferenceId) (Map Name TypeReferenceId)

-- -- ns:
-- -- foo _ = 2
-- -- baz _ = #foo () + 1
-- --
-- -- scratch:
-- -- foo#foo2 _ = #baz () + 1
-- --
-- -- a) We don't include simple updates in the unisonfile; cycle fails to form; foo refers to old baz.
-- --  file is populated with dependents in namespace of thing called `foo`: {baz}
-- --    baz _  = #foo () + 1 -- dependent of thing called `foo`
-- --
-- -- b) We do include simple updates in the typechecked file; cycle forms correctly
-- --    baz _ = foo () + 1
-- --    foo _ = baz () + 1

-- -- Do want conflicted definitions in the scratch file even if they're not transitive dependents of updates.
-- -- Can't convince myself one way or another for Merge w/o conflicts
-- -- lca:
-- -- foo#foo _ = #baz () + 1
-- -- baz#baz _ = #foo () + 1
-- --
-- -- alice:
-- -- foo#foo2 _ = #baz2 () + 1
-- -- baz#baz2 _ = #foo2 () + 1
-- --
-- -- bob:
-- -- foo _ = #baz () + 1
-- -- baz _ = #foo () + 1
-- --
-- -- updates: {foo#foo2, baz#baz2}
-- --
-- --
-- --
-- -- a) We don't include simple updates in the unisonfile; cycle fails to form; foo refers to old baz.
-- -- b) We do include simple updates in the typechecked file; cycle forms correctly

-- merge :: Namespace -> Namespace -> Namespace -> Transaction ()
-- merge lca aliceNamespace bobNamespace = do
--   buildDiffThingo lca aliceNamespace bobNamespace >>= \case
--     FailedPrereqThingo prereq -> reportFailedPrereq prereq
--     ConflictThingo conflictThingo -> writeMergeConflictToScratchFile conflictThingo
--     GoodThingo goodThingo -> do
--       mergeUf <- buildMergeUf goodThingo lca aliceNamespace bobNamespace
--       let mergeNamespace = makeMergeNamespace lca aliceNamespace bobNamespace
--       case typecheck mergeNamespace mergeUf of
--         Just mergeTuf -> do
--           newNamespace <- applyTufToNamespace mergeNamespace mergeTuf
--           saveTuf mergeTuf
--           saveConsNamespace newNamespace
--         Nothing -> do
--           tempBranchName <- getTempBranchName
--           forkBranch tempBranchName
--           saveConsNamespace mergeNamespace
--           writeUfToScratchFile mergeUf

-- reportFailedPrereq :: Prereq -> Transaction ()
-- reportFailedPrereq = wundefined

-- writeUfToScratchFile :: UnisonFile v a -> Transaction ()
-- writeUfToScratchFile = wundefined

-- -- switch us to this other branch, for the purposes of the saveCons call
-- forkBranch :: ProjectBranchName -> Transaction ()
-- forkBranch = wundefined

-- getTempBranchName :: Transaction ProjectBranchName
-- getTempBranchName = wundefined

-- saveConsNamespace :: Namespace -> Transaction ()
-- saveConsNamespace = wundefined

-- saveTuf :: TypecheckedUnisonFile v a -> Transaction ()
-- saveTuf = wundefined

-- applyTufToNamespace :: Namespace -> TypecheckedUnisonFile v a -> Transaction Namespace
-- applyTufToNamespace = wundefined

-- typecheck :: Namespace -> UnisonFile v a -> Maybe (TypecheckedUnisonFile v a)
-- typecheck = wundefined

-- makeMergeNamespace :: Namespace -> Namespace -> Namespace -> Namespace
-- makeMergeNamespace lca alice bob = wundefined

-- buildMergeUf :: GoodThingo -> Namespace -> Namespace -> Namespace -> Transaction (UnisonFile v a)
-- buildMergeUf mergeResult lca alice bob = wundefined

-- writeMergeConflictToScratchFile :: ConflictThingo -> Transaction ()
-- writeMergeConflictToScratchFile = wundefined

-- buildDiffThingo :: Namespace -> Namespace -> Namespace -> Transaction DiffThingo
-- buildDiffThingo lca alice bob = wundefined

-- update :: TypecheckedUnisonFile v a -> Namespace -> Transaction ()
-- update tuf namespace = do
--   updatesPlusDependentsUf <- addDependentsToTuf tuf namespace
--   case typecheck namespace updatesPlusDependentsUf of
--     Just updatesPlusDependentsTuf -> do
--       newNamespace <- applyTufToNamespace namespace updatesPlusDependentsTuf
--       saveTuf updatesPlusDependentsTuf
--       saveConsNamespace newNamespace
--     Nothing -> do
--       writeUfToScratchFile updatesPlusDependentsUf

-- addDependentsToTuf :: TypecheckedUnisonFile v a -> Namespace -> Transaction (UnisonFile v a)
-- addDependentsToTuf tuf ns = do
--   dependentsUf <- computeTufDependents tuf ns
--   wundefined

-- upgradeLibrary :: Namespace -> Namespace -> Namespace -> Transaction ()
-- upgradeLibrary oldNamespace newNamespace project = do
--   upgradeTuf <- buildUpdateTuf oldNamespace newNamespace
--   dependentsUf <- computeTufDependents upgradeTuf (withoutLib project)
--   wundefined

-- -- remove the `lib` subnamespace
-- withoutLib :: Namespace -> Namespace
-- withoutLib = wundefined

-- buildUpdateTuf :: Namespace -> Namespace -> Transaction (TypecheckedUnisonFile v a)
-- buildUpdateTuf old new = wundefined

-- -- | find the dependents in `ns` of the names in `tuf`, and substitute vars for each dependent.
-- computeTufDependents :: TypecheckedUnisonFile v a -> Namespace -> Transaction (UnisonFile v a)
-- computeTufDependents tuf ns = do
--   dependents :: NamespaceIds <- filterToDependentsOf (tufToNames tuf) ns
--   substituteNames dependents

-- substituteNames :: NamespaceIds -> Transaction (UnisonFile v a)
-- substituteNames = wundefined

-- tufToNames :: TypecheckedUnisonFile v a -> t00
-- tufToNames = wundefined

-- filterToDependentsOf :: t0 -> Namespace -> Transaction NamespaceIds
-- filterToDependentsOf = wundefined

-- type NameThingo r = Map Name ([Name], r)

-- newtype Source = Source ProjectBranchName

-- type ConflictNameThingo r = NameThingo (NEMap Source r)

-- type ConflictThingo = Defns (ConflictNameThingo Referent) (ConflictNameThingo TypeReference)

-- type GoodThingo = Defns (NameThingo (Source, Referent)) (NameThingo (Source, TypeReference))

-- data Prereq = Prereq

-- data DiffThingo
--   = ConflictThingo ConflictThingo
--   | GoodThingo GoodThingo
--   | FailedPrereqThingo Prereq

-- -- instance Semigroup DiffThingo where
-- --   GoodThingo (Defns tms1 tps1) <> GoodThingo (Defns tms2 tps2) =
-- --     let conflictedTms =

-- -- instance Monoid DiffThingo where
-- --   mempty = GoodThingo (Defns mempty mempty)
