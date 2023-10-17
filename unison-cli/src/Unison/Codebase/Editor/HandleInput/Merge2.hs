-- | @merge@ input handler
module Unison.Codebase.Editor.HandleInput.Merge2
  ( handleMerge,
  )
where

import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Lens (over, (^.))
import Control.Monad.Except qualified as Except (throwError)
import Control.Monad.Reader (ask)
import Control.Monad.State.Strict (StateT)
import Control.Monad.State.Strict qualified as State
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans.Except qualified as Except
import Data.Foldable (foldlM)
import Data.Function (on)
import Data.Functor.Compose (Compose (..))
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.List.NonEmpty (pattern (:|))
import Data.List.NonEmpty qualified as List.NonEmpty
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Semialign (alignWith)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.These (These (..))
import GHC.Clock (getMonotonicTime)
import Text.ANSI qualified as Text
import Text.Printf (printf)
import U.Codebase.Branch (Branch, CausalBranch)
import U.Codebase.Branch qualified as Branch
import U.Codebase.BranchV3 (BranchV3 (..), CausalBranchV3)
import U.Codebase.BranchV3 qualified as BranchV3
import U.Codebase.Causal (Causal)
import U.Codebase.Causal qualified as Causal
import U.Codebase.HashTags (BranchHash (..), CausalHash (..))
import U.Codebase.Reference
  ( Reference,
    Reference' (..),
    ReferenceType,
    TermReferenceId,
    TypeReference,
    TypeReferenceId,
  )
import U.Codebase.Reference qualified as Reference
import U.Codebase.Referent (Referent)
import U.Codebase.Referent qualified as Referent
import U.Codebase.Sqlite.HashHandle qualified as HashHandle
import U.Codebase.Sqlite.Operations qualified as Operations
import U.Codebase.Sqlite.V2.HashHandle (v2HashHandle)
import Unison.Builtin qualified as Builtins
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.ProjectUtils qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as V1 (Branch, Branch0)
import Unison.Codebase.Branch qualified as V1.Branch
import Unison.Codebase.Causal qualified as V1 (Causal)
import Unison.Codebase.Causal qualified as V1.Causal
import Unison.Codebase.Causal.Type qualified as V1.Causal
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Path qualified as Path
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.ConstructorType (ConstructorType)
import Unison.DataDeclaration qualified as V1.Decl
import Unison.Hash (Hash)
import Unison.Hash qualified as Hash
import Unison.Merge2 (MergeOutput)
import Unison.Merge2 qualified as Merge
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment (..))
import Unison.NameSegment qualified as NameSegment
import Unison.Prelude hiding (catMaybes)
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl)
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Project (ProjectAndBranch (..), ProjectBranchName)
import Unison.Reference (TermReference)
import Unison.Referent qualified as V1 (Referent)
import Unison.Referent qualified as V1.Referent
import Unison.ShortHash (ShortHash)
import Unison.ShortHash qualified as ShortHash
import Unison.Sqlite (Transaction)
import Unison.Sqlite qualified as Sqlite
import Unison.Syntax.Name qualified as Name (toText)
import Unison.UnisonFile.Type (TypecheckedUnisonFile (TypecheckedUnisonFileId), UnisonFile)
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Cache qualified as Cache
import Unison.Util.Map qualified as Map
import Unison.Util.Relation qualified as Relation
import Unison.Util.Set qualified as Set
import Unison.Util.Star3 (Star3)
import Unison.Util.Star3 qualified as Star3
import Witch (unsafeFrom)

-- Temporary simple way to time a transaction
step :: Text -> Transaction a -> Transaction a
step name action = do
  t0 <- Sqlite.unsafeIO getMonotonicTime
  result <- action
  Sqlite.unsafeIO do
    t1 <- getMonotonicTime
    Text.putStrLn (Text.pack (printf "%4d ms | " (round ((t1 - t0) * 1000) :: Int)) <> name)
  pure result

data MergePreconditionViolation
  = ConflictedAliases !ProjectBranchName !Name !Name
  | -- A name refers to two different terms
    ConflictedTermName !(Set Referent)
  | -- A name refers to two different terms
    ConflictedTypeName !(Set TypeReference)
  | -- We can't put a builtin in a scratch file, so we bomb in situations where we'd have to
    ConflictInvolvingBuiltin
  | -- A second naming of a constructor was discovered underneath a decl's name, e.g.
    --
    --   Foo#Foo
    --   Foo.Bar#Foo#0
    --   Foo.Some.Other.Name.For.Bar#Foo#0
    ConstructorAlias !Name
  | -- There were some definitions at the top level of lib.*, which we don't like
    DefnsInLib
  | MissingConstructorName !Name
  | NestedDeclAlias !Name
  | NoConstructorNames !Name
  | StrayConstructor !Name
  deriving stock (Show)

data MergeResult v a
  = -- PPED is whatever `prettyUnisonFile` accepts
    MergePropagationNotTypecheck PPED.PrettyPrintEnvDecl (UnisonFile v a)
  | MergeConflicts PPED.PrettyPrintEnvDecl (MergeOutput v a)
  | MergeDone

handleMerge :: ProjectBranchName -> Cli ()
handleMerge bobBranchName = do
  -- Load the current project branch ("alice"), and the branch from the same project to merge in ("bob")
  (ProjectAndBranch project aliceProjectBranch, _path) <- Cli.expectCurrentProjectBranch
  bobProjectBranch <- Cli.expectProjectBranchByName project bobBranchName
  let alicePath = Cli.projectBranchPath (ProjectAndBranch (project ^. #projectId) (aliceProjectBranch ^. #branchId))
  let bobPath = Cli.projectBranchPath (ProjectAndBranch (project ^. #projectId) (bobProjectBranch ^. #branchId))

  Cli.Env {codebase} <- ask
  loadTerm <- do
    cache <- Cache.semispaceCache 1024
    pure (cacheTransaction cache (Codebase.unsafeGetTerm codebase))
  loadDecl <- do
    cache <- Cache.semispaceCache 1024
    pure (cacheTransaction cache (Codebase.unsafeGetTypeDeclaration codebase))
  -- Since loading a decl type loads the decl and projects out the decl type, just reuse the loadDecl cache
  let loadDeclType ref =
        case ref of
          ReferenceBuiltin name ->
            Map.lookup ref Builtins.builtinConstructorType
              & maybe (error ("Unknown builtin: " ++ Text.unpack name)) pure
          ReferenceDerived refId -> V1.Decl.constructorType <$> loadDecl refId
  loadDeclNumConstructors <- do
    cache <- Cache.semispaceCache 1024
    pure (cacheTransaction cache Operations.expectDeclNumConstructors)

  result <-
    Cli.runTransactionWithRollback2 \rollback -> do
      -- Load causals
      aliceCausal <- step "load alice causal" $ Codebase.getShallowCausalFromRoot Nothing (Path.unabsolute alicePath)
      bobCausal <- step "load bob causal" $ Codebase.getShallowCausalFromRoot Nothing (Path.unabsolute bobPath)

      -- Load shallow branches
      aliceBranch <- step "load shallow alice branch" $ Causal.value aliceCausal
      bobBranch <- step "load shallow bob branch" $ Causal.value bobCausal

      -- Load deep definitions
      (aliceDefns, aliceCausalTree) <- step "load alice definitions" $ loadNamespaceDefns loadDeclNumConstructors aliceBranch (aliceCausal ^. #causalHash) & onLeftM (rollback . Left)
      (bobDefns, bobCausalTree) <- step "load bob definitions" $ loadNamespaceDefns loadDeclNumConstructors bobBranch (bobCausal ^. #causalHash) & onLeftM (rollback . Left)
      let defns = Merge.TwoWay {alice = aliceDefns, bob = bobDefns}
      let causalHashes = Merge.TwoWay {alice = aliceCausalTree, bob = bobCausalTree}

      (maybeLcaLibdeps, diffs) <- do
        step "compute lca" (Operations.lca (Causal.causalHash aliceCausal) (Causal.causalHash bobCausal)) >>= \case
          Nothing -> do
            diffs <-
              Merge.nameBasedNamespaceDiff
                loadDecl
                loadTerm
                Merge.TwoOrThreeWay {lca = Nothing, alice = aliceDefns, bob = bobDefns}
            pure (Nothing, diffs)
          Just lcaCausalHash -> do
            lcaCausal <- step "load lca causal" $ Operations.expectCausalBranchByCausalHash lcaCausalHash
            lcaBranch <- step "load lca shallow branch" $ Causal.value lcaCausal
            (lcaDefns, _) <- step "load lca definitions" do
              loadNamespaceDefns loadDeclNumConstructors lcaBranch (lcaCausal ^. #causalHash) & onLeftM (rollback . Left)
            diffs <-
              Merge.nameBasedNamespaceDiff
                loadDecl
                loadTerm
                Merge.TwoOrThreeWay {lca = Just lcaDefns, alice = aliceDefns, bob = bobDefns}
            step "look for alice conflicted aliases" do
              findConflictedAlias aliceDefns (diffs ^. #alice) & onJust \(name1, name2) ->
                rollback (Left (ConflictedAliases (aliceProjectBranch ^. #name) name1 name2))
            step "look for bob conflicted aliases" do
              findConflictedAlias bobDefns (diffs ^. #bob) & onJust \(name1, name2) ->
                rollback (Left (ConflictedAliases (bobProjectBranch ^. #name) name1 name2))
            lcaLibdeps <- step "load lca library dependencies" $ loadLibdeps lcaBranch
            pure (Just lcaLibdeps, diffs)

      let conflictedNames =
            Merge.Defns
              { terms = conflictsish (diffs ^. #alice . #terms) (diffs ^. #bob . #terms),
                types = conflictsish (diffs ^. #alice . #types) (diffs ^. #bob . #types)
              }

      -- Load and merge libdeps
      mergedLibdeps <- do
        aliceLibdeps <- step "load alice library dependencies" $ loadLibdeps aliceBranch
        bobLibdeps <- step "load bob library dependencies" $ loadLibdeps bobBranch
        pure $
          Merge.mergeLibdeps
            ((==) `on` Causal.causalHash)
            getTwoFreshNames
            maybeLcaLibdeps
            aliceLibdeps
            bobLibdeps

      -- For some things below we only care about the `Map Name ref` direction of our `BiMultimap ref Name` definitions
      let aliceNames :: Merge.Defns (Map Name Referent) (Map Name TypeReference)
          aliceNames = aliceDefns & over #terms BiMultimap.range & over #types BiMultimap.range

      let bobNames :: Merge.Defns (Map Name Referent) (Map Name TypeReference)
          bobNames = bobDefns & over #terms BiMultimap.range & over #types BiMultimap.range

      let updates = filterUpdates defns diffs

      whatToTypecheck :: Merge.WhatToTypecheck <-
        step "compute whatToTypecheck" $
          Merge.whatToTypecheck (aliceNames, updates ^. #alice) (bobNames, updates ^. #bob)

      -- If there are no conflicts, then proceed to typechecking
      mergeResult <-
        if null (conflictedNames ^. #terms) && null (conflictedNames ^. #types)
          then do
            let typecheck = wundefined

                namelookup :: Merge.RefToName = wundefined

            uf <- do
              let combinedUpdates :: Merge.UpdatesRefnt
                  combinedUpdates =
                    -- These left-biased unions are fine; at this point we know Alice's and Bob's updates
                    Merge.Defns
                      { terms = Map.union (updates ^. #alice . #terms) (updates ^. #bob . #terms),
                        types = Map.union (updates ^. #alice . #types) (updates ^. #bob . #types)
                      }
              Merge.computeUnisonFile namelookup loadTerm loadDecl loadDeclType whatToTypecheck combinedUpdates

            typecheck uf >>= \case
              Just tuf@(TypecheckedUnisonFileId {}) -> do
                let saveToCodebase = wundefined
                let consAndSaveNamespace = wundefined
                saveToCodebase tuf
                consAndSaveNamespace tuf
                pure MergeDone
              Nothing -> do
                let ppe :: PrettyPrintEnvDecl = wundefined
                pure $ MergePropagationNotTypecheck ppe (void uf)
          else do
            conflicts <- filterConflicts defns conflictedNames & onLeft (rollback . Left)
            dependents <- collectDependentsOfInterest defns updates
            let conflicted = conflicts <> dependents
            let unconflicted = filterUnconflicted defns updates conflicted

            Sqlite.unsafeIO do
              Text.putStrLn ""
              Text.putStrLn "===== alice conflicted ====="
              printConflicted (defns ^. #alice) (conflicted ^. #alice)

            Sqlite.unsafeIO do
              Text.putStrLn ""
              Text.putStrLn "===== bob conflicted ====="
              printConflicted (defns ^. #bob) (conflicted ^. #bob)

            Sqlite.unsafeIO do
              Text.putStrLn ""
              Text.putStrLn "===== alice unconflicted ====="
              printNamespace (unconflicted ^. #alice)
              Text.putStrLn ""
              Text.putStrLn "===== bob unconflicted ====="
              printNamespace (unconflicted ^. #bob)

            let branchV3 = unconflictedToBranchV3 defns causalHashes
            branchV1 <- loadV3BranchAsV1Branch0 loadDeclType (Codebase.expectBranchForHash codebase) branchV3

            -- TODO the rest

            -- that's a lie
            pure MergeDone

      Sqlite.unsafeIO do
        Text.putStrLn ""
        Text.putStrLn "===== lca->alice diff ====="
        printTypesDiff (aliceDefns ^. #types) (diffs ^. #alice . #types)
        printTermsDiff (aliceDefns ^. #terms) (diffs ^. #alice . #terms)
        Text.putStrLn ""
        Text.putStrLn "===== lca->bob diff ====="
        printTypesDiff (bobDefns ^. #types) (diffs ^. #bob . #types)
        printTermsDiff (bobDefns ^. #terms) (diffs ^. #bob . #terms)
        Text.putStrLn ""
        Text.putStrLn "===== merged libdeps dependencies ====="
        printLibdeps mergedLibdeps
        Text.putStrLn ""
        Text.putStrLn "===== conflicts ====="
        printTypeConflicts (conflictedNames ^. #types)
        printTermConflicts (conflictedNames ^. #terms)
        Text.putStrLn ""

      pure (Right mergeResult)

  do
    scratchFile <-
      Cli.getLatestFile >>= \case
        Just (scratchFile, _) -> pure scratchFile
        Nothing -> pure "merge.u"

    case result of
      Left err -> liftIO (print err)
      Right mergeResult -> case mergeResult of
        MergePropagationNotTypecheck ppe uf -> do
          Cli.respond $ Output.OutputMergeScratchFile ppe scratchFile (void uf)
        MergeConflicts ppe mergeOutput -> do
          (scratchFile, _) <- Cli.expectLatestFile
          Cli.respond $ Output.OutputMergeConflictScratchFile ppe scratchFile (void mergeOutput)
        MergeDone -> Cli.respond Output.Success

-- TODO document this
collectDependentsOfInterest ::
  Merge.TwoWay (Merge.Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
  Merge.TwoWay (Merge.Defns (Map Name Referent) (Map Name TypeReference)) ->
  Transaction (Merge.TwoWay (Merge.Defns (Set TermReferenceId) (Set TypeReferenceId)))
collectDependentsOfInterest defns updates = do
  alice <- getDependents (defns ^. #alice) (updates ^. #bob)
  bob <- getDependents (defns ^. #bob) (updates ^. #alice)
  pure Merge.TwoWay {alice, bob}
  where
    getDependents ::
      Merge.Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
      Merge.Defns (Map Name Referent) (Map Name TypeReference) ->
      Transaction (Merge.Defns (Set TermReferenceId) (Set TypeReferenceId))
    getDependents defns updates =
      -- The `dependentsWithinScope` query hands back a `Map Reference.Id ReferenceType`, but we would rather
      -- have two different maps, so we twiddle.
      fmap (Map.foldlWithKey' f (Merge.Defns Set.empty Set.empty)) do
        Operations.dependentsWithinScope
          (defnsToScope defns)
          (Set.union wawaTerms wawaTypes)
      where
        f ::
          Merge.Defns (Set TermReferenceId) (Set TypeReferenceId) ->
          Reference.Id ->
          ReferenceType ->
          Merge.Defns (Set TermReferenceId) (Set TypeReferenceId)
        f acc ref = \case
          Reference.RtTerm -> acc & over #terms (Set.insert ref)
          Reference.RtType -> acc & over #types (Set.insert ref)

        Merge.Defns wawaTerms wawaTypes = makeWawa2 defns updates

-- TODO document, rename
makeWawa2 ::
  Merge.Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
  Merge.Defns (Map Name Referent) (Map Name TypeReference) ->
  Merge.Defns (Set TermReference) (Set TypeReference)
makeWawa2 personOneDefns personTwoUpdates =
  let personOneDefnsUpdatedByPersonTwo :: Merge.Defns (Map Name Referent) (Map Name TypeReference)
      personOneDefnsUpdatedByPersonTwo =
        personOneDefns
          & over #terms ((`Map.intersection` (personTwoUpdates ^. #terms)) . BiMultimap.range)
          & over #types ((`Map.intersection` (personTwoUpdates ^. #types)) . BiMultimap.range)

      personOneTypeRefsUpdatedByPersonTwo :: Set TypeReference
      personOneTypeRefsUpdatedByPersonTwo =
        Set.fromList (Map.elems (personOneDefnsUpdatedByPersonTwo ^. #types))
   in (personOneDefnsUpdatedByPersonTwo ^. #terms)
        & termToReference
        & over #types (Set.union personOneTypeRefsUpdatedByPersonTwo)
  where
    -- Turn each referent into a reference:
    --
    --   1. For constructors, just ignore the constructor id and use the type reference.
    --   2. For terms, use that term reference.
    termToReference :: Map Name Referent -> Merge.Defns (Set TermReference) (Set TypeReference)
    termToReference =
      Map.foldl' f (Merge.Defns Set.empty Set.empty)
      where
        f ::
          Merge.Defns (Set TermReference) (Set TypeReference) ->
          Referent ->
          Merge.Defns (Set TermReference) (Set TypeReference)
        f acc = \case
          Referent.Con typeRef _conId -> acc & over #types (Set.insert typeRef)
          Referent.Ref termRef -> acc & over #terms (Set.insert termRef)

namespaceToBranchV3 ::
  Merge.NamespaceTree (Merge.Defns (Map NameSegment Referent) (Map NameSegment TypeReference), [CausalHash]) ->
  BranchV3 Transaction
namespaceToBranchV3 ((Merge.Defns {terms, types}, _causalParents) :< children) =
  BranchV3.BranchV3
    { terms,
      types,
      children = namespaceToCausalV3 <$> children
    }

namespaceToCausalV3 ::
  Merge.NamespaceTree (Merge.Defns (Map NameSegment Referent) (Map NameSegment TypeReference), [CausalHash]) ->
  BranchV3.CausalBranchV3 Transaction
namespaceToCausalV3 namespace@((_, causalParentHashes) :< _) =
  HashHandle.mkCausal
    v2HashHandle
    (HashHandle.hashBranchV3 v2HashHandle branchV3)
    (Map.fromList (map (\ch -> (ch, Operations.expectCausalBranchByCausalHash ch)) causalParentHashes))
    (pure branchV3)
  where
    branchV3 :: BranchV3 Transaction
    branchV3 =
      namespaceToBranchV3 namespace

filterUpdates ::
  Merge.TwoWay (Merge.Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
  Merge.TwoWay (Merge.Defns (Map Name (Merge.DiffOp Hash)) (Map Name (Merge.DiffOp Hash))) ->
  Merge.TwoWay (Merge.Defns (Map Name Referent) (Map Name TypeReference))
filterUpdates defns diff =
  Merge.TwoWay
    { alice = filterUpdates1 (defns ^. #alice) (diff ^. #alice),
      bob = filterUpdates1 (defns ^. #bob) (diff ^. #bob)
    }

-- `filterUpdates1 defns diff` returns the subset of `defns` that corresponds to updates (according to `diff`).
filterUpdates1 ::
  Merge.Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
  Merge.Defns (Map Name (Merge.DiffOp Hash)) (Map Name (Merge.DiffOp Hash)) ->
  Merge.Defns (Map Name Referent) (Map Name TypeReference)
filterUpdates1 defns diff =
  defns
    & over #terms ((`Map.intersection` (Map.filter isUpdate (diff ^. #terms))) . BiMultimap.range)
    & over #types ((`Map.intersection` (Map.filter isUpdate (diff ^. #types))) . BiMultimap.range)
  where
    isUpdate :: Merge.DiffOp Hash -> Bool
    isUpdate = \case
      Merge.Added {} -> False
      Merge.Deleted {} -> False
      Merge.Updated {} -> True

filterConflicts ::
  Merge.TwoWay (Merge.Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
  Merge.Defns (Set Name) (Set Name) ->
  Either MergePreconditionViolation (Merge.TwoWay (Merge.Defns (Set TermReferenceId) (Set TypeReferenceId)))
filterConflicts defns conflictedNames = do
  alice <- filterConflicts1 (defns ^. #alice) conflictedNames
  bob <- filterConflicts1 (defns ^. #bob) conflictedNames
  pure Merge.TwoWay {alice, bob}

-- `filterConflicts1 defns conflicts` filters `defns` down to just the conflicted type and term references.
--
-- It fails if it any conflict involving a builtin is discovered, since we can't handle those yet.
filterConflicts1 ::
  Merge.Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
  Merge.Defns (Set Name) (Set Name) ->
  Either MergePreconditionViolation (Merge.Defns (Set TermReferenceId) (Set TypeReferenceId))
filterConflicts1 defns conflicts = do
  terms <- foldlM doTerm Set.empty (onlyConflicted (conflicts ^. #terms) (defns ^. #terms))
  types <- foldlM doType Set.empty (onlyConflicted (conflicts ^. #types) (defns ^. #types))
  pure Merge.Defns {terms, types}
  where
    onlyConflicted :: Ord ref => Set Name -> BiMultimap ref Name -> Set ref
    onlyConflicted keys =
      Set.fromList . Map.elems . (`Map.restrictKeys` keys) . BiMultimap.range

    doTerm :: Set TermReferenceId -> Referent -> Either MergePreconditionViolation (Set TermReferenceId)
    doTerm refs = \case
      Referent.Con {} -> Right refs
      Referent.Ref (ReferenceBuiltin _) -> Left ConflictInvolvingBuiltin
      Referent.Ref (ReferenceDerived ref) -> Right $! Set.insert ref refs

    doType :: Set TypeReferenceId -> TypeReference -> Either MergePreconditionViolation (Set TypeReferenceId)
    doType refs = \case
      ReferenceBuiltin _ -> Left ConflictInvolvingBuiltin
      ReferenceDerived ref -> Right $! Set.insert ref refs

filterUnconflicted ::
  Merge.TwoWay (Merge.Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
  Merge.TwoWay (Merge.Defns (Map Name Referent) (Map Name TypeReference)) ->
  Merge.TwoWay (Merge.Defns (Set TermReferenceId) (Set TypeReferenceId)) ->
  Merge.TwoWay (Merge.Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name))
filterUnconflicted defns updates conflicted =
  Merge.TwoWay
    { alice = filterUnconflicted1 (defns ^. #alice) (conflicted ^. #alice) (updates ^. #bob),
      bob = filterUnconflicted1 (defns ^. #bob) (conflicted ^. #bob) (updates ^. #alice)
    }

-- `filterUnconflicted1 defns conflicted` returns the subset of `defns` that are not in `conflicted`.
-- TODO update comment
filterUnconflicted1 ::
  Merge.Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
  Merge.Defns (Set TermReferenceId) (Set TypeReferenceId) ->
  Merge.Defns (Map Name Referent) (Map Name TypeReference) ->
  Merge.Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)
filterUnconflicted1 personOneDefns personOneConflicted personTwoUpdates =
  personOneDefns
    & over #terms filterUnconflictedTerms
    & over #types filterUnconflictedTypes
  where
    filterUnconflictedTerms :: BiMultimap Referent Name -> BiMultimap Referent Name
    filterUnconflictedTerms =
      BiMultimap.filter g . BiMultimap.filterDom f
      where
        f :: Referent -> Bool
        f = \case
          -- Consider a constructor term "unconflicted" if its decl is unconflicted.
          Referent.Con (ReferenceDerived typeRef) _conId -> not (Set.member typeRef (personOneConflicted ^. #types))
          -- Keep builtin terms (since they can't be conflicted, per a precondition)
          Referent.Ref (ReferenceDerived termRef) -> not (Set.member termRef (personOneConflicted ^. #terms))
          -- Keep builtin constructors (which don't even exist) and builtin terms (since they can't be
          -- conflicted, per a precondition)
          Referent.Con (ReferenceBuiltin _) _ -> True
          Referent.Ref (ReferenceBuiltin _) -> True

        g :: Referent -> Name -> Bool
        g ref name =
          case ref of
            Referent.Con typeRef _conId ->
              Set.disjoint (BiMultimap.lookupDom typeRef personTwoUpdatedTypes) (possibleDeclNames name)
            Referent.Ref _termRef -> not (Map.member name (personTwoUpdates ^. #terms))

    personTwoUpdatedTypes :: BiMultimap TypeReference Name
    personTwoUpdatedTypes =
      BiMultimap.fromRange (personTwoUpdates ^. #types)

    filterUnconflictedTypes =
      BiMultimap.withoutDom
        (Set.map ReferenceDerived (personOneConflicted ^. #types))

unconflictedToBranchV3 ::
  Merge.TwoWay (Merge.Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
  Merge.TwoWay (Merge.NamespaceTree CausalHash) ->
  BranchV3 Transaction
unconflictedToBranchV3 unconflicted causalHashes =
  namespaceToBranchV3 $
    Merge.mergeNamespaceTrees
      (\(aliceDefns, aliceCausal) -> (aliceDefns, [aliceCausal]))
      (\(bobDefns, bobCausal) -> (bobDefns, [bobCausal]))
      ( \(aliceDefns, aliceCausal) (bobDefns, bobCausal) ->
          -- A left-biased union is fine here because we are merging unconflicted things; where the maps aren't
          -- disjoint, the values are equal
          (aliceDefns <> bobDefns, [aliceCausal, bobCausal])
      )
      (makeBigTree (unconflicted ^. #alice) (causalHashes ^. #alice))
      (makeBigTree (unconflicted ^. #bob) (causalHashes ^. #bob))
  where
    makeBigTree ::
      Merge.Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
      Merge.NamespaceTree CausalHash ->
      Merge.NamespaceTree (Merge.Defns (Map NameSegment Referent) (Map NameSegment TypeReference), CausalHash)
    makeBigTree defns causals =
      Merge.mergeNamespaceTrees
        (\_ -> error "impossible")
        (\_ -> error "impossible")
        (,)
        (Merge.unflattenNamespaceTree defns)
        causals

-- "hey.Maybe.internal.Just" -> {"hey.Maybe.internal", "hey.Maybe", "hey"}
possibleDeclNames :: Name -> Set Name
possibleDeclNames =
  Name.reverseSegments
    -- "Just" :| ["internal", "Maybe", "hey"]
    >>> List.NonEmpty.toList
    -- ["Just", "internal", "Maybe", "hey"]
    >>> List.tails
    -- [["Just", "internal", "Maybe", "hey"], ["internal", "Maybe", "hey"], ["Maybe", "hey"], ["hey"], []]
    >>> drop 1
    -- [["internal", "Maybe", "hey"], ["Maybe", "hey"], ["hey"], []]
    >>> mapMaybe List.NonEmpty.nonEmpty
    -- ["internal" :| ["Maybe", "hey"], "Maybe" :| ["hey"], "hey" :| []]
    >>> map (Name.fromReverseSegments)
    -- ["hey.Maybe.internal", "hey.Maybe", "hey"]
    >>> Set.fromList

-- `defnsToScope defns` converts a flattened namespace `defns` to the set of untagged reference ids contained within,
-- for the purpose of searching for transitive dependents of conflicts that are contained in that set.
defnsToScope :: Merge.Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) -> Set Reference.Id
defnsToScope (Merge.Defns terms types) =
  Set.union
    (Set.mapMaybe Referent.toReferenceId (BiMultimap.dom terms))
    (Set.mapMaybe Reference.toId (BiMultimap.dom types))

loadV3BranchAsV1Branch0 ::
  Monad m =>
  (TypeReference -> m ConstructorType) ->
  (CausalHash -> m (V1.Branch m)) ->
  BranchV3 m ->
  m (V1.Branch0 m)
loadV3BranchAsV1Branch0 loadDeclType loadBranch BranchV3 {terms, types, children} = do
  terms1 <- traverse (referent2to1 loadDeclType) terms
  children1 <- traverse (loadV3CausalAsV1Branch loadDeclType loadBranch) children
  pure $
    V1.Branch.branch0
      (makeStar3 terms1)
      (makeStar3 types)
      children1
      Map.empty
  where
    makeStar3 :: Ord ref => Map NameSegment ref -> Star3 ref NameSegment x y
    makeStar3 =
      foldr (\(name, ref) -> Star3.insertD1 (ref, name)) emptyStar3 . Map.toList
      where
        emptyStar3 =
          Star3.Star3 Set.empty Relation.empty Relation.empty Relation.empty

loadV3CausalAsV1Branch ::
  forall m.
  Monad m =>
  (TypeReference -> m ConstructorType) ->
  (CausalHash -> m (V1.Branch m)) ->
  CausalBranchV3 m ->
  m (V1.Branch m)
loadV3CausalAsV1Branch loadDeclType loadBranch causal = do
  branch <- causal ^. #value
  head <- loadV3BranchAsV1Branch0 loadDeclType loadBranch branch
  let currentHash = causal ^. #causalHash
  let valueHash = coerce @BranchHash @(Hash.HashFor (V1.Branch0 m)) (causal ^. #valueHash)
  pure $
    V1.Branch.Branch
      case Map.toList (causal ^. #parents) of
        [] -> V1.Causal.UnsafeOne {currentHash, valueHash, head}
        [(parentHash, parent)] ->
          V1.Causal.UnsafeCons
            { currentHash,
              valueHash,
              head,
              tail = (parentHash, convertParent parent)
            }
        _ ->
          V1.Causal.UnsafeMerge
            { currentHash,
              valueHash,
              head,
              tails = convertParent <$> (causal ^. #parents)
            }
  where
    convertParent :: m (CausalBranch m) -> m (V1.Causal m (V1.Branch0 m))
    convertParent loadParent = do
      parent <- loadParent
      v1Branch <- loadBranch (parent ^. #causalHash)
      pure (V1.Branch._history v1Branch)

-- Convert a v2 referent (missing decl type) to a v1 referent using the provided lookup-decl-type function.
referent2to1 :: Applicative m => (TypeReference -> m ConstructorType) -> Referent -> m V1.Referent
referent2to1 loadDeclType = \case
  Referent.Con typeRef conId -> do
    declTy <- loadDeclType typeRef
    pure (V1.Referent.Con (ConstructorReference typeRef conId) declTy)
  Referent.Ref termRef -> pure (V1.Referent.Ref termRef)

-- Given a name like "base", try "base__1", then "base__2", etc, until we find a name that doesn't
-- clash with any existing dependencies.
getTwoFreshNames :: Set NameSegment -> NameSegment -> (NameSegment, NameSegment)
getTwoFreshNames names name0 =
  go2 0
  where
    -- if
    --   name0 = "base"
    --   names = {"base__5", "base__6"}
    -- then
    --   go2 4 = ("base__4", "base__7")
    go2 :: Integer -> (NameSegment, NameSegment)
    go2 !i
      | Set.member name names = go2 (i + 1)
      | otherwise = (name, go1 (i + 1))
      where
        name = mangled i

    -- if
    --   name0 = "base"
    --   names = {"base__5", "base__6"}
    -- then
    --   go1 5 = "base__7"
    go1 :: Integer -> NameSegment
    go1 !i
      | Set.member name names = go1 (i + 1)
      | otherwise = name
      where
        name = mangled i

    mangled :: Integer -> NameSegment
    mangled i =
      NameSegment (NameSegment.toText name0 <> "__" <> tShow i)

-- | Load all term and type names from a branch (excluding dependencies) into memory.
--
-- Fails if:
--   * The "lib" namespace contains any top-level terms or decls. (Only child namespaces are expected here).
--   * One name is associated with more than one reference.
--   * Any type declarations are "incoherent" (see `checkDeclCoherency`)
loadNamespaceDefns ::
  Monad m =>
  (TypeReferenceId -> m Int) ->
  Branch m ->
  CausalHash ->
  m
    ( Either
        MergePreconditionViolation
        ( Merge.Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name),
          Merge.NamespaceTree CausalHash
        )
    )
loadNamespaceDefns loadNumConstructors branch causalHash = do
  libdepsHasTopLevelDefns <-
    case Map.lookup Name.libSegment (branch ^. #children) of
      Nothing -> pure False
      Just libdepsCausal -> do
        libdepsBranch <- Causal.value libdepsCausal
        pure (not (Map.null (libdepsBranch ^. #terms)) || not (Map.null (libdepsBranch ^. #types)))
  if libdepsHasTopLevelDefns
    then pure (Left DefnsInLib)
    else do
      defns0 <- loadNamespaceDefns0 branch causalHash
      case makeNamespaceDefns1 defns0 of
        Left err -> pure (Left err)
        Right defns1 ->
          checkDeclCoherency loadNumConstructors defns1 <&> \case
            Left err -> Left err
            Right () -> Right (Merge.flattenNamespaceTree (fmap fst defns1), fmap snd defns1)

type NamespaceDefns0 =
  Merge.NamespaceTree (Merge.Defns (Map NameSegment (Set Referent)) (Map NameSegment (Set TypeReference)), CausalHash)

-- | Load all "namespace definitions" of a branch, which are all terms and type declarations *except* those defined
-- in the "lib" namespace.
loadNamespaceDefns0 :: forall m. Monad m => Branch m -> CausalHash -> m NamespaceDefns0
loadNamespaceDefns0 branch causalHash = do
  let terms = Map.map Map.keysSet (branch ^. #terms)
  let types = Map.map Map.keysSet (branch ^. #types)
  children <-
    for (Map.delete Name.libSegment (branch ^. #children)) \childCausal -> do
      childBranch <- Causal.value childCausal
      loadNamespaceDefns0_ childBranch (childCausal ^. #causalHash)
  pure ((Merge.Defns {terms, types}, causalHash) :< children)

loadNamespaceDefns0_ :: forall m. Monad m => Branch m -> CausalHash -> m NamespaceDefns0
loadNamespaceDefns0_ branch causalHash = do
  let terms = Map.map Map.keysSet (branch ^. #terms)
  let types = Map.map Map.keysSet (branch ^. #types)
  children <-
    for (branch ^. #children) \childCausal -> do
      childBranch <- Causal.value childCausal
      loadNamespaceDefns0_ childBranch (childCausal ^. #causalHash)
  pure ((Merge.Defns {terms, types}, causalHash) :< children)

type NamespaceDefns1 =
  Merge.NamespaceTree (Merge.Defns (Map NameSegment Referent) (Map NameSegment TypeReference), CausalHash)

-- | Assert that there are no unconflicted names in a namespace.
makeNamespaceDefns1 :: NamespaceDefns0 -> Either MergePreconditionViolation NamespaceDefns1
makeNamespaceDefns1 =
  traverse \(Merge.Defns {terms, types}, causalHash) -> do
    terms <- traverse (assertUnconflicted ConflictedTermName) terms
    types <- traverse (assertUnconflicted ConflictedTypeName) types
    pure (Merge.Defns terms types, causalHash)
  where
    assertUnconflicted :: (Set ref -> MergePreconditionViolation) -> Set ref -> Either MergePreconditionViolation ref
    assertUnconflicted conflicted refs =
      case Set.asSingleton refs of
        Nothing -> Left (conflicted refs)
        Just ref -> Right ref

-- The "decl coherency check": a type declaration in a namespace is "coherent" if it satisfies both of the following
-- criteria.
--
--   1. For each naming of the type decl (say "Foo"#foohash), there exists exactly one name for each of its constructors
--      arbitrarily deep in the corresponding namespace ("Foo" in this example).
--
--      This allows us to render the decl naturally, as in
--
--        structural type Foo
--          = Bar Nat Int
--          | internal.hello.Bonk Nat
--
--      which corresponds to the three names
--
--        "Foo"                     => #foohash
--        "Foo.Bar"                 => #foohash#0
--        "Foo.internal.hello.Bonk" => #foohash#1
--
--      We could not do if there was at least one constructor whose full name does not contain the full name of the type
--      decl itself as a prefix.
--
--      A notable consequence of this requirement is that a second naming of a decl (i.e. an alias) cannot be embedded
--      within the first naming, as in:
--
--        type Foo = ...
--        type Foo.some.inner.namespace = ... -- an alias of Foo
--
--   2. No constructor has a "stray" name that does not have a prefix that equals the type declaration's name. For
--      example, in the namespace
--
--        "Foo"                 => #foohash
--        "Foo.Bar"             => #foohash#0
--        "Deep.What.SomeAlias" => #foohash#0
--
--      the constructor "What.SomeAlias" is "stray", as the type decl #foohash has no name that matches any prefix
--      (i.e. "Deep.What" nor "Deep").
--
-- On to the implementation. We are going to traverse the namespace depth-first. As we go, we have a stateful mapping
-- between decl reference that we *have* seen a name for in one of our parent namespace, and its corresponding set of
-- constructors that we *haven't* yet seen names for, but expect to, before fully searching the corresponding
-- sub-namespace (e.g. the child namespace named "Foo" of the namepace that declares a decl "Foo").
--
-- When processing a namespace, we first process all terms. Each constructor will fall into one of three cases:
--
--   +----------------------------------------------------------------------------------------------------------------+
--   | Case         | Mapping before       | Encountered constructor | Mapping after                                  |
--   +----------------------------------------------------------------------------------------------------------------+
--   | Happy path   | { #foo : {0, 1, 2} } | #foo#1                  | { #foo : {0, 2} }                              |
--   | Already seen | { #foo : {0, 1, 2} } | #foo#5                  | Error: duplicate naming for constructor #foo#5 |
--   | Never seen   | { #foo : {0, 1, 2} } | #bar#2                  | Error: stray constructor #bar#2                |
--   +----------------------------------------------------------------------------------------------------------------+
--
-- In "happy path", we see a naming of a constructor that we're expecting, and check it off.
-- In "already seen", we see a second naming of a constructor that we're no longer expecting, and fail.
-- In "never seen", we see a naming of a constructor before any naming of its decl, so we fail.
--
-- Next, we process all type decls. Each will again fall into one of three cases:
--
--   +-----------------------------------------------------------------------------------------------------+
--   | Case             | Mapping before       | Declaration | Num constructors | New mapping              |
--   +-----------------------------------------------------------------------------------------------------+
--   | Uninhabited decl |                      | #foo        | 0                |                          |
--   | Inhabited decl   |                      | #foo        | 1 or more        | { #foo : {0, ..., n-1} } |
--   | Already seen     | { foo : {0, 1, 2}  } | #foo        | Irrelevant       | Error: nested decl alias |
--   +-----------------------------------------------------------------------------------------------------+
--
-- In "uninhabited decl", we find a decl with no constructors, so we don't expect anything new.
-- In "already seen", we find a second naming of a decl, whose constructors will necessarily violate coherency condition
--   (1) above.
--
-- In "inhabited decl", we find a decl with N constructors, and handle it by:
--   1. Adding to our state that we expect a name for each.
--   2. Recursing into the child namespace whose name matches the decl.
--   3. (If we return from the recursion without short-circuiting) remove the mapping added in step (1) and assert that
--      its value is the empty set (meaning we encountered a name for every constructor).
--
-- Note: This check could be moved into SQLite (with sufficient schema support) some day, but for now, because the merge
-- algorithm needs to pull lots of stuff into memory anyway, we just do this in memory, too.
--
-- Note: once upon a time, decls could be "incoherent". Then, we decided we want decls to be "coherent". Thus, this
-- machinery was invented.
checkDeclCoherency ::
  forall m.
  Monad m =>
  (TypeReferenceId -> m Int) ->
  NamespaceDefns1 ->
  m (Either MergePreconditionViolation ())
checkDeclCoherency loadNumConstructors =
  runExceptT . (`State.evalStateT` Map.empty) . go []
  where
    go :: [NameSegment] -> NamespaceDefns1 -> StateT (Map TypeReferenceId IntSet) (ExceptT MergePreconditionViolation m) ()
    go prefix ((Merge.Defns {terms, types}, _) :< children) = do
      for_ (Map.toList terms) \case
        (_, Referent.Ref _) -> pure ()
        (_, Referent.Con (ReferenceBuiltin _) _) -> pure ()
        (name, Referent.Con (ReferenceDerived typeRef) conId) -> do
          -- could use modifyM on newer tranformers
          s0 <- State.get
          s1 <- lift (Except.except (Map.upsertF f typeRef s0))
          State.put s1
          where
            f :: Maybe IntSet -> Either MergePreconditionViolation IntSet
            f = \case
              Nothing -> Left (StrayConstructor (fullName name))
              Just expected -> IntSet.alterF g (unsafeFrom @Word64 conId) expected
                where
                  g :: Bool -> Either MergePreconditionViolation Bool
                  g = \case
                    False -> Left (ConstructorAlias (fullName name))
                    True -> Right False

      childrenWeWentInto <-
        forMaybe (Map.toList types) \case
          (_, ReferenceBuiltin _) -> pure Nothing
          (name, ReferenceDerived typeRef) -> do
            s0 <- State.get
            whatHappened <- do
              let recordNewDecl :: Maybe IntSet -> Compose (ExceptT MergePreconditionViolation m) WhatHappened IntSet
                  recordNewDecl =
                    Compose . \case
                      Just _ -> Except.throwError (NestedDeclAlias (fullName name))
                      Nothing ->
                        lift (loadNumConstructors typeRef) <&> \case
                          0 -> UninhabitedDecl
                          n -> InhabitedDecl (IntSet.fromAscList [0 .. n - 1])
              lift (getCompose (Map.upsertF recordNewDecl typeRef s0))
            case whatHappened of
              UninhabitedDecl -> pure Nothing
              InhabitedDecl s1 ->
                case Map.lookup name children of
                  Nothing -> Except.throwError (NoConstructorNames (fullName name))
                  Just child -> do
                    State.put s1
                    go (name : prefix) child
                    s2 <- State.get
                    -- fromJust is safe here because we upserted `typeRef` key above
                    let (fromJust -> constructorIdsWithoutNames, s3) = Map.deleteLookup typeRef s2
                    when (not (IntSet.null constructorIdsWithoutNames)) do
                      Except.throwError (MissingConstructorName (fullName name))
                    State.put s3
                    pure (Just name)

      let childrenWeHaventGoneInto = children `Map.withoutKeys` Set.fromList childrenWeWentInto
      for_ (Map.toList childrenWeHaventGoneInto) \(name, child) -> go (name : prefix) child
      where
        fullName name =
          Name.fromReverseSegments (name :| prefix)

data WhatHappened a
  = UninhabitedDecl
  | InhabitedDecl !a
  deriving stock (Functor, Show)

-- @findConflictedAlias namespace diff@, given a namespace and a diff from an old namespace, will return the first
-- "conflicted alias" encountered (if any), where a "conflicted alias" is a pair of names that referred to the same
-- thing in the old namespace, but different things in the new one.
--
-- For example, if the old namespace was
--
--   foo = #foo
--   bar = #foo
--
-- and the new namespace is
--
--   foo = #baz
--   bar = #qux
--
-- then (foo, bar) is a conflicted alias.
--
-- This function currently doesn't return whether the conflicted alias is a decl or a term, but it could.
findConflictedAlias ::
  Merge.Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
  Merge.Defns (Map Name (Merge.DiffOp Hash)) (Map Name (Merge.DiffOp Hash)) ->
  Maybe (Name, Name)
findConflictedAlias aliceDefns aliceDiff =
  asum
    [ go (aliceDefns ^. #terms) (aliceDiff ^. #terms),
      go (aliceDefns ^. #types) (aliceDiff ^. #types)
    ]
  where
    go ::
      forall ref.
      Ord ref =>
      BiMultimap ref Name ->
      Map Name (Merge.DiffOp Hash) ->
      Maybe (Name, Name)
    go namespace diff =
      asum (map f (Map.toList diff))
      where
        f :: (Name, Merge.DiffOp Hash) -> Maybe (Name, Name)
        f (name, op) =
          case op of
            Merge.Added _ -> Nothing
            Merge.Deleted _ -> Nothing
            Merge.Updated _ hash ->
              BiMultimap.lookupPreimage name namespace
                & Set.delete name
                & Set.toList
                & map (g hash)
                & asum
          where
            g :: Hash -> Name -> Maybe (Name, Name)
            g hash alias =
              case Map.lookup alias diff of
                Just (Merge.Updated _ hash2) | hash == hash2 -> Nothing
                _ -> Just (name, alias)

-- conflictsish(diffish(lca, alice), diffish(lca, bob))
conflictsish :: forall hash name. (Eq hash, Ord name) => Map name (Merge.DiffOp hash) -> Map name (Merge.DiffOp hash) -> Set name
conflictsish aliceDiff bobDiff =
  Map.keysSet (Map.mapMaybe id (alignWith f aliceDiff bobDiff))
  where
    f :: These (Merge.DiffOp hash) (Merge.DiffOp hash) -> Maybe ()
    f = \case
      These (Merge.Added x) (Merge.Added y) | x /= y -> Just ()
      These (Merge.Updated _ x) (Merge.Updated _ y) | x /= y -> Just ()
      -- Not a conflict:
      --   delete/delete
      -- Not a conflict, perhaps only temporarily, because it's easier to implement (we ignore these deletes):
      --   delete/update
      --   update/delete
      -- Impossible cases:
      --   add/delete
      --   add/update
      _ -> Nothing

-- | Load the library dependencies (lib.*) of a namespace.
loadLibdeps :: Branch Transaction -> Transaction (Map NameSegment (CausalBranch Transaction))
loadLibdeps branch =
  case Map.lookup Name.libSegment (Branch.children branch) of
    Nothing -> pure Map.empty
    Just dependenciesCausal -> Branch.children <$> Causal.value dependenciesCausal

-----------------------------------------------------------------------------------------------------------------------
-- Debug show/print utils

showCausal :: CausalBranch m -> Text
showCausal =
  showCausalHash . Causal.causalHash

showCausalHash :: CausalHash -> Text
showCausalHash =
  ("#" <>) . Text.take 4 . Hash.toBase32HexText . unCausalHash

showNamedReference :: Name -> Reference -> Text
showNamedReference name ref =
  Name.toText name <> showReference ref

showNamedReferent :: Name -> Referent -> Text
showNamedReferent name ref =
  Name.toText name <> showReferent ref

showNamespaceHash :: BranchHash -> Text
showNamespaceHash =
  ("#" <>) . Text.take 4 . Hash.toBase32HexText . unBranchHash

showReference :: Reference -> Text
showReference =
  showShortHash . Reference.toShortHash

showReferent :: Referent -> Text
showReferent =
  showShortHash . Referent.toShortHash

showShortHash :: ShortHash -> Text
showShortHash =
  ShortHash.toText . ShortHash.shortenTo 4

printConflicted ::
  Merge.Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
  Merge.Defns (Set TermReferenceId) (Set TypeReferenceId) ->
  IO ()
printConflicted (Merge.Defns terms types) (Merge.Defns conflictedTermRefs conflictedTypeRefs) =
  printNamespace (Merge.Defns conflictedTerms conflictedTypes)
  where
    conflictedTerms = BiMultimap.restrictDom (Set.map (Referent.Ref . ReferenceDerived) conflictedTermRefs) terms
    conflictedTypes = BiMultimap.restrictDom (Set.map ReferenceDerived conflictedTypeRefs) types

printNamespace :: Merge.Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) -> IO ()
printNamespace (Merge.Defns terms types) =
  Text.putStr . Text.unlines $
    map (\(name, ref) -> "term " <> showNamedReferent name ref) (Map.toList (BiMultimap.range terms))
      ++ map (\(name, ref) -> "type " <> showNamedReference name ref) (Map.toList (BiMultimap.range types))

printTypesDiff :: BiMultimap TypeReference Name -> Map Name (Merge.DiffOp Hash) -> IO ()
printTypesDiff declNames = do
  Text.putStr . Text.unlines . map f . Map.toList
  where
    f :: (Name, Merge.DiffOp Hash) -> Text
    f (name, op) =
      case op of
        Merge.Added _ -> Text.green ("decl " <> Name.toText name) <> ref
        Merge.Deleted _ -> Text.red ("decl " <> Name.toText name) <> ref
        Merge.Updated _ _ -> Text.magenta ("decl " <> Name.toText name) <> ref
      where
        ref =
          Text.brightBlack (showReference (fromJust (BiMultimap.lookupRan name declNames)))

printTermsDiff :: BiMultimap Referent Name -> Map Name (Merge.DiffOp Hash) -> IO ()
printTermsDiff termNames = do
  Text.putStr . Text.unlines . map f . Map.toList
  where
    f :: (Name, Merge.DiffOp Hash) -> Text
    f (name, op) =
      case op of
        Merge.Added _ -> Text.green ("term " <> Name.toText name) <> ref
        Merge.Deleted _ -> Text.red ("term " <> Name.toText name) <> ref
        Merge.Updated _ _ -> Text.magenta ("decl " <> Name.toText name) <> ref
      where
        ref =
          Text.brightBlack (showReferent (fromJust (BiMultimap.lookupRan name termNames)))

printLibdeps :: Map NameSegment (CausalBranch Transaction) -> IO ()
printLibdeps =
  Text.putStr . Text.unlines . map f . Map.toList
  where
    f (name, causal) =
      "dependency " <> NameSegment.toText name <> Text.brightBlack (showCausal causal)

printTypeConflicts :: Set Name -> IO ()
printTypeConflicts =
  Text.putStrLn . Text.unwords . map (("decl " <>) . Name.toText) . Set.toList

printTermConflicts :: Set Name -> IO ()
printTermConflicts =
  Text.putStrLn . Text.unwords . map (("term " <>) . Name.toText) . Set.toList

-----------------------------------------------------------------------------------------------------------------------
-- Utilities for caching transaction calls
--
-- These ought to be in a more general-puprose location, but defining here for now

cacheTransaction :: forall k v. Cache.Cache k v -> (k -> Transaction v) -> (k -> Transaction v)
cacheTransaction cache f k =
  unTransactionWithMonadIO (Cache.apply cache (TransactionWithMonadIO . f) k)

newtype TransactionWithMonadIO a
  = TransactionWithMonadIO (Transaction a)
  deriving newtype (Applicative, Functor, Monad)

unTransactionWithMonadIO :: TransactionWithMonadIO a -> Transaction a
unTransactionWithMonadIO (TransactionWithMonadIO m) = m

instance MonadIO TransactionWithMonadIO where
  liftIO :: forall a. IO a -> TransactionWithMonadIO a
  liftIO = coerce @(IO a -> Transaction a) Sqlite.unsafeIO
