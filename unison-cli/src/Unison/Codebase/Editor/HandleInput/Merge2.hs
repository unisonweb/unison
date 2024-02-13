module Unison.Codebase.Editor.HandleInput.Merge2
  ( handleMerge,
  )
where

import U.Codebase.Reference qualified as Reference
import Data.Foldable (foldlM)
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as NESet
import Control.Lens (Lens', over, view, (%=), (.=), (.~), (^.))
import Control.Monad.Except qualified as Except (throwError)
import Control.Monad.Reader (ask)
import Control.Monad.State.Strict (StateT)
import Control.Monad.State.Strict qualified as State
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans.Except qualified as Except
import Data.Function (on)
import Data.Functor.Compose (Compose (..))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List.NonEmpty (pattern (:|))
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromJust)
import Data.Semialign (alignWith, unzip, zip)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.These (These (..))
import GHC.Clock (getMonotonicTime)
import Text.Printf (printf)
import U.Codebase.Branch (Branch (..), CausalBranch)
import U.Codebase.Branch qualified as Branch
import U.Codebase.Causal qualified as Causal
import U.Codebase.HashTags (BranchHash (..), CausalHash (..))
import U.Codebase.Reference
  ( Reference,
    Reference' (..),
    ReferenceType,
    TermReference,
    TermReferenceId,
    TypeReference,
    TypeReferenceId,
  )
import U.Codebase.Referent (Referent)
import U.Codebase.Referent qualified as Referent
import U.Codebase.Sqlite.Operations qualified as Operations
import U.Codebase.Sqlite.ProjectBranch qualified as Sqlite (ProjectBranch)
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.ProjectUtils qualified as Cli
import Unison.Cli.TypeCheck (computeTypecheckingEnvironment, typecheckTerm)
import Unison.Cli.UniqueTypeGuidLookup qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Path qualified as Path
import Unison.Hash (Hash)
import Unison.Merge.Database (MergeDatabase (..), makeMergeDatabase, referent2to1)
import Unison.Merge.Diff qualified as Merge
import Unison.Merge.DiffOp qualified as Merge
import Unison.Merge.Libdeps qualified as Merge
import Unison.Merge.PreconditionViolation qualified as Merge
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment (..))
import Unison.NameSegment qualified as NameSegment
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchName)
import Unison.Referent qualified as V1 (Referent)
import Unison.Referent qualified as V1.Referent
import Unison.Sqlite (Transaction)
import Unison.Sqlite qualified as Sqlite
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Map qualified as Map
import Unison.Util.Nametree
  ( Defns (..),
    Nametree (..),
    bimapDefns,
    flattenNametree,
    traverseNametreeWithName,
    unflattenNametree,
  )
import Unison.Util.Set qualified as Set
import Prelude hiding (unzip, zip)

-- Temporary simple way to time a transaction
step :: Text -> Transaction a -> Transaction a
step name action = do
  t0 <- Sqlite.unsafeIO getMonotonicTime
  result <- action
  Sqlite.unsafeIO do
    t1 <- getMonotonicTime
    Text.putStrLn (Text.pack (printf "%4d ms | " (round ((t1 - t0) * 1000) :: Int)) <> name)
  pure result

handleMerge :: ProjectBranchName -> Cli ()
handleMerge bobBranchName = do
  -- Load the current project branch ("alice"), and the branch from the same project to merge in ("bob")
  (ProjectAndBranch project aliceProjectBranch, _path) <- Cli.expectCurrentProjectBranch
  bobProjectBranch <- Cli.expectProjectBranchByName project bobBranchName
  let projectBranches = Merge.TwoWay {alice = aliceProjectBranch, bob = bobProjectBranch}
  let alicePath = Cli.projectBranchPath (ProjectAndBranch (project ^. #projectId) (aliceProjectBranch ^. #branchId))
  let bobPath = Cli.projectBranchPath (ProjectAndBranch (project ^. #projectId) (bobProjectBranch ^. #branchId))

  -- Create a bunch of cached database lookup functions
  Cli.Env {codebase} <- ask
  db@MergeDatabase {loadCausal} <- makeMergeDatabase codebase
  _ <-
    Cli.runTransactionWithRollback \abort0 -> do
      -- Helper used throughout: abort this transaction with an output message.
      let abort :: Merge.PreconditionViolation -> Transaction void
          abort =
            mergePreconditionViolationToOutput db >=> abort0
      -- Load causals
      aliceCausal <- Codebase.getShallowCausalFromRoot Nothing (Path.unabsolute alicePath)
      bobCausal <- Codebase.getShallowCausalFromRoot Nothing (Path.unabsolute bobPath)
      maybeLcaCausal <-
        step "compute lca" (Operations.lca (Causal.causalHash aliceCausal) (Causal.causalHash bobCausal)) >>= \case
          Nothing -> pure Nothing
          Just lcaCausalHash -> do
            -- If LCA == bob, then we are at or ahead of bob, so the merge is done.
            when (lcaCausalHash == bobCausal ^. #causalHash) do
              abort0 $
                Output.MergeAlreadyUpToDate
                  (Right (ProjectAndBranch project bobProjectBranch))
                  (Right (ProjectAndBranch project aliceProjectBranch))
            Just <$> loadCausal lcaCausalHash
      -- Load shallow branches
      aliceBranch <- Causal.value aliceCausal
      bobBranch <- Causal.value bobCausal

      -- Load deep definitions
      (aliceCausalTree, aliceDeclNames, aliceDefns) <-
        step "load alice definitions" do
          (definitions0, causalHashes) <- unzip <$> loadNamespaceInfo abort (aliceCausal ^. #causalHash) aliceBranch
          (declNames, definitions1) <- assertNamespaceSatisfiesPreconditions db abort (projectBranches ^. #alice . #name) aliceBranch definitions0
          pure (causalHashes, declNames, definitions1)
      (bobCausalTree, bobDeclNames, bobDefns) <-
        step "load bob definitions" do
          (definitions0, causalHashes) <- unzip <$> loadNamespaceInfo abort (bobCausal ^. #causalHash) bobBranch
          (declNames, definitions1) <- assertNamespaceSatisfiesPreconditions db abort (projectBranches ^. #bob . #name) bobBranch definitions0
          pure (causalHashes, declNames, definitions1)
      let defns = Merge.TwoWay {alice = aliceDefns, bob = bobDefns}
      let causalHashes = Merge.TwoWay {alice = aliceCausalTree, bob = bobCausalTree}
      let declNames = Merge.TwoWay {alice = aliceDeclNames, bob = bobDeclNames}

      (lcaDefns, lcaLibdeps, diffs) <- do
        case maybeLcaCausal of
          Nothing -> do
            diffs <-
              Merge.nameBasedNamespaceDiff
                db
                Merge.TwoOrThreeWay {lca = Nothing, alice = aliceDefns, bob = bobDefns}
            pure (Defns BiMultimap.empty BiMultimap.empty, Map.empty, diffs)
          Just lcaCausal -> do
            lcaBranch <- Causal.value lcaCausal
            lcaDefns <- loadLcaDefinitions abort (lcaCausal ^. #causalHash) lcaBranch
            diffs <-
              Merge.nameBasedNamespaceDiff
                db
                Merge.TwoOrThreeWay {lca = Just lcaDefns, alice = aliceDefns, bob = bobDefns}
            abortIfAnyConflictedAliases abort projectBranches lcaDefns diffs
            lcaLibdeps <- maybe Map.empty snd <$> loadLibdeps lcaBranch
            pure (lcaDefns, lcaLibdeps, diffs)

      -- Load and merge libdeps
      (libdepsCausalParents, libdeps) <- do
        maybeAliceLibdeps <- loadLibdeps aliceBranch
        maybeBobLibdeps <- loadLibdeps bobBranch
        pure $
          ( Set.fromList (catMaybes [fst <$> maybeAliceLibdeps, fst <$> maybeBobLibdeps]),
            Merge.mergeLibdeps
              ((==) `on` Causal.causalHash)
              getTwoFreshNames
              lcaLibdeps
              (maybe Map.empty snd maybeAliceLibdeps)
              (maybe Map.empty snd maybeBobLibdeps)
          )

      let conflictedNames =
            Defns
              { terms = getConflicts (view #terms <$> diffs),
                types = getConflicts (view #types <$> diffs)
              }
      -- TODO is swapping constructors' names handled correctly here?
      -- TODO is exchanging constructor for function handled correctly here?
      -- TODO is exchanging function for constructor handled correctly here?
      conflicted <- filterConflicts conflictedNames defns & onLeft abort
      let updates0 = filterUpdates diffs defns
      let updates = bimapDefns BiMultimap.range BiMultimap.range <$> updates0
      let updatedNames = bimapDefns BiMultimap.ran BiMultimap.ran <$> updates0
      dependents <- collectDependentsOfInterest defns updatedNames
      let unconflicted = filterUnconflicted (BiMultimap.range <$> declNames) conflicted updates dependents defns
      let unconflicted' = filterUnconflicted' updatedNames (conflicted <> dependents) defns
      let unconflictedUpdates = filterUnconflictedUpdates lcaDefns updates0 unconflicted'
      case null (conflictedNames ^. #terms) && null (conflictedNames ^. #types) of
        True -> do
          -- no conflicts
          undefined
        False -> do
          -- conflicts
          undefined
  undefined

-- Load namespace info into memory.
--
-- Fails if:
--   * One name is associated with more than one reference.
loadNamespaceInfo ::
  (forall void. Merge.PreconditionViolation -> Transaction void) ->
  CausalHash ->
  Branch Transaction ->
  Transaction
    ( Nametree
        ( Defns (Map NameSegment Referent) (Map NameSegment TypeReference),
          CausalHash
        )
    )
loadNamespaceInfo abort causalHash branch = do
  defns <- loadNamespaceInfo0 branch causalHash
  assertNamespaceHasNoConflictedNames defns & onLeft abort

-- | Load all "namespace definitions" of a branch, which are all terms and type declarations *except* those defined
-- in the "lib" namespace.
loadNamespaceInfo0 ::
  Monad m =>
  Branch m ->
  CausalHash ->
  m
    ( Nametree
        ( Defns (Map NameSegment (Set Referent)) (Map NameSegment (Set TypeReference)),
          CausalHash
        )
    )
loadNamespaceInfo0 branch causalHash = do
  let terms = Map.map Map.keysSet (branch ^. #terms)
  let types = Map.map Map.keysSet (branch ^. #types)
  let value = (Defns {terms, types}, causalHash)
  children <-
    for (Map.delete Name.libSegment (branch ^. #children)) \childCausal -> do
      childBranch <- Causal.value childCausal
      loadNamespaceInfo0_ childBranch (childCausal ^. #causalHash)
  pure Nametree {value, children}

loadNamespaceInfo0_ ::
  Monad m =>
  Branch m ->
  CausalHash ->
  m
    ( Nametree
        ( Defns (Map NameSegment (Set Referent)) (Map NameSegment (Set TypeReference)),
          CausalHash
        )
    )
loadNamespaceInfo0_ branch causalHash = do
  let terms = Map.map Map.keysSet (branch ^. #terms)
  let types = Map.map Map.keysSet (branch ^. #types)
  let value = (Defns {terms, types}, causalHash)
  children <-
    for (branch ^. #children) \childCausal -> do
      childBranch <- Causal.value childCausal
      loadNamespaceInfo0_ childBranch (childCausal ^. #causalHash)
  pure Nametree {value, children}

-- | Assert that there are no unconflicted names in a namespace.
assertNamespaceHasNoConflictedNames ::
  Nametree
    ( Defns (Map NameSegment (Set Referent)) (Map NameSegment (Set TypeReference)),
      CausalHash
    ) ->
  Either
    Merge.PreconditionViolation
    ( Nametree
        ( Defns (Map NameSegment Referent) (Map NameSegment TypeReference),
          CausalHash
        )
    )
assertNamespaceHasNoConflictedNames =
  traverseNametreeWithName \names (Defns {terms, types}, causalHash) -> do
    terms <-
      terms & Map.traverseWithKey \name ->
        assertUnconflicted (Merge.ConflictedTermName (Name.fromReverseSegments (name :| names)))
    types <-
      types & Map.traverseWithKey \name ->
        assertUnconflicted (Merge.ConflictedTypeName (Name.fromReverseSegments (name :| names)))
    pure (Defns terms types, causalHash)
  where
    assertUnconflicted :: (Set ref -> Merge.PreconditionViolation) -> Set ref -> Either Merge.PreconditionViolation ref
    assertUnconflicted conflicted refs =
      case Set.asSingleton refs of
        Nothing -> Left (conflicted refs)
        Just ref -> Right ref

-- Convert a merge precondition violation to an output message.
mergePreconditionViolationToOutput :: MergeDatabase -> Merge.PreconditionViolation -> Transaction Output.Output
mergePreconditionViolationToOutput db = \case
  Merge.ConflictedAliases branch name1 name2 -> pure (Output.MergeConflictedAliases branch name1 name2)
  Merge.ConflictedTermName name refs -> Output.MergeConflictedTermName name <$> Set.traverse (referent2to1 db) refs
  Merge.ConflictedTypeName name refs -> pure (Output.MergeConflictedTypeName name refs)
  Merge.ConflictInvolvingBuiltin name -> pure (Output.MergeConflictInvolvingBuiltin name)
  Merge.ConstructorAlias branch name1 name2 -> pure (Output.MergeConstructorAlias branch name1 name2)
  Merge.DefnsInLib -> pure Output.MergeDefnsInLib
  Merge.MissingConstructorName name -> pure (Output.MergeMissingConstructorName name)
  Merge.NestedDeclAlias name -> pure (Output.MergeNestedDeclAlias name)
  Merge.NoConstructorNames name -> pure (Output.MergeNoConstructorNames name)
  Merge.StrayConstructor name -> pure (Output.MergeStrayConstructor name)

-- Assert that a namespace satisfies a few preconditions.
--
-- Fails if:
--   * The "lib" namespace contains any top-level terms or decls. (Only child namespaces are expected here).
--   * Any type declarations are "incoherent" (see `checkDeclCoherency`)
assertNamespaceSatisfiesPreconditions ::
  MergeDatabase ->
  (forall void. Merge.PreconditionViolation -> Transaction void) ->
  ProjectBranchName ->
  Branch Transaction ->
  ( Nametree
      (Defns (Map NameSegment Referent) (Map NameSegment TypeReference))
  ) ->
  Transaction (BiMultimap Name Name, Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name))
assertNamespaceSatisfiesPreconditions db abort branchName branch defns = do
  Map.lookup Name.libSegment (branch ^. #children) `whenJust` \libdepsCausal -> do
    libdepsBranch <- Causal.value libdepsCausal
    when (not (Map.null (libdepsBranch ^. #terms)) || not (Map.null (libdepsBranch ^. #types))) do
      abort Merge.DefnsInLib
  declNames <- checkDeclCoherency db branchName defns & onLeftM abort
  pure
    ( declNames,
      Defns
        { terms = flattenNametree (view #terms) defns,
          types = flattenNametree (view #types) defns
        }
    )

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
  MergeDatabase ->
  ProjectBranchName ->
  ( Nametree
      (Defns (Map NameSegment Referent) (Map NameSegment TypeReference))
  ) ->
  -- | Returns @BiMultimap TypeName ConstructorName@
  Transaction (Either Merge.PreconditionViolation (BiMultimap Name Name))
checkDeclCoherency MergeDatabase {loadDeclNumConstructors} branchName =
  runExceptT
    . fmap (view #declNames)
    . (`State.execStateT` DeclCoherencyCheckState Map.empty BiMultimap.empty)
    . go []
  where
    go ::
      [NameSegment] ->
      ( Nametree
          (Defns (Map NameSegment Referent) (Map NameSegment TypeReference))
      ) ->
      StateT DeclCoherencyCheckState (ExceptT Merge.PreconditionViolation Transaction) ()
    go prefix (Nametree Defns {terms, types} children) = do
      for_ (Map.toList terms) \case
        (_, Referent.Ref _) -> pure ()
        (_, Referent.Con (ReferenceBuiltin _) _) -> pure ()
        (name, Referent.Con (ReferenceDerived typeRef) conId) -> do
          DeclCoherencyCheckState {expectedConstructors} <- State.get
          expectedConstructors1 <- lift (Except.except (Map.upsertF f typeRef expectedConstructors))
          #expectedConstructors .= expectedConstructors1
          where
            f :: Maybe (IntMap MaybeConstructorName) -> Either Merge.PreconditionViolation (IntMap MaybeConstructorName)
            f = \case
              Nothing -> Left (Merge.StrayConstructor (fullName name))
              Just expected -> IntMap.alterF g (fromIntegral @Word64 @Int conId) expected
                where
                  g :: Maybe MaybeConstructorName -> Either Merge.PreconditionViolation (Maybe MaybeConstructorName)
                  g = \case
                    Nothing -> error "didnt put expected constructor id"
                    Just NoConstructorNameYet -> Right (Just (YesConstructorName (fullName name)))
                    Just (YesConstructorName firstName) -> Left (Merge.ConstructorAlias branchName firstName (fullName name))

      childrenWeWentInto <-
        forMaybe (Map.toList types) \case
          (_, ReferenceBuiltin _) -> pure Nothing
          (name, ReferenceDerived typeRef) -> do
            DeclCoherencyCheckState {expectedConstructors} <- State.get
            whatHappened <- do
              let recordNewDecl ::
                    Maybe (IntMap MaybeConstructorName) ->
                    Compose (ExceptT Merge.PreconditionViolation Transaction) WhatHappened (IntMap MaybeConstructorName)
                  recordNewDecl =
                    Compose . \case
                      Just _ -> Except.throwError (Merge.NestedDeclAlias typeName)
                      Nothing ->
                        lift (loadDeclNumConstructors typeRef) <&> \case
                          0 -> UninhabitedDecl
                          n -> InhabitedDecl (IntMap.fromAscList [(i, NoConstructorNameYet) | i <- [0 .. n - 1]])
              lift (getCompose (Map.upsertF recordNewDecl typeRef expectedConstructors))
            case whatHappened of
              UninhabitedDecl -> pure Nothing
              InhabitedDecl expectedConstructors1 -> do
                child <-
                  Map.lookup name children & onNothing do
                    Except.throwError (Merge.NoConstructorNames typeName)
                #expectedConstructors .= expectedConstructors1
                go (name : prefix) child
                DeclCoherencyCheckState {expectedConstructors} <- State.get
                -- fromJust is safe here because we upserted `typeRef` key above
                let (fromJust -> maybeConstructorNames, expectedConstructors1) =
                      Map.deleteLookup typeRef expectedConstructors
                constructorNames <-
                  unMaybeConstructorNames maybeConstructorNames & onNothing do
                    Except.throwError (Merge.MissingConstructorName typeName)
                #expectedConstructors .= expectedConstructors1
                #declNames %= \declNames ->
                  foldr (BiMultimap.insert typeName) declNames constructorNames
                pure (Just name)
            where
              typeName = fullName name

      let childrenWeHaventGoneInto = children `Map.withoutKeys` Set.fromList childrenWeWentInto
      for_ (Map.toList childrenWeHaventGoneInto) \(name, child) -> go (name : prefix) child
      where
        fullName name =
          Name.fromReverseSegments (name :| prefix)

data DeclCoherencyCheckState = DeclCoherencyCheckState
  { expectedConstructors :: !(Map TypeReferenceId (IntMap MaybeConstructorName)),
    declNames :: !(BiMultimap Name Name)
  }
  deriving stock (Generic)

data MaybeConstructorName
  = NoConstructorNameYet
  | YesConstructorName !Name

unMaybeConstructorNames :: IntMap MaybeConstructorName -> Maybe [Name]
unMaybeConstructorNames =
  traverse f . IntMap.elems
  where
    f :: MaybeConstructorName -> Maybe Name
    f = \case
      NoConstructorNameYet -> Nothing
      YesConstructorName name -> Just name

data WhatHappened a
  = UninhabitedDecl
  | InhabitedDecl !a
  deriving stock (Functor, Show)

-- Like `loadNamespaceInfo`, but for loading the LCA, which has fewer preconditions.
--
-- Fails if:
--   * One name is associated with more than one reference.
loadLcaDefinitions ::
  Monad m =>
  (forall void. Merge.PreconditionViolation -> m void) ->
  CausalHash ->
  Branch m ->
  m (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name))
loadLcaDefinitions abort causalHash branch = do
  defns0 <- loadNamespaceInfo0 branch causalHash
  defns1 <- assertNamespaceHasNoConflictedNames defns0 & onLeft abort
  let defns2 = fst <$> defns1
  pure
    Defns
      { terms = flattenNametree (view #terms) defns2,
        types = flattenNametree (view #types) defns2
      }

abortIfAnyConflictedAliases ::
  (forall void. Merge.PreconditionViolation -> Transaction void) ->
  Merge.TwoWay Sqlite.ProjectBranch ->
  Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
  Merge.TwoWay (Defns (Map Name (Merge.DiffOp Hash)) (Map Name (Merge.DiffOp Hash))) ->
  Transaction ()
abortIfAnyConflictedAliases abort projectBranchNames lcaDefns diffs = do
  findConflictedAlias lcaDefns (diffs ^. #alice) `whenJust` \(name1, name2) ->
    abort (Merge.ConflictedAliases (projectBranchNames ^. #alice . #name) name1 name2)
  findConflictedAlias lcaDefns (diffs ^. #bob) `whenJust` \(name1, name2) ->
    abort (Merge.ConflictedAliases (projectBranchNames ^. #bob . #name) name1 name2)

-- @findConflictedAlias namespace diff@, given an old namespace and a diff to a new namespace, will return the first
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
-- This function currently doesn't return whether the conflicted alias is a decl or a term, but it certainly could.
findConflictedAlias ::
  Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
  Defns (Map Name (Merge.DiffOp Hash)) (Map Name (Merge.DiffOp Hash)) ->
  Maybe (Name, Name)
findConflictedAlias defns diff =
  asum
    [ go (defns ^. #terms) (diff ^. #terms),
      go (defns ^. #types) (diff ^. #types)
    ]
  where
    go :: forall ref. Ord ref => BiMultimap ref Name -> Map Name (Merge.DiffOp Hash) -> Maybe (Name, Name)
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

-- | Load the library dependencies (lib.*) of a namespace.
loadLibdeps :: Branch Transaction -> Transaction (Maybe (CausalHash, Map NameSegment (CausalBranch Transaction)))
loadLibdeps branch =
  case Map.lookup Name.libSegment (Branch.children branch) of
    Nothing -> pure Nothing
    Just dependenciesCausal -> do
      dependenciesBranch <- Causal.value dependenciesCausal
      pure (Just (Causal.causalHash dependenciesCausal, Branch.children dependenciesBranch))

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

------------------------------------------------------------------------------------------------------------------------
-- Conflicts

-- `getConflicts diffs` returns the set of conflicted names in `diffs`, where `diffs` contains two branches' diffs from
-- their LCA.
getConflicts :: forall hash name. (Eq hash, Ord name) => Merge.TwoWay (Map name (Merge.DiffOp hash)) -> Set name
getConflicts (Merge.TwoWay aliceDiff bobDiff) =
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

------------------------------------------------------------------------------------------------------------------------
-- Filtering definitions down to just updates, just conflicts, etc.

filterUpdates ::
  Merge.TwoWay (Defns (Map Name (Merge.DiffOp Hash)) (Map Name (Merge.DiffOp Hash))) ->
  Merge.TwoWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
  Merge.TwoWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name))
filterUpdates diff defns =
  Merge.TwoWay
    { alice = filterUpdates1 (diff ^. #alice) (defns ^. #alice),
      bob = filterUpdates1 (diff ^. #bob) (defns ^. #bob)
    }

-- `filterUpdates1 diff defns` returns the subset of `defns` that corresponds to updates (according to `diff`).
filterUpdates1 ::
  Defns (Map Name (Merge.DiffOp Hash)) (Map Name (Merge.DiffOp Hash)) ->
  Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
  Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)
filterUpdates1 diff defns =
  defns
    & over #terms (BiMultimap.restrictRan updatedTermNames)
    & over #types (BiMultimap.restrictRan updatedTypeNames)
  where
    updatedTermNames :: Set Name
    updatedTermNames =
      filterUpdatedNames (diff ^. #terms)

    updatedTypeNames :: Set Name
    updatedTypeNames =
      filterUpdatedNames (diff ^. #types)

    filterUpdatedNames :: Map Name (Merge.DiffOp Hash) -> Set Name
    filterUpdatedNames =
      Map.foldMapWithKey \name op ->
        if isUpdate op
          then Set.singleton name
          else Set.empty

    isUpdate :: Merge.DiffOp Hash -> Bool
    isUpdate = \case
      Merge.Added {} -> False
      Merge.Deleted {} -> False
      Merge.Updated {} -> True


-- `filterConflicts conflicts defns` filters `defns` down to just the conflicted type and term references.
--
-- Fails if it any conflict involving a builtin is discovered, since we can't handle those yet.
filterConflicts ::
  Defns (Set Name) (Set Name) ->
  Merge.TwoWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
  Either Merge.PreconditionViolation (Merge.TwoWay (Defns (Set TermReferenceId) (Set TypeReferenceId)))
filterConflicts conflicts defns = do
  alice <- filterConflicts1 conflicts (defns ^. #alice)
  bob <- filterConflicts1 conflicts (defns ^. #bob)
  pure Merge.TwoWay {alice, bob}

-- `filterConflicts1 defns conflicts` filters `defns` down to just the conflicted type and term references.
--
-- Fails if it any conflict involving a builtin is discovered, since we can't handle those yet.
filterConflicts1 ::
  Defns (Set Name) (Set Name) ->
  Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
  Either Merge.PreconditionViolation (Defns (Set TermReferenceId) (Set TypeReferenceId))
filterConflicts1 conflicts defns = do
  terms <- foldlM doTerm Set.empty (Map.toList (onlyConflicted (conflicts ^. #terms) (defns ^. #terms)))
  types <- foldlM doType Set.empty (Map.toList (onlyConflicted (conflicts ^. #types) (defns ^. #types)))
  pure Defns {terms, types}
  where
    onlyConflicted :: Ord ref => Set Name -> BiMultimap ref Name -> Map Name ref
    onlyConflicted conflictedNames =
      (`Map.restrictKeys` conflictedNames) . BiMultimap.range

    doTerm :: Set TermReferenceId -> (Name, Referent) -> Either Merge.PreconditionViolation (Set TermReferenceId)
    doTerm acc (name, ref) =
      case ref of
        Referent.Con {} -> Right acc
        Referent.Ref (ReferenceBuiltin _) -> Left (Merge.ConflictInvolvingBuiltin name)
        Referent.Ref (ReferenceDerived ref) -> Right $! Set.insert ref acc

    doType :: Set TypeReferenceId -> (Name, TypeReference) -> Either Merge.PreconditionViolation (Set TypeReferenceId)
    doType acc (name, ref) =
      case ref of
        ReferenceBuiltin _ -> Left (Merge.ConflictInvolvingBuiltin name)
        ReferenceDerived ref -> Right $! Set.insert ref acc

-- `filterUnconflicted declNames updates dirty defns` returns the subset of `defns` that are "unconflicted", i.e. ready
-- to put into a namespace and saved to the database.
--
--   * `declNames`: Mappings from constructor name to decl name
--   * `conflicted`: Conflicted things
--   * `updates`: Updates
--   * `dependents`: Dependents of interest (per other person's updates)
--   * `defns`: Definitions
--
-- TODO delete this in favor of filterUnconflicted'
filterUnconflicted ::
  Merge.TwoWay (Map Name Name) ->
  Merge.TwoWay (Defns (Set TermReferenceId) (Set TypeReferenceId)) ->
  Merge.TwoWay (Defns (Map Name Referent) (Map Name TypeReference)) ->
  Merge.TwoWay (Defns (Set TermReferenceId) (Set TypeReferenceId)) ->
  Merge.TwoWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
  Merge.TwoWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name))
filterUnconflicted declNames conflicted updates dependents =
  f #bob #alice . f #alice #bob
  where
    f ::
      (forall a. Lens' (Merge.TwoWay a) a) ->
      (forall a. Lens' (Merge.TwoWay a) a) ->
      Merge.TwoWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
      Merge.TwoWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name))
    f alice bob =
      over alice (filterUnconflicted1 (declNames ^. alice) (dirty ^. alice) (updates ^. bob))

    dirty :: Merge.TwoWay (Defns (Set TermReferenceId) (Set TypeReferenceId))
    dirty =
      conflicted <> dependents

-- `filterUnconflicted1 declName dirty updates defns` returns the subset of `defns` that are "unconflicted", i.e. ready
-- to put into a namespace and saved to the database.
--
--   * `declName`: Alice's mapping from constructor name to decl name
--   * `dirty`: Alice's conflicted things, plus her dependents of interest (per Bob's updates)
--   * `updates`: Bob's updates
--   * `defns`: Alice's definitions
--
-- TODO delete this in favor of filterUnconflicted1'
filterUnconflicted1 ::
  Map Name Name ->
  Defns (Set TermReferenceId) (Set TypeReferenceId) ->
  Defns (Map Name Referent) (Map Name TypeReference) ->
  Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
  Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)
filterUnconflicted1 aliceConstructorNameToDeclName aliceDirty bobUpdates =
  over #types filterUnconflictedTypes . over #terms filterUnconflictedTerms
  where
    filterUnconflictedTerms :: BiMultimap Referent Name -> BiMultimap Referent Name
    filterUnconflictedTerms =
      BiMultimap.filterDom isNotConflicted >>> BiMultimap.filterDomain wasNotUpdatedByBob
      where
        isNotConflicted :: Referent -> Bool
        isNotConflicted = \case
          -- Consider a constructor term "unconflicted" if its decl is unconflicted.
          Referent.Con (ReferenceDerived typeRef) _conId -> not (Set.member typeRef (aliceDirty ^. #types))
          Referent.Ref (ReferenceDerived termRef) -> not (Set.member termRef (aliceDirty ^. #terms))
          -- Keep builtin constructors (which don't even exist) and builtin terms (since they can't be
          -- conflicted, per a precondition)
          Referent.Con (ReferenceBuiltin _) _ -> True
          Referent.Ref (ReferenceBuiltin _) -> True

        wasNotUpdatedByBob :: Referent -> NESet Name -> Bool
        wasNotUpdatedByBob ref names1 =
          case ref of
            Referent.Con _ _ ->
              let declNames = Set.mapMaybe (`Map.lookup` aliceConstructorNameToDeclName) names
               in Set.disjoint typeNamesUpdatedByBob declNames
            Referent.Ref _ -> Set.disjoint termNamesUpdatedByBob names
          where
            names = NESet.toSet names1

    filterUnconflictedTypes :: BiMultimap TypeReference Name -> BiMultimap TypeReference Name
    filterUnconflictedTypes =
      BiMultimap.withoutDom dirty >>> BiMultimap.filterDomain wasNotUpdatedByBob
      where
        dirty :: Set TypeReference
        dirty =
          Set.map ReferenceDerived (aliceDirty ^. #types)

        wasNotUpdatedByBob :: TypeReference -> NESet Name -> Bool
        wasNotUpdatedByBob _ =
          Set.disjoint typeNamesUpdatedByBob . NESet.toSet

    termNamesUpdatedByBob :: Set Name
    termNamesUpdatedByBob =
      Map.keysSet (bobUpdates ^. #terms)

    typeNamesUpdatedByBob :: Set Name
    typeNamesUpdatedByBob =
      Map.keysSet (bobUpdates ^. #types)

-- `filterUnconflicted updates dirty defns` returns references in the subset of `defns` that are "unconflicted", i.e.
-- ready to put into a namespace and saved to the database.
--
--   * `conflicted`: Conflicted things
--   * `updatedNames`: Updated names
--   * `dependents`: Dependents of interest (per other person's updates)
--   * `defns`: Definitions
filterUnconflicted' ::
  Merge.TwoWay (Defns (Set Name) (Set Name)) ->
  Merge.TwoWay (Defns (Set TermReferenceId) (Set TypeReferenceId)) ->
  Merge.TwoWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
  Merge.TwoWay (Defns (Set TermReference) (Set TypeReference))
filterUnconflicted' updatedNames dirty defns =
  Merge.TwoWay
    { alice = f #alice #bob,
      bob = f #bob #alice
    }
  where
    f ::
      (forall a. Lens' (Merge.TwoWay a) a) ->
      (forall a. Lens' (Merge.TwoWay a) a) ->
      Defns (Set TermReference) (Set TypeReference)
    f alice bob =
      filterUnconflicted1' (dirty ^. alice) (updatedNames ^. bob) (defns ^. #alice)

-- `filterUnconflicted1 declName dirty updates defns` returns the references in the subset of `defns` that are
-- "unconflicted", i.e. ready to put into a namespace and saved to the database.
--
--   * `declName`: Alice's mapping from constructor name to decl name
--   * `dirty`: Alice's conflicted things, plus her dependents of interest (per Bob's updates)
--   * `updatedNames`: The names Bob updated
--   * `defns`: Alice's definitions
filterUnconflicted1' ::
  Defns (Set TermReferenceId) (Set TypeReferenceId) ->
  Defns (Set Name) (Set Name) ->
  Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
  Defns (Set TermReference) (Set TypeReference)
filterUnconflicted1' aliceDirty bobUpdatedNames =
  over #types filterUnconflictedTypes . over #terms filterUnconflictedTerms
  where
    filterUnconflictedTerms :: BiMultimap Referent Name -> Set TermReference -- (BiMultimap Referent Name
    filterUnconflictedTerms =
      BiMultimap.domain >>> Map.foldMapWithKey \ref0 names ->
        case ref0 of
          Referent.Con _ _ -> Set.empty
          Referent.Ref ref ->
            if Set.disjoint (bobUpdatedNames ^. #terms) (NESet.toSet names) && isNotDirty ref
              then Set.singleton ref
              else Set.empty
      where
        isNotDirty :: TermReference -> Bool
        isNotDirty = \case
          -- Keep builtin terms (since they can't be conflicted, per a precondition)
          ReferenceBuiltin _ -> True
          ReferenceDerived termRef -> not (Set.member termRef (aliceDirty ^. #terms))

    filterUnconflictedTypes :: BiMultimap TypeReference Name -> Set TypeReference
    filterUnconflictedTypes =
      BiMultimap.domain
        >>> Map.foldMapWithKey \ref names ->
          if wasNotUpdatedByBob ref names && isNotDirty ref
            then Set.singleton ref
            else Set.empty
      where
        wasNotUpdatedByBob :: TypeReference -> NESet Name -> Bool
        wasNotUpdatedByBob _ =
          Set.disjoint (bobUpdatedNames ^. #types) . NESet.toSet

        isNotDirty :: TypeReference -> Bool
        isNotDirty = \case
          -- Keep builtin types (since they can't be conflicted, per a precondition)
          ReferenceBuiltin _ -> True
          ReferenceDerived ref -> not (Set.member ref (aliceDirty ^. #types))

filterUnconflictedUpdates ::
  Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
  Merge.TwoWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
  Merge.TwoWay (Defns (Set TermReference) (Set TypeReference)) ->
  Merge.TwoWay (Defns (Map Referent Referent) (Map TypeReference TypeReference))
filterUnconflictedUpdates lcaDefns updates0 unconflicted' =
  f <$> updates0 <*> unconflicted'
  where
    f ::
      Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
      Defns (Set TermReference) (Set TypeReference) ->
      Defns (Map Referent Referent) (Map TypeReference TypeReference)
    f (Defns termUpdates typeUpdates) (Defns unconflictedTerms unconflictedTypes) =
      Defns
        { terms =
            termUpdates
              & BiMultimap.domain
              & Map.foldlWithKey'
                ( \acc ref0 names ->
                    let isUnconflicted =
                          case ref0 of
                            Referent.Con ref _ -> Set.member ref unconflictedTypes
                            Referent.Ref ref -> Set.member ref unconflictedTerms
                     in if isUnconflicted
                          then Map.insert (oldTermByName names) ref0 acc
                          else acc
                )
                Map.empty,
          types =
            typeUpdates
              & BiMultimap.domain
              & Map.foldlWithKey'
                ( \acc ref names ->
                    if Set.member ref unconflictedTypes
                      then Map.insert (oldTypeByName names) ref acc
                      else acc
                )
                Map.empty
        }

    -- Define partial functions that look up the *old* term/type of an update. The functions take the *new* set of
    -- names for an updated thing, which necessarily has a non-empty intersection with the old names (because it's
    -- an update). It is a merge precondition that all these names in the non-empty intersection are also aliases
    -- in the LCA, so any name will do. Just look them up one by one until one succeeds. (A lookup can fail if the
    -- name didn't exist in the LCA, but is a new alias in the current branch).
    oldTermByName :: NESet Name -> Referent
    oldTermByName = fromJust . altMap (\name -> BiMultimap.lookupRan name (lcaDefns ^. #terms))
    oldTypeByName :: NESet Name -> TypeReference
    oldTypeByName = fromJust . altMap (\name -> BiMultimap.lookupRan name (lcaDefns ^. #types))

------------------------------------------------------------------------------------------------------------------------
-- Dependents of interest

-- `collectDependentsOfInterest defns updates` computes the "dependents of interest", per all definitions `defns` and
-- direct updates `updates`, which are:
--
--   1. Alice's transitive dependents of her dependencies of interest (see below for a definition).
--   2. Bob's transitive dependents of his dependencies of interest (see below for a definition).
--
-- For example, if:
--
--   * Alice updated term "foo" from #oldfoo to #alicefoo, and
--   * Bob uses the name "foo" to refer to #bobfoo (but didn't directly update the name "foo", otherwise we wouldn't
--     have gotten to this code -- nonetheless #bobfoo could be different than #oldfoo due to auto-propagated updates),
--
-- then Bob's "dependencies of interest" are just #bobfoo, and all of his transitive dependents of #bobfoo are his
-- "dependents of interest".
collectDependentsOfInterest ::
  Merge.TwoWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
  Merge.TwoWay (Defns (Set Name) (Set Name)) ->
  Transaction (Merge.TwoWay (Defns (Set TermReferenceId) (Set TypeReferenceId)))
collectDependentsOfInterest defns updates = do
  alice <- getDependents (defns ^. #alice) (updates ^. #bob)
  bob <- getDependents (defns ^. #bob) (updates ^. #alice)
  pure Merge.TwoWay {alice, bob}
  where
    getDependents ::
      Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
      Defns (Set Name) (Set Name) ->
      Transaction (Defns (Set TermReferenceId) (Set TypeReferenceId))
    getDependents defns updates =
      -- The `dependentsWithinScope` query hands back a `Map Reference.Id ReferenceType`, but we would rather
      -- have two different maps, so we twiddle.
      fmap (Map.foldlWithKey' f (Defns Set.empty Set.empty)) do
        Operations.dependentsWithinScope
          (defnsToScope defns)
          (Set.union termDependencies typeDependencies)
      where
        f ::
          Defns (Set TermReferenceId) (Set TypeReferenceId) ->
          Reference.Id ->
          ReferenceType ->
          Defns (Set TermReferenceId) (Set TypeReferenceId)
        f acc ref = \case
          Reference.RtTerm -> acc & over #terms (Set.insert ref)
          Reference.RtType -> acc & over #types (Set.insert ref)

        Defns termDependencies typeDependencies =
          collectDependenciesOfInterest defns updates

-- `defnsToScope defns` converts a flattened namespace `defns` to the set of untagged reference ids contained within,
-- for the purpose of searching for transitive dependents of conflicts that are contained in that set.
defnsToScope :: Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) -> Set Reference.Id
defnsToScope (Defns terms types) =
  Set.union
    (Set.mapMaybe Referent.toReferenceId (BiMultimap.dom terms))
    (Set.mapMaybe Reference.toId (BiMultimap.dom types))

-- `collectDependenciesOfInterest defns updates` computes the "dependencies of interest", per all definitions `defns`
-- and direct updates `updates`, which are:
--
--   1. Whatever Alice refers to by names that Bob updated.
--   2. Whatever Bob refers to by names that Alice updated.
--
-- For example, if:
--
--   * Alice updated term "foo" from #oldfoo to #alicefoo, and
--   * Bob uses the name "foo" to refer to #bobfoo (but didn't directly update the name "foo", otherwise we wouldn't
--     have gotten to this code -- nonetheless #bobfoo could be different than #oldfoo due to auto-propagated updates),
--
-- then Bob's "dependencies of interest" are just #bobfoo.
collectDependenciesOfInterest ::
  Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
  Defns (Set Name) (Set Name) ->
  Defns (Set TermReference) (Set TypeReference)
collectDependenciesOfInterest aliceDefns bobUpdates =
  goTerms (BiMultimap.range (aliceDefns ^. #terms) `Map.restrictKeys` (bobUpdates ^. #terms))
    <> goTypes (BiMultimap.range (aliceDefns ^. #types) `Map.restrictKeys` (bobUpdates ^. #types))
  where
    -- Turn each referent into a reference:
    --
    --   1. For constructors, just ignore the constructor id and use the type reference.
    --   2. For terms, use that term reference.
    goTerms :: Foldable f => f Referent -> Defns (Set TermReference) (Set TypeReference)
    goTerms =
      foldl' f (Defns Set.empty Set.empty)
      where
        f ::
          Defns (Set TermReference) (Set TypeReference) ->
          Referent ->
          Defns (Set TermReference) (Set TypeReference)
        f acc = \case
          Referent.Con typeRef _conId -> acc & over #types (Set.insert typeRef)
          Referent.Ref termRef -> acc & over #terms (Set.insert termRef)

    goTypes :: Foldable f => f TypeReference -> Defns (Set terms) (Set TypeReference)
    goTypes types =
      Defns
        { terms = Set.empty,
          types = Set.fromList (toList types)
        }
