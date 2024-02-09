module Unison.Codebase.Editor.HandleInput.Merge2
  ( handleMerge,
  )
where

import Control.Lens (Lens', over, view, (%=), (.=), (.~), (^.))
import Control.Monad.Except qualified as Except (throwError)
import Control.Monad.Reader (ask)
import Control.Monad.State.Strict (StateT)
import Control.Monad.State.Strict qualified as State
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans.Except qualified as Except
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
import Unison.Merge.PreconditionViolation qualified as Merge
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment (..))
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
