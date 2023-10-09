-- | @merge@ input handler
module Unison.Codebase.Editor.HandleInput.Merge2
  ( handleMerge,
  )
where

import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Lens ((^.))
import Control.Monad.Except qualified as Except (throwError)
import Control.Monad.Reader (ask)
import Control.Monad.State.Strict (StateT)
import Control.Monad.State.Strict qualified as State
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans.Except qualified as Except
import Data.Bimap (Bimap)
import Data.Bimap qualified as Bimap
import Data.ByteString.Short (ShortByteString)
import Data.Function (on)
import Data.Functor.Compose (Compose (..))
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List.NonEmpty (pattern (:|))
import Data.List.NonEmpty qualified as List1
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Monoid (Endo (Endo))
import Data.Semialign (align, alignWith)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy.Builder qualified as Text (Builder)
import Data.Text.Lazy.Builder qualified as Text.Builder
import Data.Text.Lazy.Builder.Int qualified as Text.Builder
import Data.These (These (..))
import Data.Tuple.Strict
import GHC.Clock (getMonotonicTime)
import Text.ANSI qualified as Text
import Text.Printf (printf)
import U.Codebase.Branch (Branch (Branch), CausalBranch)
import U.Codebase.Branch qualified as Branch
import U.Codebase.Branch.Diff (DefinitionDiffs (DefinitionDiffs), Diff (..))
import U.Codebase.Branch.Diff qualified as Diff
import U.Codebase.Causal (Causal (Causal))
import U.Codebase.Causal qualified as Causal
import U.Codebase.HashTags (BranchHash (..), CausalHash (..))
import U.Codebase.Reference (Reference, Reference' (..), TermReference, TermReferenceId, TypeReference, TypeReferenceId)
import U.Codebase.Reference qualified as Reference
import U.Codebase.Referent (Referent)
import U.Codebase.Referent qualified as Referent
import U.Codebase.Sqlite.HashHandle (HashHandle)
import U.Codebase.Sqlite.HashHandle qualified as HashHandle
import U.Codebase.Sqlite.Operations qualified as Operations
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Path (Path')
import Unison.Codebase.Path qualified as Path
import Unison.ConstructorReference (ConstructorReference, ConstructorReferenceId, GConstructorReference (..))
import Unison.ConstructorReference qualified as ConstructorReference
import Unison.Core.ConstructorId (ConstructorId)
import Unison.DataDeclaration qualified as V1 (Decl)
import Unison.Hash (Hash)
import Unison.Hash qualified as Hash
import Unison.HashQualified' qualified as HQ'
import Unison.Merge qualified as Merge
import Unison.Merge2 qualified as Merge
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment (..))
import Unison.NameSegment qualified as NameSegment
import Unison.Prelude hiding (catMaybes)
import Unison.PrettyPrintEnv (PrettyPrintEnv (..))
import Unison.PrettyPrintEnv qualified as Ppe
import Unison.Referent qualified as V1 (Referent)
import Unison.Referent qualified as V1.Referent
import Unison.ShortHash (ShortHash)
import Unison.ShortHash qualified as ShortHash
import Unison.Sqlite (Transaction)
import Unison.Sqlite qualified as Sqlite
import Unison.Syntax.Name qualified as Name (toText)
import Unison.Term qualified as V1 (Term)
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Map qualified as Map
import Unison.Util.Monoid (intercalateMap)
import Unison.Util.Relation (Relation)
import Unison.Util.Relation qualified as Relation
import Unison.Util.Relation3 (Relation3)
import Unison.Util.Relation3 qualified as Relation3
import Unison.Util.Set qualified as Set
import Unison.Var (Var)
import Witch (unsafeFrom)
import Witherable (catMaybes)

-- Temporary simple way to time a transaction
step :: Text -> Transaction a -> Transaction a
step name action = do
  t0 <- Sqlite.unsafeIO getMonotonicTime
  result <- action
  Sqlite.unsafeIO do
    t1 <- getMonotonicTime
    Text.putStrLn (Text.pack (printf "%4d ms | " (round ((t1 - t0) * 1000) :: Int)) <> name)
  pure result

handleMerge :: Path' -> Path' -> Path' -> Cli ()
handleMerge alicePath0 bobPath0 _resultPath = do
  Cli.Env {codebase} <- ask

  alicePath <- Cli.resolvePath' alicePath0
  bobPath <- Cli.resolvePath' bobPath0

  result <-
    Cli.runTransactionWithRollback2 \rollback -> do
      aliceCausal <- step "load alice causal" $ Codebase.getShallowCausalFromRoot Nothing (Path.unabsolute alicePath)
      bobCausal <- step "load bob causal" $ Codebase.getShallowCausalFromRoot Nothing (Path.unabsolute bobPath)

      let aliceCausalHash = Causal.causalHash aliceCausal
      let bobCausalHash = Causal.causalHash bobCausal
      maybeLcaCausalHash <- step "compute lca" $ Operations.lca aliceCausalHash bobCausalHash

      -- Read the (shallow) branches out of the database.
      aliceBranch <- step "load shallow alice branch" $ Causal.value aliceCausal
      bobBranch <- step "load shallow bob branch" $ Causal.value bobCausal

      aliceDefns <- step "load alice definitions" do
        loadNamespaceDefns Operations.expectDeclNumConstructors aliceBranch & onLeftM (rollback . Left)
      bobDefns <- step "load bob definitions" do
        loadNamespaceDefns Operations.expectDeclNumConstructors bobBranch & onLeftM (rollback . Left)

      aliceLibdeps <- step "load alice library dependencies" $ loadLibdeps aliceBranch
      bobLibdeps <- step "load bob library dependencies" $ loadLibdeps bobBranch

      (maybeLcaLibdeps, diffs) <-
        case maybeLcaCausalHash of
          Nothing -> do
            diffs <-
              Merge.nameBasedNamespaceDiff
                (Codebase.unsafeGetTypeDeclaration codebase)
                (Codebase.unsafeGetTerm codebase)
                Merge.TwoOrThreeWay
                  { lca = Nothing,
                    alice = aliceDefns,
                    bob = bobDefns
                  }
            pure (Nothing, diffs)
          Just lcaCausalHash -> do
            lcaCausal <- step "load lca causal" $ Operations.expectCausalBranchByCausalHash lcaCausalHash
            lcaBranch <- step "load lca shallow branch" $ Causal.value lcaCausal
            lcaDefns <- step "load lca definitions" do
              loadNamespaceDefns Operations.expectDeclNumConstructors lcaBranch & onLeftM (rollback . Left)
            diffs <-
              Merge.nameBasedNamespaceDiff
                (Codebase.unsafeGetTypeDeclaration codebase)
                (Codebase.unsafeGetTerm codebase)
                Merge.TwoOrThreeWay
                  { lca = Just lcaDefns,
                    alice = aliceDefns,
                    bob = bobDefns
                  }
            findConflictedAlias aliceDefns (diffs ^. #alice) & onJust \(name1, name2) ->
              rollback (werror ("conflicted alice aliases: " ++ Text.unpack (Name.toText name1) ++ ", " ++ Text.unpack (Name.toText name2)))
            findConflictedAlias bobDefns (diffs ^. #bob) & onJust \(name1, name2) ->
              rollback (werror ("conflicted bob aliases: " ++ Text.unpack (Name.toText name1) ++ ", " ++ Text.unpack (Name.toText name2)))
            lcaLibdeps <- step "load lca library dependencies" $ loadLibdeps lcaBranch
            pure (Just lcaLibdeps, diffs)

    -- (maybeLcaLibdeps, aliceDiff@(Merge.NamespaceDefns aliceDeclDiff aliceTermDiff), bobDiff@(Merge.NamespaceDefns bobDeclDiff bobTermDiff)) <-
    --   case maybeLcaCausalHash of
    --     Nothing -> do
    --       (aliceDiff, bobDiff) <-
    --         Merge.nameBasedNamespaceDiff
    --           (Codebase.unsafeGetTypeDeclaration codebase)
    --           (Codebase.unsafeGetTerm codebase)
    --           Nothing
    --           aliceDefns
    --           bobDefns
    --       pure (Nothing, aliceDiff, bobDiff)
    --     Just lcaCausalHash -> do
    --       lcaCausal <- step "load lca causal" $ Operations.expectCausalBranchByCausalHash lcaCausalHash
    --       lcaBranch <- step "load lca shallow branch" $ Causal.value lcaCausal
    --       T2 lcaDeclNames lcaTermNames <- step "load lca names" do
    --         loadBranchDefinitionNames lcaBranch & onLeftM \err ->
    --           rollback (werror (Text.unpack err))
    --       let lcaDefns = Merge.NamespaceDefns {decls = lcaDeclNames, terms = lcaTermNames}
    --       (aliceDiff, bobDiff) <-
    --         Merge.nameBasedNamespaceDiff
    --           (Codebase.unsafeGetTypeDeclaration codebase)
    --           (Codebase.unsafeGetTerm codebase)
    --           (Just lcaDefns)
    --           aliceDefns
    --           bobDefns


      let conflictedTerms = conflictsish (diffs ^. #alice . #terms) (diffs ^. #bob . #terms)
      let conflictedTypes = conflictsish (diffs ^. #alice . #types) (diffs ^. #bob . #types)

      let mergedLibdeps =
            Merge.mergeLibdeps
              ((==) `on` Causal.causalHash)
              getTwoFreshNames
              maybeLcaLibdeps
              aliceLibdeps
              bobLibdeps



      Sqlite.unsafeIO do
        Text.putStrLn ""
        Text.putStrLn "===== lca->alice diff ====="
        printDeclsDiff (aliceDefns ^. #types) (diffs ^. #alice . #types)
        printTermsDiff (aliceDefns ^. #terms) (diffs ^. #alice . #terms)
        Text.putStrLn ""
        Text.putStrLn "===== lca->bob diff ====="
        printDeclsDiff (bobDefns ^. #types) (diffs ^. #bob . #types)
        printTermsDiff (bobDefns ^. #terms) (diffs ^. #bob . #terms)
        Text.putStrLn ""
        Text.putStrLn "===== merged libdeps dependencies ====="
        printLibdeps mergedLibdeps
        Text.putStrLn ""
        Text.putStrLn "===== conflicts ====="
        printDeclConflicts conflictedTypes
        printTermConflicts conflictedTerms
        Text.putStrLn ""


      pure (Right ())

  case result of
    Left err -> liftIO (print err)
    Right () -> pure ()

makeNamespace ::
  HashHandle ->
  Shallow (Set CausalHash) ->
  Map NameSegment (CausalBranch Transaction) ->
  Map Name TypeReference ->
  Map Name Referent ->
  CausalBranch Transaction
makeNamespace hashHandle ancestors libdeps allDecls allTerms =
  -- honker hashHandle ancestors (makeShallowDefinitions allDecls) (makeShallowDefinitions allTerms)
  undefined

honker ::
  HashHandle ->
  Shallow (Set CausalHash) ->
  Shallow (Map NameSegment TypeReference) ->
  Shallow (Map NameSegment Referent) ->
  CausalBranch Transaction
honker hashHandle (thisLevelAncestors :< childrenAncestors) (thisLevelDecls :< childrenDecls) (thisLevelTerms :< childrenTerms) =
  let branch =
        Branch
          { children = honker2 childrenAncestors childrenDecls childrenTerms,
            patches = Map.empty,
            terms = Map.map unconflictedAndWithoutMetadata thisLevelTerms,
            types = Map.map unconflictedAndWithoutMetadata thisLevelDecls
          }
      branchHash = runIdentity (HashHandle.hashBranch hashHandle wundefined)
      causalHash = HashHandle.hashCausal hashHandle branchHash thisLevelAncestors
   in Causal
        { causalHash,
          valueHash = branchHash,
          parents = wundefined,
          value = pure branch
        }
  where
    unconflictedAndWithoutMetadata :: ref -> Map ref (Transaction Branch.MdValues)
    unconflictedAndWithoutMetadata ref =
      Map.singleton ref (pure (Branch.MdValues Map.empty))

honker2 ::
  Map NameSegment (Shallow (Set CausalHash)) ->
  Map NameSegment (Shallow (Map NameSegment TypeReference)) ->
  Map NameSegment (Shallow (Map NameSegment Referent)) ->
  Map NameSegment (CausalBranch Transaction)
honker2 ancestors decls terms = undefined

type Shallow a =
  Cofree (Map NameSegment) a

oneLayerShallow :: a -> Shallow a
oneLayerShallow =
  (:< Map.empty)

makeShallowDefinitions :: forall ref. Map Name ref -> Shallow (Map NameSegment ref)
makeShallowDefinitions =
  foldr insert (oneLayerShallow Map.empty) . Map.toList . Map.mapKeys (List1.reverse . Name.segments)
  where
    insert :: (List1.NonEmpty NameSegment, ref) -> Shallow (Map NameSegment ref) -> Shallow (Map NameSegment ref)
    insert (k :| ks, v) (xs :< ys) =
      case List1.nonEmpty ks of
        Nothing -> Map.insert k v xs :< ys
        Just ks1 -> xs :< merge k ks1 v ys

    merge ::
      NameSegment ->
      List1.NonEmpty NameSegment ->
      ref ->
      Map NameSegment (Shallow (Map NameSegment ref)) ->
      Map NameSegment (Shallow (Map NameSegment ref))
    merge k ks v =
      Map.upsert (insert (ks, v) . fromMaybe (oneLayerShallow Map.empty)) k

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

data MergePreconditionViolation
  = StrayConstructor !Name
  deriving stock (Show)

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
  m (Either MergePreconditionViolation (Merge.Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)))
loadNamespaceDefns loadNumConstructors branch = do
  libdepsHasTopLevelDefns <-
    case Map.lookup Name.libSegment (branch ^. #children) of
      Nothing -> pure False
      Just libdepsCausal -> do
        libdepsBranch <- Causal.value libdepsCausal
        pure (not (Map.null (libdepsBranch ^. #terms)) || not (Map.null (libdepsBranch ^. #types)))
  if libdepsHasTopLevelDefns
    then pure (Left (werror "bad"))
    else do
      defns0 <- loadNamespaceDefns0 branch
      case makeNamespaceDefns1 defns0 of
        Left err -> pure (Left err)
        Right defns1 ->
          checkDeclCoherency loadNumConstructors defns1 <&> \case
            Left err -> Left err
            Right () -> Right (Merge.flattenNamespaceTree defns1)

type NamespaceDefns0 =
  Merge.NamespaceTree (Merge.Defns (Map NameSegment (Set Referent)) (Map NameSegment (Set TypeReference)))

-- | Load all "namespace definitions" of a branch, which are all terms and type declarations *except* those defined
-- in the "lib" namespace.
loadNamespaceDefns0 :: forall m. Monad m => Branch m -> m NamespaceDefns0
loadNamespaceDefns0 branch = do
  let terms = Map.map Map.keysSet (branch ^. #terms)
  let types = Map.map Map.keysSet (branch ^. #types)
  children <-
    for (Map.delete Name.libSegment (branch ^. #children)) \childCausal -> do
      childBranch <- Causal.value childCausal
      loadNamespaceDefns0_ childBranch
  pure (Merge.Defns {terms, types} :< children)

loadNamespaceDefns0_ :: forall m. Monad m => Branch m -> m NamespaceDefns0
loadNamespaceDefns0_ branch = do
  let terms = Map.map Map.keysSet (branch ^. #terms)
  let types = Map.map Map.keysSet (branch ^. #types)
  children <-
    for (branch ^. #children) \childCausal -> do
      childBranch <- Causal.value childCausal
      loadNamespaceDefns0_ childBranch
  pure (Merge.Defns {terms, types} :< children)

type NamespaceDefns1 =
  Merge.NamespaceTree (Merge.Defns (Map NameSegment Referent) (Map NameSegment TypeReference))

-- | Assert that there are no unconflicted names in a namespace.
makeNamespaceDefns1 :: NamespaceDefns0 -> Either MergePreconditionViolation NamespaceDefns1
makeNamespaceDefns1 =
  traverse \Merge.Defns {terms, types} -> do
    terms <- traverse assertUnconflicted terms
    types <- traverse assertUnconflicted types
    pure (Merge.Defns terms types)
  where
    assertUnconflicted :: Set ref -> Either MergePreconditionViolation ref
    assertUnconflicted refs =
      case Set.asSingleton refs of
        Nothing -> Left (werror "conflicted ref")
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
checkDeclCoherency :: forall m. Monad m => (TypeReferenceId -> m Int) -> NamespaceDefns1 -> m (Either MergePreconditionViolation ())
checkDeclCoherency loadNumConstructors =
  runExceptT . (`State.evalStateT` Map.empty) . go []
  where
    go :: [NameSegment] -> NamespaceDefns1 -> StateT (Map TypeReferenceId IntSet) (ExceptT MergePreconditionViolation m) ()
    go prefix (Merge.Defns {terms, types} :< children) = do
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
              Nothing -> Left (StrayConstructor (Name.fromReverseSegments (name :| prefix)))
              Just expected -> IntSet.alterF g (unsafeFrom @Word64 conId) expected
                where
                  g :: Bool -> Either MergePreconditionViolation Bool
                  g = \case
                    False -> Left (werror ("duplicate constructor " ++ show (ConstructorReference typeRef conId)))
                    True -> Right False

      childrenWeWentInto <-
        forMaybe (Map.toList types) \case
          (_, ReferenceBuiltin _) -> pure Nothing
          (name, ReferenceDerived typeRef) -> do
            s0 <- State.get
            whatHappened <- do
              let f :: Maybe IntSet -> Compose m WhatHappened IntSet
                  f =
                    Compose . \case
                      Just _ -> pure NestedDeclAlias
                      Nothing ->
                        loadNumConstructors typeRef <&> \case
                          0 -> UninhabitedDecl
                          n -> InhabitedDecl (IntSet.fromAscList [0 .. n - 1])
              lift (lift (getCompose (Map.upsertF f typeRef s0)))
            case whatHappened of
              NestedDeclAlias -> Except.throwError (werror "embedded alias")
              UninhabitedDecl -> pure Nothing
              InhabitedDecl s1 ->
                case Map.lookup name children of
                  Nothing -> Except.throwError (werror "no names for constructors")
                  Just child -> do
                    State.put s1
                    go (name : prefix) child
                    s2 <- State.get
                    let (Just x, s3) = Map.deleteLookup typeRef s2
                    when (not (IntSet.null x)) (werror "missing name for constructor")
                    State.put s3
                    pure (Just name)

      let childrenWeHaventGoneInto = children `Map.withoutKeys` Set.fromList childrenWeWentInto
      for_ (Map.toList childrenWeHaventGoneInto) \(name, child) -> go (name : prefix) child

data WhatHappened a
  = NestedDeclAlias
  | UninhabitedDecl
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
  Merge.DefnsA Referent TypeReference ->
  Merge.DefnsB (Merge.DiffOp Hash) (Merge.DiffOp Hash) ->
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

printDeclsDiff :: BiMultimap TypeReference Name -> Map Name (Merge.DiffOp Hash) -> IO ()
printDeclsDiff declNames = do
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

printDeclConflicts :: Set Name -> IO ()
printDeclConflicts =
  Text.putStrLn . Text.unwords . map (("decl " <>) . Name.toText) . Set.toList

printTermConflicts :: Set Name -> IO ()
printTermConflicts =
  Text.putStrLn . Text.unwords . map (("term " <>) . Name.toText) . Set.toList
