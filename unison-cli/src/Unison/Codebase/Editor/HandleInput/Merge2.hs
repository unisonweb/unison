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
import Unison.ConstructorReference (ConstructorReferenceId, GConstructorReference (..))
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

  Cli.runTransactionWithRollback \rollback -> do
    aliceCausal <- step "load alice causal" $ Codebase.getShallowCausalFromRoot Nothing (Path.unabsolute alicePath)
    bobCausal <- step "load bob causal" $ Codebase.getShallowCausalFromRoot Nothing (Path.unabsolute bobPath)

    let aliceCausalHash = Causal.causalHash aliceCausal
    let bobCausalHash = Causal.causalHash bobCausal
    maybeLcaCausalHash <- step "compute lca" $ Operations.lca aliceCausalHash bobCausalHash

    -- Read the (shallow) branches out of the database.
    aliceBranch <- step "load shallow alice branch" $ Causal.value aliceCausal
    bobBranch <- step "load shallow bob branch" $ Causal.value bobCausal

    T2 aliceDeclNames aliceTermNames <- step "load alice names" do
      loadBranchDefinitionNames aliceBranch & onLeftM \err ->
        rollback (werror (Text.unpack err))
    let aliceDefns = Merge.NamespaceDefns {decls = aliceDeclNames, terms = aliceTermNames}

    T2 bobDeclNames bobTermNames <- step "load bob names" do
      loadBranchDefinitionNames bobBranch & onLeftM \err ->
        rollback (werror (Text.unpack err))
    let bobDefns = Merge.NamespaceDefns {decls = bobDeclNames, terms = bobTermNames}

    aliceLibdeps <- step "load alice library dependencies" $ loadLibdeps aliceBranch
    bobLibdeps <- step "load bob library dependencies" $ loadLibdeps bobBranch

    (maybeLcaLibdeps, Merge.NamespaceDefns aliceDeclDiff aliceTermDiff, Merge.NamespaceDefns bobDeclDiff bobTermDiff) <-
      case maybeLcaCausalHash of
        Nothing -> do
          (aliceDiff, bobDiff) <-
            Merge.nameBasedNamespaceDiff
              (Codebase.unsafeGetTypeDeclaration codebase)
              (Codebase.unsafeGetTerm codebase)
              Nothing
              aliceDefns
              bobDefns
          pure (Nothing, aliceDiff, bobDiff)
        Just lcaCausalHash -> do
          lcaCausal <- step "load lca causal" $ Operations.expectCausalBranchByCausalHash lcaCausalHash
          lcaBranch <- step "load lca shallow branch" $ Causal.value lcaCausal
          T2 lcaDeclNames lcaTermNames <- step "load lca names" do
            loadBranchDefinitionNames lcaBranch & onLeftM \err ->
              rollback (werror (Text.unpack err))
          let lcaDefns = Merge.NamespaceDefns {decls = lcaDeclNames, terms = lcaTermNames}
          (aliceDiff, bobDiff) <-
            Merge.nameBasedNamespaceDiff
              (Codebase.unsafeGetTypeDeclaration codebase)
              (Codebase.unsafeGetTerm codebase)
              (Just lcaDefns)
              aliceDefns
              bobDefns

          findConflictedAlias aliceDefns aliceDiff & onJust \(name1, name2) ->
            rollback (werror ("conflicted alice aliases: " ++ Text.unpack (Name.toText name1) ++ ", " ++ Text.unpack (Name.toText name2)))

          findConflictedAlias bobDefns bobDiff & onJust \(name1, name2) ->
            rollback (werror ("conflicted bob aliases: " ++ Text.unpack (Name.toText name1) ++ ", " ++ Text.unpack (Name.toText name2)))

          lcaLibdeps <- step "load lca library dependencies" $ loadLibdeps lcaBranch

          pure (Just lcaLibdeps, aliceDiff, bobDiff)

    let conflictedDecls = conflictsish aliceDeclDiff bobDeclDiff
    let conflictedTerms = conflictsish aliceTermDiff bobDeclDiff

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
      printDeclsDiff aliceDeclNames aliceDeclDiff
      printTermsDiff aliceTermNames aliceTermDiff
      Text.putStrLn ""
      Text.putStrLn "===== lca->bob diff ====="
      printDeclsDiff bobDeclNames bobDeclDiff
      printTermsDiff bobTermNames bobTermDiff
      Text.putStrLn ""
      Text.putStrLn "===== merged libdeps dependencies ====="
      printLibdeps mergedLibdeps
      Text.putStrLn ""
      Text.putStrLn "===== conflicts ====="
      printDeclConflicts conflictedDecls
      printTermConflicts conflictedTerms
      Text.putStrLn ""

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

-- | Load all term and type names from a branch (excluding dependencies) into memory.
--
-- Fails if:
--   * One name is associated with more than one reference.
loadBranchDefinitionNames ::
  forall m.
  Monad m =>
  Branch m ->
  -- TODO better failure type than text
  m (Either Text (T2 (BiMultimap TypeReference Name) (BiMultimap Referent Name)))
loadBranchDefinitionNames =
  runExceptT . go []
  where
    go ::
      [NameSegment] ->
      Branch m ->
      ExceptT Text m (T2 (BiMultimap TypeReference Name) (BiMultimap Referent Name))
    go reversePrefix branch = do
      types <- ExceptT (pure (branchTypeNames reversePrefix branch))
      terms <- ExceptT (pure (branchTermNames reversePrefix branch))

      T2 childrenTypes childrenTerms <-
        -- Ignore children namespaces of `lib`
        if reversePrefix == [Name.libSegment]
          then pure (T2 BiMultimap.empty BiMultimap.empty)
          else do
            childrenNames <-
              for (Map.toList (Branch.children branch)) \(childName, childCausal) -> do
                childBranch <- lift (Causal.value childCausal)
                go (childName : reversePrefix) childBranch
            -- These unions are safe because names of one child (e.g. "child1.foo.bar") can't equal the names of any other
            -- child (e.g. "child2.baz.qux").
            let combine (T2 xs0 ys0) (T2 xs1 ys1) =
                  T2 (BiMultimap.unsafeUnion xs0 xs1) (BiMultimap.unsafeUnion ys0 ys1)
            pure (foldr combine (T2 BiMultimap.empty BiMultimap.empty) childrenNames)

      -- These unions are safe because the names at this level (e.g. "foo.bar.baz") can't equal the names at deeper
      -- levels (e.g. "foo.bar.baz.qux")
      let allTypes = BiMultimap.unsafeUnion types childrenTypes
      let allTerms = BiMultimap.unsafeUnion terms childrenTerms
      pure (T2 allTypes allTerms)

    branchTypeNames :: [NameSegment] -> Branch m -> Either Text (BiMultimap TypeReference Name)
    branchTypeNames reversePrefix branch =
      branchDefnNames reversePrefix (Branch.types branch)

    branchTermNames :: [NameSegment] -> Branch m -> Either Text (BiMultimap Referent Name)
    branchTermNames reversePrefix branch =
      branchDefnNames reversePrefix (Branch.terms branch)

    branchDefnNames ::
      forall metadata ref.
      Ord ref =>
      [NameSegment] ->
      Map NameSegment (Map ref metadata) ->
      Either Text (BiMultimap ref Name)
    branchDefnNames reversePrefix =
      foldr f (Right BiMultimap.empty) . Map.toList
      where
        f ::
          (NameSegment, Map ref metadata) ->
          Either Text (BiMultimap ref Name) ->
          Either Text (BiMultimap ref Name)
        f (segment, refs) = \case
          Left err -> Left err
          Right acc ->
            case Set.asSingleton (Map.keysSet refs) of
              Nothing -> Left ("multiple refs with name " <> Name.toText name)
              Just ref -> Right (BiMultimap.insert ref name acc)
          where
            name = Name.fromReverseSegments (segment :| reversePrefix)

data Oink terms types = Oink
  { terms :: terms,
    types :: types
  }

type StepOne =
  Cofree
    (Map NameSegment)
    (Oink (Map NameSegment (Set Referent)) (Map NameSegment (Set TypeReference)))

loadStepOne :: forall m. Monad m => Branch m -> m StepOne
loadStepOne branch = do
  let terms = Map.map Map.keysSet (branch ^. #terms)
  let types = Map.map Map.keysSet (branch ^. #types)
  children <-
    for (branch ^. #children) \childCausal -> do
      childBranch <- Causal.value childCausal
      loadStepOne childBranch
  pure (Oink {terms, types} :< children)

type StepTwo =
  Cofree
    (Map NameSegment)
    (Oink (Map NameSegment Referent) (Map NameSegment TypeReference))

doStepTwo :: StepOne -> Either Text StepTwo
doStepTwo =
  traverse \Oink {terms, types} -> do
    terms <- traverse assertUnconflicted terms
    types <- traverse assertUnconflicted types
    pure Oink {terms, types}
  where
    assertUnconflicted :: Set ref -> Either Text ref
    assertUnconflicted refs =
      case Set.asSingleton refs of
        Nothing -> Left (werror "conflicted ref")
        Just ref -> Right ref

doStepThree :: forall m. Monad m => (TypeReferenceId -> m Int) -> StepTwo -> m (Either Text ())
doStepThree loadNumConstructors =
  runExceptT . (`State.evalStateT` Map.empty) . go
  where
    go :: StepTwo -> StateT (Map TypeReferenceId IntSet) (ExceptT Text m) ()
    go (Oink {terms, types} :< children) = do
      for_ terms \case
        Referent.Ref _ -> pure ()
        Referent.Con (ReferenceBuiltin _) _ -> pure ()
        Referent.Con (ReferenceDerived typeRef) conId -> do
          -- could use modifyM on newer tranformers
          s0 <- State.get
          s1 <- lift (Except.except (Map.upsertF f typeRef s0))
          State.put s1
          where
            f :: Maybe IntSet -> Either Text IntSet
            f = \case
              Nothing -> Left (werror "stray constructor")
              Just expected -> IntSet.alterF g (unsafeFrom @Word64 conId) expected
                where
                  g :: Bool -> Either Text Bool
                  g = \case
                    False -> Left (werror ("duplicate constructor " ++ show (ConstructorReference typeRef conId)))
                    True -> Right False

      for_ (Map.toList types) \case
        (_, ReferenceBuiltin _) -> pure ()
        (name, ReferenceDerived typeRef) -> do
          s0 <- State.get
          honk <- do
            let f :: Maybe IntSet -> Compose m Honk IntSet
                f =
                  Compose . \case
                    Just _ -> pure HonkEmbeddedAlias
                    Nothing ->
                      loadNumConstructors typeRef <&> \case
                        0 -> HonkVoidBoy
                        n -> HonkWoohoo (IntSet.fromAscList [0 .. n - 1])
            lift (lift (getCompose (Map.upsertF f typeRef s0)))
          case honk of
            HonkEmbeddedAlias -> Except.throwError (werror "embedded alias")
            HonkVoidBoy -> pure ()
            HonkWoohoo s1 ->
              case Map.lookup name children of
                Nothing -> Except.throwError (werror "no names for constructors")
                Just child -> do
                  go child
                  s0 <- State.get
                  let (Just x, s1) = Map.deleteLookup typeRef s0
                  when (not (IntSet.null x)) (werror "missing name for constructor")
                  State.put s1

data Honk a
  = HonkEmbeddedAlias
  | HonkVoidBoy
  | HonkWoohoo !a
  deriving stock (Functor)

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
  forall hash name.
  (Eq hash, Ord name) =>
  Merge.NamespaceDefns BiMultimap TypeReference name Referent name ->
  Merge.NamespaceDefns Map name (Merge.DiffOp hash) name (Merge.DiffOp hash) ->
  Maybe (name, name)
findConflictedAlias aliceDefns aliceDiff =
  asum
    [ go (aliceDefns ^. #decls) (aliceDiff ^. #decls),
      go (aliceDefns ^. #terms) (aliceDiff ^. #terms)
    ]
  where
    go ::
      forall ref.
      Ord ref =>
      BiMultimap ref name ->
      Map name (Merge.DiffOp hash) ->
      Maybe (name, name)
    go namespace diff =
      asum (map f (Map.toList diff))
      where
        f :: (name, Merge.DiffOp hash) -> Maybe (name, name)
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
      _ -> Nothing

-- | Load the library dependencies (lib.*) of a namespace.
loadLibdeps :: Branch Transaction -> Transaction (Map NameSegment (CausalBranch Transaction))
loadLibdeps branch =
  case Map.lookup Name.libSegment (Branch.children branch) of
    Nothing -> pure Map.empty
    Just dependenciesCausal -> Branch.children <$> Causal.value dependenciesCausal

------------------------------------------------------------------------------------------------------------------------
-- The "decl coherency tracker" is a small data structure and API for implmenenting a check on the shape of decls in
-- a namespace.
--
-- It could be moved into SQLite (with sufficient schema support) some day, but for now, because the merge algorithm
-- needs to pull lots of stuff into memory anyway, we just do this in memory, too.
--
-- A historical note: once upon a time, decls could be "incoherent". Then, we decided we want decls to be "coherent".
-- Thus, this machinery was invented.
--
-- Now, onto the definition: a type declaration is "coherent" if it satisfies both of the following criteria.
--
--   1. For each naming of a type decl (say Foo#foohash), there exists exactly one name for each constructor of #foohash
--      arbitrarily deep in namespace Foo.
--
--      This means we can render the decl naturally, as in
--
--        structural type Foo
--          = Bar Nat Int
--          | internal.hello.Bonk Nat
--
--      which we could not do if there was at least one constructor whose full name does not contain the full name of
--      the type decl itself as a prefix.
--
--      A notable consequence of this requirement is that a second naming of a decl (i.e. an alias) cannot be embedded
--      within the first naming, as in:
--
--        type Foo = ...
--        type Foo.some.inner.namespace = ... -- an alias of Foo
--
--   2. For each naming of a constructor (say Foo.internal.Bar#foohash#2), there exists exactly one naming of its type
--      declaration #foohash that is a prefix of the constructor's name.
--
--      This means that we won't have any "stray" aliases for constructors, as in the namespace:
--
--        "Foo"       => #foohash
--        "Foo.Bar"   => #foohash#0
--        "SomeAlias" => #foohash#0
--
-- On to the implementation: we keep a stateful mapping between type reference and the set of constructors we expect to
-- see beneath it.
--
-- When processing a namespace, for each type declaration, we add an entry to the map that contains a set of every
-- constructor id. (If a type declaration already exists in the map, that means we're looking at one alias embedded
-- within another, so we bail.)
--
-- For example, if a namespace has a decl #foohash with two constructors, we would add the following key/value pair:
--
--   #foohash => {0, 1}
--
-- Whenever we encounter a constructor reference in a namespace, we delete the corresponding entry from the map. For
-- example, were we to encounter #foohash#0, we would adjust the mapping to contain:
--
--   #foohash => {1}
--
-- Were we to delete the last constructor id in the value of the map, we would leave behind the empty set:
--
--   #foohash => {}
--
-- The key #foohash in this example is still needed, even with an empty set of unseen constructor ids, to support the
-- check for condition (2) above, that there are no "stray" decls. Because if we come across a constructor reference
-- such as #barhash#2, with no corresponding key in our map, that means we are not underneath any namespace whose name
-- is shared by a naming for the type declaration #barhash.
--
-- Thus, at the end of fully processing each of a namespace's children with this stateful mapping, we will be in one of
-- three cases:
--
--   1. The processing short-circuited at some point because a "stray" naming of a constructor was discovered somewhere
--      in a child namespace.
--   2. The mapping at the end of the processing contains at least one constructor id underneath at least one key, which
--      means that decl is not "coherent" (as it doesn't have a name for that constructor underneath it in the
--      namespace).
--   3. The mapping at the end of the processing has no constructor ids under any keys, which means the decl is
--      "coherent".
type DeclCoherencyTracker =
  Map TypeReferenceId IntSet

-- a namespace has:
--
--   terms :: Map NameSegment Referent
--   types :: Map NameSegment TypeReference
--
--     "Foo" => #foohash
--     "Bar" => #foohash
--
--   children :: Map NameSegment Child
--
--     "Foo" =>
--       terms = {
--         "Bar" => #foohash#1
--       }
--
-- 1. Process each constructor in `terms`, each of which can:
--      1. Check off that we've seen it
--      2. Blow up because it's a stray
--
-- 2. Process each decl in `types`:
--      1. If key doesn't exist, if n == 0, do JACK SHIT
--
--      2. If key doesn't exist, if n > 0, add it with value = {0,1,...,n-1}
--
--         Process child:
--           1. Recurse
--           2. Post-process the state
--               1. Delete+lookup that key/value pair from map
--               2. If value is not {}, error: missing name for constructor
--
--      3. If key does exist, bad, bail (one alias embedded within another, so constructor name check will surely fail)
--

data Something

-- `helloDecl loadNumConstructors ref tracker` updates `tracker` to start tracking decl `ref`.
helloDecl ::
  forall m.
  Monad m =>
  (TypeReferenceId -> m Int) ->
  TypeReferenceId ->
  Maybe Something ->
  DeclCoherencyTracker ->
  m (Either Text DeclCoherencyTracker)
helloDecl loadNumConstructors ref child tracker =
  runExceptT (Map.alterF f ref tracker)
  where
    f :: Maybe IntSet -> ExceptT Text m (Maybe IntSet)
    f = \case
      Just _ -> Except.throwE (werror "one alias embedded within another")
      Nothing ->
        lift do
          loadNumConstructors ref <&> \case
            0 -> Nothing
            n -> Just (IntSet.fromAscList [0 .. n - 1])

-- `helloConstructor ref tracker` updates `tracker` to record that `ref` was observed.
helloConstructor :: ConstructorReferenceId -> DeclCoherencyTracker -> Either Text DeclCoherencyTracker
helloConstructor (ConstructorReference ref cid) =
  Map.upsertF f ref
  where
    f :: Maybe IntSet -> Either Text IntSet
    f = \case
      Nothing -> Left (werror ("stray constructor " ++ show (ConstructorReference ref cid)))
      Just expected0 ->
        IntSet.alterF g (unsafeFrom @Word64 cid) expected0
        where
          g :: Bool -> Either Text Bool
          g = \case
            False -> Left (werror ("duplicate constructor " ++ show (ConstructorReference ref cid)))
            True -> Right False

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
