module Unison.Merge.Diff
  ( synhashDefns,
    synhashDefns0,
    synhashLcaDefns,
    diffSynhashedDefns,
    diffSynhashedDefns',
    humanizeDiffs,
  )
where

import Data.List.NonEmpty qualified as NEL
import Data.List.NonEmpty qualified as NEList
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict qualified as Map
import Data.Semialign (alignWith, unalign)
import Data.Set qualified as Set
import Data.Set.NonEmpty qualified as NESet
import Data.These (These (..))
import Data.Zip qualified as Zip
import U.Codebase.Reference (TypeReference)
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.DataDeclaration (Decl)
import Unison.DataDeclaration qualified as DataDeclaration
import Unison.DeclNameLookup (DeclNameLookup)
import Unison.DeclNameLookup qualified as DeclNameLookup
import Unison.Hash (Hash (Hash))
import Unison.Merge.DiffOp (DiffOp (..), DiffOp2 (..))
import Unison.Merge.DiffOp qualified as DiffOp
import Unison.Merge.HumanDiffOp (HumanDiffOp (..))
import Unison.Merge.Synhash qualified as Synhash
import Unison.Merge.Synhashed (Synhashed (..))
import Unison.Merge.Synhashed qualified as Synhashed
import Unison.Merge.ThreeWay (GThreeWay, ThreeWay (..))
import Unison.Merge.ThreeWay qualified as ThreeWay
import Unison.Merge.TwoWay (TwoWay (..))
import Unison.Merge.TwoWay qualified as TwoWay
import Unison.Merge.Updated (GUpdated (..), Updated)
import Unison.Merge.Updated qualified as Updated
import Unison.Name (Name)
import Unison.Names (Names)
import Unison.Names qualified as Names
import Unison.Parser.Ann (Ann)
import Unison.PartialDeclNameLookup (PartialDeclNameLookup (..))
import Unison.PartialDeclNameLookup qualified as PartialDeclNameLookup
import Unison.Prelude hiding (catMaybes)
import Unison.PrettyPrintEnv (PrettyPrintEnv (..))
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl (..))
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Reference (Reference' (..), TermReference, TermReferenceId, TypeReferenceId)
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Symbol (Symbol)
import Unison.Syntax.Name qualified as Name
import Unison.Term (Term)
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Defns (Defns (..), DefnsF, DefnsF2, DefnsF3, unzipDefns, zipDefnsWith)
import Unison.Util.Defns qualified as Defns
import Unison.Util.Relation (Relation)
import Unison.Util.Relation qualified as Rel

-- | @nameBasedNamespaceDiff db declNameLookups defns@ returns Alice's and Bob's name-based namespace diffs, each in the
-- form:
--
-- > terms :: Map Name (DiffOp (Synhashed Referent))
-- > types :: Map Name (DiffOp (Synhashed TypeReference))
--
-- where each name is paired with its diff-op (added, deleted, or updated), relative to the LCA between Alice and Bob's
-- branches. If the hash of a name did not change, it will not appear in the map.
nameBasedNamespaceDiff ::
  (HasCallStack) =>
  TwoWay DeclNameLookup ->
  PartialDeclNameLookup ->
  ThreeWay PrettyPrintEnvDecl ->
  ThreeWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
  Defns (Map TermReferenceId (Term Symbol Ann)) (Map TypeReferenceId (Decl Symbol Ann)) ->
  ( -- Core diffs, i.e. adds, deletes, and updates which have different synhashes.
    TwoWay (DefnsF3 (Map Name) DiffOp Synhashed Referent TypeReference),
    -- Propagated updates, i.e. updates which have the same synhash but different Unison hashes.
    TwoWay (DefnsF (Map Name) (Updated Referent) (Updated TypeReference))
  )
nameBasedNamespaceDiff declNameLookups lcaDeclNameLookup ppeds defns0 hydratedDefns =
  let -- Throw away the Ref->Name lookup direction of defns, we don't need it.
      defns = bimap BiMultimap.range BiMultimap.range <$> defns0

      -- For the (LCA, Alice) and (LCA, Bob) pairs of defns that we will soon syntactic-hash-then-diff, throw away the
      -- definitions that we can tell *would* have the same syntactic hash. This allows us to avoid computing syntactic
      -- hashes unnecessarily, when a cheaper comparison (comparing refs, essentially) will suffice.
      narrowedLcaDefns :: TwoWay (DefnsF (Map Name) Referent TypeReference)
      narrowedDefns :: TwoWay (DefnsF (Map Name) Referent TypeReference)
      (narrowedLcaDefns, narrowedDefns) =
        Zip.unzip $
          narrowDefns lcaDeclNameLookup defns.lca
            <$> declNameLookups
            <*> ThreeWay.forgetLca defns

      -- Compute the syntactic hashes for all of the relevant (to either Alice or Bob) definitions in the LCA.
      allSynhashedNarrowedLcaDefns :: DefnsF2 (Map Name) Synhashed Referent TypeReference
      allSynhashedNarrowedLcaDefns =
        synhashLcaDefns
          id
          synhashPPE
          lcaDeclNameLookup
          (TwoWay.twoWay (zipDefnsWith Map.union Map.union) narrowedLcaDefns)
          hydratedDefns

      -- Project out just the synhashed LCA definitions relevant to Alice and Bob with map intersection
      synhashedNarrowedLcaDefns :: TwoWay (DefnsF2 (Map Name) Synhashed Referent TypeReference)
      synhashedNarrowedLcaDefns =
        zipDefnsWith Map.intersection Map.intersection allSynhashedNarrowedLcaDefns <$> narrowedLcaDefns

      -- Compute the syntactic hash of definitions
      synhashedDefns :: TwoWay (DefnsF2 (Map Name) Synhashed Referent TypeReference)
      synhashedDefns =
        synhashDefns0 id synhashPPE hydratedDefns <$> declNameLookups <*> narrowedDefns

      -- Compute 2-way diffs
      diff ::
        TwoWay
          ( DefnsF3 (Map Name) DiffOp Synhashed Referent TypeReference,
            DefnsF (Map Name) (Updated Referent) (Updated TypeReference)
          )
      diff =
        diffSynhashedDefns0 <$> synhashedNarrowedLcaDefns <*> synhashedDefns
   in Zip.unzip diff
  where
    synhashPPE :: PPE.PrettyPrintEnv
    synhashPPE =
      let ThreeWay {lca = lcaPPE, alice = alicePPE, bob = bobPPE} = PPED.unsuffixifiedPPE <$> ppeds
       in alicePPE `PPE.addFallback` bobPPE `PPE.addFallback` lcaPPE

-- `narrowDefns` takes and old and new namespace (and their respective decl name lookups), and returns old' and new'
-- namespaces, that contain only definitions that have a chance at having different syntactic hashes.
--
-- (It's not quite as simple as retaining only the definitions with non-equal Unison hashes, as a type declaration's
-- syntactic hash changes if any of its constructors are renamed, but its Unison hash does not).
narrowDefns ::
  PartialDeclNameLookup ->
  DefnsF (Map Name) Referent TypeReference ->
  DeclNameLookup ->
  DefnsF (Map Name) Referent TypeReference ->
  (DefnsF (Map Name) Referent TypeReference, DefnsF (Map Name) Referent TypeReference)
narrowDefns oldDeclNameLookup oldDefns newDeclNameLookup newDefns =
  unzipDefns $
    zipDefnsWith
      (narrowTerms oldDeclNameLookup newDeclNameLookup)
      (narrowTypes oldDeclNameLookup newDeclNameLookup)
      oldDefns
      newDefns

narrowTerms ::
  PartialDeclNameLookup ->
  DeclNameLookup ->
  Map Name Referent ->
  Map Name Referent ->
  (Map Name Referent, Map Name Referent)
narrowTerms oldDeclNameLookup newDeclNameLookup =
  filterOutEqualSynhash \name oldRef newRef ->
    case (oldRef, newRef) of
      -- Drop hash-equal terms
      (Referent.Ref oldRef1, Referent.Ref newRef1) -> oldRef1 == newRef1
      -- Drop equal constructors only if they would have equal synhashes, i.e. their types have the same
      -- namings of constructors
      (Referent.Con oldRef1 _, Referent.Con newRef1 _) ->
        let oldConstructorNames =
              PartialDeclNameLookup.expectConstructorNames
                oldDeclNameLookup
                (PartialDeclNameLookup.expectDeclName oldDeclNameLookup name)
            newConstructorNames =
              DeclNameLookup.expectConstructorNames
                newDeclNameLookup
                (DeclNameLookup.expectDeclName newDeclNameLookup name)
         in oldRef1 == newRef1 && oldConstructorNames == map Just newConstructorNames
      _ -> False

narrowTypes ::
  PartialDeclNameLookup ->
  DeclNameLookup ->
  Map Name TypeReference ->
  Map Name TypeReference ->
  (Map Name TypeReference, Map Name TypeReference)
narrowTypes oldDeclNameLookup newDeclNameLookup =
  filterOutEqualSynhash \name oldRef newRef ->
    -- Drop equal types only if they would have equal synhashes, i.e. they have the same namings of constructors
    let oldConstructorNames = PartialDeclNameLookup.expectConstructorNames oldDeclNameLookup name
        newConstructorNames = DeclNameLookup.expectConstructorNames newDeclNameLookup name
     in oldRef == newRef && oldConstructorNames == map Just newConstructorNames

filterOutEqualSynhash ::
  forall ref.
  (Name -> ref -> ref -> Bool) ->
  Map Name ref ->
  Map Name ref ->
  (Map Name ref, Map Name ref)
filterOutEqualSynhash equal oldDefns newDefns =
  unalign $
    Map.merge
      (Map.mapMissing \_ -> This)
      (Map.mapMissing \_ -> That)
      (Map.zipWithMaybeMatched f)
      oldDefns
      newDefns
  where
    f :: Name -> ref -> ref -> Maybe (These ref ref)
    f name oldRef newRef =
      if equal name oldRef newRef
        then Nothing
        else Just (These oldRef newRef)

------------------------------------------------------------------------------------------------------------------------
-- Syntactic hashing

-- | @synhashDefns declNameLookups ppes defns hydratedDefns@ computes syntactic hashes of @defns@.
synhashDefns ::
  (HasCallStack) =>
  GThreeWay PartialDeclNameLookup DeclNameLookup ->
  ThreeWay PrettyPrintEnvDecl ->
  ThreeWay (DefnsF (Map Name) Referent TypeReference) ->
  Defns (Map TermReferenceId (Term Symbol Ann)) (Map TypeReferenceId (Decl Symbol Ann)) ->
  ThreeWay (DefnsF2 (Map Name) Synhashed Referent TypeReference)
synhashDefns declNameLookups ppes defns hydratedDefns =
  ThreeWay
    { alice = synhashDefns0 id ppe hydratedDefns declNameLookups.alice defns.alice,
      bob = synhashDefns0 id ppe hydratedDefns declNameLookups.bob defns.bob,
      lca = synhashLcaDefns id ppe declNameLookups.lca defns.lca hydratedDefns
    }
  where
    ppe :: PPE.PrettyPrintEnv
    ppe =
      ppes.alice.unsuffixifiedPPE
        `PPE.addFallback` ppes.bob.unsuffixifiedPPE
        `PPE.addFallback` ppes.lca.unsuffixifiedPPE

synhashLcaDefns ::
  (HasCallStack) =>
  (term -> Term Symbol Ann) ->
  PrettyPrintEnv ->
  PartialDeclNameLookup ->
  DefnsF (Map Name) Referent TypeReference ->
  Defns (Map TermReferenceId term) (Map TypeReferenceId (Decl Symbol Ann)) ->
  DefnsF2 (Map Name) Synhashed Referent TypeReference
synhashLcaDefns toTerm ppe declNameLookup defns hydratedDefns =
  synhashDefnsWith hashReferent hashType defns
  where
    -- For the LCA only, if we don't have a name for every constructor, or we don't have a name for a decl, that's okay,
    -- just use a dummy syntactic hash (e.g. where we return `Hash mempty` below in two places).
    --
    -- This is safe and correct; Alice/Bob can't have such a decl (it violates a merge precondition), so there's no risk
    -- that we accidentally get an equal hash and classify a real update as unchanged.

    hashReferent :: Name -> Referent -> Hash
    hashReferent name = \case
      Referent.Con (ConstructorReference ref _) _ ->
        case Map.lookup name declNameLookup.constructorToDecl of
          Nothing -> Hash mempty -- see note above
          Just declName -> hashType declName ref
      Referent.Ref ref -> synhashTermReference toTerm ppe hydratedDefns.terms ref

    hashType :: Name -> TypeReference -> Hash
    hashType name = \case
      ReferenceBuiltin builtin -> Synhash.synhashBuiltinDecl builtin
      ReferenceDerived ref ->
        case sequence (declNameLookup.declToConstructors Map.! name) of
          Nothing -> Hash mempty -- see note above
          Just names -> synhashDerivedDecl ppe hydratedDefns.types names name ref

synhashDefns0 ::
  (HasCallStack) =>
  (term -> Term Symbol Ann) ->
  PrettyPrintEnv ->
  Defns (Map TermReferenceId term) (Map TypeReferenceId (Decl Symbol Ann)) ->
  DeclNameLookup ->
  DefnsF (Map Name) Referent TypeReference ->
  DefnsF2 (Map Name) Synhashed Referent TypeReference
synhashDefns0 toTerm ppe hydratedDefns declNameLookup =
  synhashDefnsWith hashReferent hashType
  where
    hashReferent :: Name -> Referent -> Hash
    hashReferent name = \case
      -- We say that a referent constructor *in the namespace* (distinct from a referent that is in a term body) has a
      -- synhash that is simply equal to the synhash of its type declaration. This is because the type declaration and
      -- constructors are changed in lock-step: it is not possible to change one, but not the other.
      --
      -- For example, if Alice updates `type Foo = Bar Nat` to `type Foo = Bar Nat Nat`, we want different synhashes on
      -- both the type (Foo) and the constructor (Foo.Bar).
      Referent.Con (ConstructorReference ref _) _ -> hashType (DeclNameLookup.expectDeclName declNameLookup name) ref
      Referent.Ref ref -> synhashTermReference toTerm ppe hydratedDefns.terms ref

    hashType :: Name -> TypeReference -> Hash
    hashType name = \case
      ReferenceBuiltin builtin -> Synhash.synhashBuiltinDecl builtin
      ReferenceDerived ref ->
        synhashDerivedDecl ppe hydratedDefns.types (DeclNameLookup.expectConstructorNames declNameLookup name) name ref

synhashDerivedDecl ::
  (HasCallStack) =>
  PrettyPrintEnv ->
  Map TypeReferenceId (Decl Symbol Ann) ->
  [Name] ->
  Name ->
  TypeReferenceId ->
  Hash
synhashDerivedDecl ppe declsById names name ref =
  declsById
    & expectDecl ref
    & DataDeclaration.setConstructorNames (map Name.toVar names)
    & Synhash.synhashDerivedDecl ppe name

synhashTermReference ::
  (HasCallStack) =>
  (term -> Term Symbol Ann) ->
  PrettyPrintEnv ->
  Map TermReferenceId term ->
  TermReference ->
  Hash
synhashTermReference toTerm ppe termsById = \case
  ReferenceBuiltin builtin -> Synhash.synhashBuiltinTerm builtin
  ReferenceDerived ref -> Synhash.synhashDerivedTerm ppe (toTerm (expectTerm ref termsById))

synhashDefnsWith ::
  (HasCallStack) =>
  (Name -> term -> Hash) ->
  (Name -> typ -> Hash) ->
  DefnsF (Map Name) term typ ->
  DefnsF2 (Map Name) Synhashed term typ
synhashDefnsWith hashTerm hashType = do
  bimap (Map.mapWithKey hashTerm1) (Map.mapWithKey hashType1)
  where
    hashTerm1 name term =
      Synhashed (hashTerm name term) term

    hashType1 name typ =
      Synhashed (hashType name typ) typ

------------------------------------------------------------------------------------------------------------------------
-- Diffing syntactic hashes

-- | @diffSynhashedDefns defns@, given the output of @synhashDefns@, computes the two two-way diffs (each consisting of
-- the "core" diffs, i.e. adds/delete/updates, alongside the propagated updates, i.e. updates that have the same synhash
-- but different Unison hashes).
diffSynhashedDefns ::
  ThreeWay (DefnsF2 (Map Name) Synhashed Referent TypeReference) ->
  ( -- Core diffs, i.e. adds, deletes, and updates which have different synhashes.
    TwoWay (DefnsF3 (Map Name) DiffOp Synhashed Referent TypeReference),
    -- Propagated updates, i.e. updates which have the same synhash but different Unison hashes.
    TwoWay (DefnsF (Map Name) (Updated Referent) (Updated TypeReference))
  )
diffSynhashedDefns defns =
  Zip.unzip (diffSynhashedDefns0 defns.lca <$> ThreeWay.forgetLca defns)

-- | @diffSynhashedDefns defns@, given the output of @synhashDefns@, computes the two two-way diffs (each consisting of
-- the "core" diffs, i.e. adds/delete/updates, alongside the propagated updates, i.e. updates that have the same synhash
-- but different Unison hashes).
diffSynhashedDefns' ::
  TwoWay (Updated (DefnsF2 (Map Name) Synhashed Referent TypeReference)) ->
  ( -- Core diffs, i.e. adds, deletes, and updates which have different synhashes.
    TwoWay (DefnsF3 (Map Name) DiffOp Synhashed Referent TypeReference),
    -- Propagated updates, i.e. updates which have the same synhash but different Unison hashes.
    TwoWay (DefnsF (Map Name) (Updated Referent) (Updated TypeReference))
  )
diffSynhashedDefns' =
  Zip.unzip . fmap diffSynhashedDefns0'

diffSynhashedDefns0 ::
  (Eq term, Eq typ) =>
  DefnsF2 (Map Name) Synhashed term typ ->
  DefnsF2 (Map Name) Synhashed term typ ->
  ( -- Core diffs, i.e. adds, deletes, and updates which have different synhashes.
    DefnsF3 (Map Name) DiffOp Synhashed term typ,
    -- Propagated updates, i.e. updates which have the same synhash but different Unison hashes.
    DefnsF (Map Name) (Updated term) (Updated typ)
  )
diffSynhashedDefns0 old new =
  unzipDefns (zipDefnsWith f f old new)
  where
    f ::
      (Eq ref) =>
      Map Name (Synhashed ref) ->
      Map Name (Synhashed ref) ->
      (Map Name (DiffOp (Synhashed ref)), Map Name (Updated ref))
    f old new =
      partitionPropagated (diffSynhashedDefns1 old new)

diffSynhashedDefns0' ::
  (Eq term, Eq typ) =>
  Updated (DefnsF2 (Map Name) Synhashed term typ) ->
  ( -- Core diffs, i.e. adds, deletes, and updates which have different synhashes.
    DefnsF3 (Map Name) DiffOp Synhashed term typ,
    -- Propagated updates, i.e. updates which have the same synhash but different Unison hashes.
    DefnsF (Map Name) (Updated term) (Updated typ)
  )
diffSynhashedDefns0' defns =
  unzipDefns (zipDefnsWith f f defns.old defns.new)
  where
    f ::
      (Eq ref) =>
      Map Name (Synhashed ref) ->
      Map Name (Synhashed ref) ->
      (Map Name (DiffOp (Synhashed ref)), Map Name (Updated ref))
    f old new =
      partitionPropagated (diffSynhashedDefns1 old new)

-- Compute the diff by comparing old-and-new values, resulting in either an add, delete, update (propagated or not),
-- or dropping the thing entirely (because old and new have the same hash).
diffSynhashedDefns1 ::
  forall ref.
  (Eq ref) =>
  Map Name (Synhashed ref) ->
  Map Name (Synhashed ref) ->
  Map Name (DiffOp2 (Synhashed ref))
diffSynhashedDefns1 =
  Map.merge
    (Map.mapMissing \_ -> DiffOp2'Delete)
    (Map.mapMissing \_ -> DiffOp2'Add)
    (Map.zipWithMaybeMatched \_ -> f)
  where
    f :: Synhashed ref -> Synhashed ref -> Maybe (DiffOp2 (Synhashed ref))
    f old new = do
      let equalSynhashes = old == new
      -- Drop things that haven't changed
      when equalSynhashes do
        guard (Synhashed.value old /= Synhashed.value new)
      Just (DiffOp2'Update Updated {old, new} equalSynhashes)

-- Partition add/delete/update/propagated-update into add/delete/update + propagated-update
partitionPropagated :: Map Name (DiffOp2 (Synhashed ref)) -> (Map Name (DiffOp (Synhashed ref)), Map Name (Updated ref))
partitionPropagated =
  Map.mapEither \case
    DiffOp2'Add ref -> Left (DiffOp'Add ref)
    DiffOp2'Delete ref -> Left (DiffOp'Delete ref)
    DiffOp2'Update refs propagated
      | propagated -> Right (Updated.map Synhashed.value refs)
      | otherwise -> Left (DiffOp'Update refs)

-- | Post-process a diff to identify relationships humans might care about, such as whether a given addition could be
-- interpreted as an alias of an existing definition, or whether an add and deletion could be a rename.
humanizeDiffs ::
  ThreeWay Names ->
  TwoWay (DefnsF3 (Map Name) DiffOp Synhashed Referent TypeReference) ->
  TwoWay (DefnsF (Map Name) (Updated Referent) (Updated TypeReference)) ->
  TwoWay (DefnsF2 (Map Name) HumanDiffOp Referent TypeReference)
humanizeDiffs names3 =
  let names3' = names3 <&> \names -> Defns names.terms names.types
   in zipWith3
        (Defns.zipDefnsWith4 computeHumanDiffOp computeHumanDiffOp names3'.lca)
        (ThreeWay.forgetLca names3')
  where
    zipWith3 :: (Zip.Zip f) => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
    zipWith3 f a b =
      Zip.zipWith (uncurry f) (Zip.zip a b)

    computeHumanDiffOp ::
      forall ref.
      (Show ref, Ord ref) =>
      Relation Name ref ->
      Relation Name ref ->
      Map Name (DiffOp (Synhashed ref)) ->
      Map Name (Updated ref) ->
      Map Name (HumanDiffOp ref)
    computeHumanDiffOp oldNamespace newNamespace =
      alignWith \case
        This diff -> humanizeDiffOp (DiffOp.map Synhashed.value diff)
        That updated -> HumanDiffOp'PropagatedUpdate updated
        These diff updated ->
          error $
            reportBug
              "E488729"
              ( "The impossible happened, an update in merge was detected as both a propagated AND core update "
                  ++ show diff
                  ++ " and "
                  ++ show updated
              )
      where
        humanizeDiffOp :: DiffOp ref -> HumanDiffOp ref
        humanizeDiffOp = \case
          DiffOp'Add ref ->
            -- This name is newly added. We need to check if it's a new definition, an alias, or a rename.
            case Set.toList (Rel.lookupRan ref oldNamespace) of
              -- No old names for this ref, so it's a new addition not an alias
              [] -> HumanDiffOp'Add ref
              -- There are old names for this ref, but not old refs for this name, so it's
              -- either a new alias or a rename.
              --
              -- If at least one old name for this ref no longer exists, we treat it like a
              -- rename.
              n : ns -> do
                let existingNames = NESet.fromList (n NEList.:| ns)
                case NESet.nonEmptySet (Rel.lookupRan ref newNamespace) of
                  Nothing -> error (reportBug "E458329" ("Expected to find at least one name for ref in new namespace, since we found the ref by the name."))
                  Just allNewNames ->
                    case NESet.nonEmptySet (NESet.difference existingNames allNewNames) of
                      -- If all the old names still exist in the new namespace, it's a new alias.
                      Nothing -> HumanDiffOp'AliasOf ref existingNames
                      -- Otherwise, treat it as a rename.
                      Just namesWhichDisappeared ->
                        HumanDiffOp'RenamedFrom ref namesWhichDisappeared
          DiffOp'Delete ref ->
            case NEL.nonEmpty $ Set.toList (Rel.lookupRan ref newNamespace) of
              -- No names for this ref, it was removed.
              Nothing -> HumanDiffOp'Delete ref
              Just newNames -> HumanDiffOp'RenamedTo ref (NESet.fromList newNames)
          DiffOp'Update Updated {old, new} -> HumanDiffOp'Update Updated {old, new}

------------------------------------------------------------------------------------------------------------------------
-- Looking up terms and decls that we expect to be there

expectTerm :: (HasCallStack) => TermReferenceId -> Map TermReferenceId term -> term
expectTerm ref termsById =
  case Map.lookup ref termsById of
    Nothing -> error (reportBug "E488229" ("term ref " ++ show ref ++ " not found in map"))
    Just term -> term

expectDecl :: (HasCallStack) => TypeReferenceId -> Map TypeReferenceId (Decl Symbol Ann) -> Decl Symbol Ann
expectDecl ref declsById =
  case Map.lookup ref declsById of
    Nothing -> error (reportBug "E663160" ("type ref " ++ show ref ++ " not found in map " ++ show declsById))
    Just decl -> decl
