module Unison.Merge.Diff
  ( oldNameBasedNamespaceDiff,
    nameBasedNamespaceDiff,
  )
where

import Data.Bitraversable (bitraverse)
import Data.Map.Strict qualified as Map
import Data.Semialign (alignWith)
import Data.Set qualified as Set
import Data.These (These (..))
import U.Codebase.Reference (TypeReference)
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.DataDeclaration (Decl)
import Unison.DataDeclaration qualified as DataDeclaration
import Unison.Hash (Hash (Hash))
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Merge.Database (MergeDatabase (..))
import Unison.Merge.DeclNameLookup (DeclNameLookup)
import Unison.Merge.DeclNameLookup qualified as DeclNameLookup
import Unison.Merge.DiffOp (DiffOp (..))
import Unison.Merge.PartialDeclNameLookup (PartialDeclNameLookup (..))
import Unison.Merge.Synhash
import Unison.Merge.Synhashed (Synhashed (..))
import Unison.Merge.ThreeWay (ThreeWay (..))
import Unison.Merge.ThreeWay qualified as ThreeWay
import Unison.Merge.TwoWay (TwoWay (..))
import Unison.Merge.Updated (Updated (..))
import Unison.Name (Name)
import Unison.Parser.Ann (Ann)
import Unison.Prelude hiding (catMaybes)
import Unison.PrettyPrintEnv (PrettyPrintEnv (..))
import Unison.PrettyPrintEnv qualified as Ppe
import Unison.Reference (Reference' (..), TermReference, TermReferenceId, TypeReferenceId)
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Sqlite (Transaction)
import Unison.Symbol (Symbol)
import Unison.Syntax.Name qualified as Name
import Unison.Term (Term)
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Defns (Defns (..), DefnsF2, DefnsF3, zipDefnsWith)

-- | @nameBasedNamespaceDiff db declNameLookups defns@ returns Alice's and Bob's name-based namespace diffs, each in the
-- form:
--
-- > terms :: Map Name (DiffOp (Synhashed Referent))
-- > types :: Map Name (DiffOp (Synhashed TypeReference))
--
-- where each name is paired with its diff-op (added, deleted, or updated), relative to the LCA between Alice and Bob's
-- branches. If the hash of a name did not change, it will not appear in the map.
oldNameBasedNamespaceDiff ::
  MergeDatabase ->
  TwoWay DeclNameLookup ->
  PartialDeclNameLookup ->
  ThreeWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
  Transaction (TwoWay (DefnsF3 (Map Name) DiffOp Synhashed Referent TypeReference))
oldNameBasedNamespaceDiff db declNameLookups lcaDeclNameLookup defns = do
  lcaHashes <- synhashLcaDefns db ppe lcaDeclNameLookup defns.lca
  hashes <- sequence (synhashDefns db ppe <$> declNameLookups <*> ThreeWay.forgetLca defns)
  pure (diffNamespaceDefns lcaHashes <$> hashes)
  where
    ppe :: PrettyPrintEnv
    ppe =
      -- The order between Alice and Bob isn't important here for syntactic hashing; not sure right now if it matters
      -- that the LCA is added last
      deepNamespaceDefinitionsToPpe defns.alice
        `Ppe.addFallback` deepNamespaceDefinitionsToPpe defns.bob
        `Ppe.addFallback` deepNamespaceDefinitionsToPpe defns.lca

-- | @nameBasedNamespaceDiff db declNameLookups defns@ returns Alice's and Bob's name-based namespace diffs, each in the
-- form:
--
-- > terms :: Map Name (DiffOp (Synhashed Referent))
-- > types :: Map Name (DiffOp (Synhashed TypeReference))
--
-- where each name is paired with its diff-op (added, deleted, or updated), relative to the LCA between Alice and Bob's
-- branches. If the hash of a name did not change, it will not appear in the map.
nameBasedNamespaceDiff ::
  TwoWay DeclNameLookup ->
  PartialDeclNameLookup ->
  ThreeWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
  Defns (Map TermReferenceId (Term Symbol Ann)) (Map TypeReferenceId (Decl Symbol Ann)) ->
  TwoWay (DefnsF3 (Map Name) DiffOp Synhashed Referent TypeReference)
nameBasedNamespaceDiff declNameLookups lcaDeclNameLookup defns hydratedDefns =
  let lcaHashes = synhashLcaDefns2 ppe lcaDeclNameLookup defns.lca hydratedDefns
      hashes = synhashDefns2 ppe hydratedDefns <$> declNameLookups <*> ThreeWay.forgetLca defns
   in diffNamespaceDefns lcaHashes <$> hashes
  where
    ppe :: PrettyPrintEnv
    ppe =
      -- The order between Alice and Bob isn't important here for syntactic hashing; not sure right now if it matters
      -- that the LCA is added last
      deepNamespaceDefinitionsToPpe defns.alice
        `Ppe.addFallback` deepNamespaceDefinitionsToPpe defns.bob
        `Ppe.addFallback` deepNamespaceDefinitionsToPpe defns.lca

synhashLcaDefns ::
  MergeDatabase ->
  PrettyPrintEnv ->
  PartialDeclNameLookup ->
  Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
  Transaction (DefnsF2 (Map Name) Synhashed Referent TypeReference)
synhashLcaDefns db ppe declNameLookup =
  synhashDefnsWith hashReferent hashType
  where
    -- For the LCA only, if we don't have a name for every constructor, or we don't have a name for a decl, that's okay,
    -- just use a dummy syntactic hash (e.g. where we return `Hash mempty` below in two places).
    --
    -- This is safe and correct; Alice/Bob can't have such a decl (it violates a merge precondition), so there's no risk
    -- that we accidentally get an equal hash and classify a real update as unchanged.

    hashReferent :: Name -> Referent -> Transaction Hash
    hashReferent name = \case
      Referent.Con (ConstructorReference ref _) _ ->
        case Map.lookup name declNameLookup.constructorToDecl of
          Nothing -> pure (Hash mempty) -- see note above
          Just declName -> hashType declName ref
      Referent.Ref ref -> synhashTerm db.loadV1Term ppe ref

    hashType :: Name -> TypeReference -> Transaction Hash
    hashType name = \case
      ReferenceBuiltin builtin -> pure (synhashBuiltinDecl builtin)
      ReferenceDerived ref ->
        case sequence (declNameLookup.declToConstructors Map.! name) of
          Nothing -> pure (Hash mempty) -- see note above
          Just names -> do
            decl <- loadDeclWithGoodConstructorNames db names ref
            pure (synhashDerivedDecl ppe name decl)

synhashLcaDefns2 ::
  PrettyPrintEnv ->
  PartialDeclNameLookup ->
  Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
  Defns (Map TermReferenceId (Term Symbol Ann)) (Map TypeReferenceId (Decl Symbol Ann)) ->
  DefnsF2 (Map Name) Synhashed Referent TypeReference
synhashLcaDefns2 ppe declNameLookup defns hydratedDefns =
  synhashDefnsWith2 hashReferent hashType defns
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
      Referent.Ref ref -> hashTermReference ppe hydratedDefns.terms ref

    hashType :: Name -> TypeReference -> Hash
    hashType name = \case
      ReferenceBuiltin builtin -> synhashBuiltinDecl builtin
      ReferenceDerived ref ->
        case sequence (declNameLookup.declToConstructors Map.! name) of
          Nothing -> Hash mempty -- see note above
          Just names -> hashDerivedDecl ppe hydratedDefns.types names name ref

synhashDefns2 ::
  PrettyPrintEnv ->
  Defns (Map TermReferenceId (Term Symbol Ann)) (Map TypeReferenceId (Decl Symbol Ann)) ->
  DeclNameLookup ->
  Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
  DefnsF2 (Map Name) Synhashed Referent TypeReference
synhashDefns2 ppe hydratedDefns declNameLookup =
  synhashDefnsWith2 hashReferent hashType
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
      Referent.Ref ref -> hashTermReference ppe hydratedDefns.terms ref

    hashType :: Name -> TypeReference -> Hash
    hashType name = \case
      ReferenceBuiltin builtin -> synhashBuiltinDecl builtin
      ReferenceDerived ref ->
        hashDerivedDecl ppe hydratedDefns.types (DeclNameLookup.expectConstructorNames declNameLookup name) name ref

hashDerivedDecl :: PrettyPrintEnv -> Map TypeReferenceId (Decl Symbol Ann) -> [Name] -> Name -> TypeReferenceId -> Hash
hashDerivedDecl ppe declsById names name ref =
  declsById
    & expectDecl ref
    & DataDeclaration.setConstructorNames (map Name.toVar names)
    & synhashDerivedDecl ppe name

hashTermReference :: PrettyPrintEnv -> Map TermReferenceId (Term Symbol Ann) -> TermReference -> Hash
hashTermReference ppe termsById = \case
  ReferenceBuiltin builtin -> synhashBuiltinTerm builtin
  ReferenceDerived ref -> synhashDerivedTerm ppe (expectTerm ref termsById)

synhashDefns ::
  MergeDatabase ->
  PrettyPrintEnv ->
  DeclNameLookup ->
  Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
  Transaction (DefnsF2 (Map Name) Synhashed Referent TypeReference)
synhashDefns db ppe declNameLookup =
  -- FIXME: use cache so we only synhash each thing once
  synhashDefnsWith hashReferent hashType
  where
    hashReferent :: Name -> Referent -> Transaction Hash
    hashReferent name = \case
      -- We say that a referent constructor *in the namespace* (distinct from a referent that is in a term body) has a
      -- synhash that is simply equal to the synhash of its type declaration. This is because the type declaration and
      -- constructors are changed in lock-step: it is not possible to change one, but not the other.
      --
      -- For example, if Alice updates `type Foo = Bar Nat` to `type Foo = Bar Nat Nat`, we want different synhashes on
      -- both the type (Foo) and the constructor (Foo.Bar).
      Referent.Con (ConstructorReference ref _) _ -> hashType (DeclNameLookup.expectDeclName declNameLookup name) ref
      Referent.Ref ref -> synhashTerm db.loadV1Term ppe ref

    hashType :: Name -> TypeReference -> Transaction Hash
    hashType name = \case
      ReferenceBuiltin builtin -> pure (synhashBuiltinDecl builtin)
      ReferenceDerived ref -> do
        decl <- loadDeclWithGoodConstructorNames db (DeclNameLookup.expectConstructorNames declNameLookup name) ref
        pure (synhashDerivedDecl ppe name decl)

loadDeclWithGoodConstructorNames :: MergeDatabase -> [Name] -> TypeReferenceId -> Transaction (Decl Symbol Ann)
loadDeclWithGoodConstructorNames db names =
  fmap (DataDeclaration.setConstructorNames (map Name.toVar names)) . db.loadV1Decl

diffNamespaceDefns ::
  DefnsF2 (Map Name) Synhashed term typ ->
  DefnsF2 (Map Name) Synhashed term typ ->
  DefnsF3 (Map Name) DiffOp Synhashed term typ
diffNamespaceDefns =
  zipDefnsWith f f
  where
    f :: Map Name (Synhashed ref) -> Map Name (Synhashed ref) -> Map Name (DiffOp (Synhashed ref))
    f old new =
      Map.mapMaybe id (alignWith g old new)

    g :: (Eq x) => These x x -> Maybe (DiffOp x)
    g = \case
      This old -> Just (DiffOp'Delete old)
      That new -> Just (DiffOp'Add new)
      These old new
        | old == new -> Nothing
        | otherwise -> Just (DiffOp'Update Updated {old, new})

------------------------------------------------------------------------------------------------------------------------
-- Pretty-print env helpers

deepNamespaceDefinitionsToPpe :: Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) -> PrettyPrintEnv
deepNamespaceDefinitionsToPpe Defns {terms, types} =
  PrettyPrintEnv (arbitraryName terms) (arbitraryName types)
  where
    arbitraryName :: (Ord ref) => BiMultimap ref Name -> ref -> [(HQ'.HashQualified Name, HQ'.HashQualified Name)]
    arbitraryName names ref =
      BiMultimap.lookupDom ref names
        & Set.lookupMin
        & maybe [] \name -> [(HQ'.NameOnly name, HQ'.NameOnly name)]

------------------------------------------------------------------------------------------------------------------------
-- Syntactic hashing helpers

synhashDefnsWith ::
  (Monad m) =>
  (Name -> term -> m Hash) ->
  (Name -> typ -> m Hash) ->
  Defns (BiMultimap term Name) (BiMultimap typ Name) ->
  m (DefnsF2 (Map Name) Synhashed term typ)
synhashDefnsWith hashTerm hashType = do
  bitraverse
    (Map.traverseWithKey hashTerm1 . BiMultimap.range)
    (Map.traverseWithKey hashType1 . BiMultimap.range)
  where
    hashTerm1 name term = do
      hash <- hashTerm name term
      pure (Synhashed hash term)

    hashType1 name typ = do
      hash <- hashType name typ
      pure (Synhashed hash typ)

synhashDefnsWith2 ::
  (Name -> term -> Hash) ->
  (Name -> typ -> Hash) ->
  Defns (BiMultimap term Name) (BiMultimap typ Name) ->
  DefnsF2 (Map Name) Synhashed term typ
synhashDefnsWith2 hashTerm hashType = do
  bimap
    (Map.mapWithKey hashTerm1 . BiMultimap.range)
    (Map.mapWithKey hashType1 . BiMultimap.range)
  where
    hashTerm1 name term =
      Synhashed (hashTerm name term) term

    hashType1 name typ =
      Synhashed (hashType name typ) typ

------------------------------------------------------------------------------------------------------------------------
-- Looking up terms and decls that we expect to be there

expectTerm :: TermReferenceId -> Map TermReferenceId (Term Symbol Ann) -> Term Symbol Ann
expectTerm ref termsById =
  case Map.lookup ref termsById of
    Nothing -> error (reportBug "E488229" ("term ref " ++ show ref ++ " not found in map " ++ show termsById))
    Just term -> term

expectDecl :: TypeReferenceId -> Map TypeReferenceId (Decl Symbol Ann) -> Decl Symbol Ann
expectDecl ref declsById =
  case Map.lookup ref declsById of
    Nothing -> error (reportBug "E663160" ("type ref " ++ show ref ++ " not found in map " ++ show declsById))
    Just decl -> decl
