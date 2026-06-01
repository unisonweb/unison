-- | This module contains functionality that is common to the general idea of "updating" a term in Unison, which is when
-- we reassign a name from one hash to another and then see if all dependents still typecheck.
--
-- This occurs in the `pull`, `merge`, `update`, and `upgrade` commands.
module Unison.Cli.UpdateUtils
  ( -- * Getting dependents in a namespace
    getNamespaceDependentsOf,
    subtractDependents,

    -- * Hydrating definitions
    hydrateRefs,
    hydrateAliases,
    nameHydratedRefIds,
    nameHydratedRefIds2,

    -- * Alias-to-alias update propagation
    aliasDependentsInOrder,
    propagateAliasUpdates,

    -- * Unique type guids
    makeUniqueTypeGuids,

    -- * Parsing and typechecking
    parseAndTypecheck,
  )
where

import Control.Monad.Reader (ask)
import Data.Bitraversable (bitraverse)
import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import U.Codebase.Decl qualified as V2.Decl
import U.Codebase.Reference (Reference' (..), TermReferenceId, TypeReferenceId)
import U.Codebase.Sqlite.Operations qualified as Operations
import Unison.Cli.Monad (Cli, Env (..))
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.TypeCheck (computeTypecheckingEnvironment)
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.DataDeclaration (Decl)
import Unison.Debug qualified as Debug
import Unison.FileParsers qualified as FileParsers
import Unison.Hash (Hash)
import Unison.Name (Name)
import Unison.Names qualified as Names
import Unison.Parser.Ann (Ann)
import Unison.Parsers qualified as Parsers
import Unison.Prelude
import Unison.Reference (TermReference, TypeReference)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Result qualified as Result
import Unison.Sqlite (Transaction)
import Unison.Symbol (Symbol)
import Unison.Syntax.Parser qualified as Parser
import Unison.Term (Term)
import Unison.Codebase.Branch (Branch0)
import Unison.Codebase.BranchUtil qualified as BranchUtil
import Unison.Codebase.Path (Path)
import Unison.Codebase.Path qualified as Path
import Unison.Hashing.V2.Convert qualified as Hashing
import Unison.Type (Type)
import Unison.Type qualified as Type
import Unison.TypeAlias (TypeAlias)
import Unison.TypeAlias qualified as TypeAlias
import Unison.UnisonFile (TypecheckedUnisonFile)
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Defns (Defns (..), DefnsF, zipDefnsWith)
import Unison.Util.Map qualified as Map (thenInsertPair)
import Unison.Util.Pretty (Pretty)
import Unison.Util.Pretty qualified as Pretty
import Unison.Util.Set qualified as Set
import Prelude hiding (unzip, zip, zipWith)

------------------------------------------------------------------------------------------------------------------------
-- Getting dependents in a namespace

-- | Given an unconflicted namespace and a set of dependencies, return the subset of the namespace that consists of only
-- the (transitive) dependents of the dependencies.
getNamespaceDependentsOf ::
  Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
  DefnsF Set TermReference TypeReference ->
  Transaction (DefnsF (Map Name) TermReferenceId TypeReferenceId)
getNamespaceDependentsOf defns dependencies = do
  Operations.transitiveDependentsWithinScope (Names.unconflictedReferenceIds defns) dependencies
    <&> bimap (Set.foldl' addTerms Map.empty) (Set.foldl' addTypes Map.empty)
  where
    addTerms :: Map Name TermReferenceId -> TermReferenceId -> Map Name TermReferenceId
    addTerms acc0 ref =
      let names = BiMultimap.lookupDom (Referent.fromTermReferenceId ref) defns.terms
       in Set.foldl' (\acc name -> Map.insert name ref acc) acc0 names

    addTypes :: Map Name TypeReferenceId -> TypeReferenceId -> Map Name TypeReferenceId
    addTypes acc0 ref =
      let names = BiMultimap.lookupDom (Reference.fromId ref) defns.types
       in Set.foldl' (\acc name -> Map.insert name ref acc) acc0 names

subtractDependents ::
  DefnsF Set TermReferenceId TypeReferenceId ->
  DefnsF (Map Name) Referent TypeReference ->
  DefnsF (Map Name) Referent TypeReference
subtractDependents dependents =
  bimap (Map.filter keepTerm) (Map.filter keepType)
  where
    keepType :: TypeReference -> Bool
    keepType = \case
      ReferenceBuiltin _ -> True
      ReferenceDerived refId -> not (Set.member refId dependents.types)
    keepTerm :: Referent -> Bool
    keepTerm = \case
      Referent.Con (ConstructorReference ref _) _ -> keepType ref
      Referent.Ref ref ->
        case ref of
          ReferenceBuiltin _ -> True
          ReferenceDerived refId -> not (Set.member refId dependents.terms)

------------------------------------------------------------------------------------------------------------------------
-- Hydrating definitions

-- | Hydrate term/type references to actual terms/types. Alias type refs
-- are silently skipped — callers that need to handle alias dependents
-- should load them via 'hydrateAliases'.
hydrateRefs ::
  Codebase m Symbol Ann ->
  DefnsF Set TermReferenceId TypeReferenceId ->
  Transaction (Defns (Map TermReferenceId (Term Symbol Ann, Type Symbol Ann)) (Map TypeReferenceId (Decl Symbol Ann)))
hydrateRefs codebase refs = do
  declRefs <- Set.foldCommutativeM keepDecl Set.empty refs.types
  bitraverse
    (hydrateRefs1 (Codebase.unsafeGetTermComponent codebase))
    (hydrateRefs1 (Codebase.expectTypeDeclarationComponent codebase))
    Defns {terms = refs.terms, types = declRefs}
  where
    keepDecl ref acc =
      Codebase.isTypeAlias codebase (Reference.fromId ref) <&> \case
        True -> acc
        False -> Set.insert ref acc

-- | Hydrate type alias references. Decl refs are silently skipped.
hydrateAliases ::
  Codebase m Symbol Ann ->
  Set TypeReferenceId ->
  Transaction (Map TypeReferenceId (TypeAlias Symbol Ann))
hydrateAliases codebase = Set.foldCommutativeM step Map.empty
  where
    step ref acc =
      Codebase.getTypeAlias codebase ref <&> \case
        Just ta -> Map.insert ref ta acc
        Nothing -> acc

hydrateRefs1 ::
  forall defn m.
  (Monad m) =>
  (Hash -> m [defn]) ->
  Set Reference.Id ->
  m (Map Reference.Id defn)
hydrateRefs1 getComponent =
  Set.foldCommutativeM f Map.empty . Set.map Reference.idToHash
  where
    f :: Hash -> Map Reference.Id defn -> m (Map Reference.Id defn)
    f hash acc =
      List.foldl' Map.thenInsertPair acc . Reference.componentFor hash <$> getComponent hash

-- | Re-emit alias dependents whose bodies reference a ref that's being
-- updated. Walks the aliases in dependency order: for each, substitutes
-- known old refs with their new versions, re-hashes, and persists the
-- new alias. The accumulated old-to-new substitutions cascade so an
-- alias that depends on another updated alias picks up the new ref.
--
-- Returns @(substitutions, branchUpdates)@ where @branchUpdates@ rebinds
-- each alias name to its new ref. @substitutions@ extends the initial
-- map with each new (oldRef, newRef) pair.
propagateAliasUpdates ::
  forall m.
  Codebase IO Symbol Ann ->
  -- | (alias name, old ref id, alias body)
  [(Name, TypeReferenceId, TypeAlias Symbol Ann)] ->
  -- | Initial old-to-new substitutions (typically from the user's file).
  Map TypeReference TypeReference ->
  Transaction (Map TypeReference TypeReference, [(Path, Branch0 m -> Branch0 m)])
propagateAliasUpdates codebase aliases initialSubsts =
  foldM step (initialSubsts, []) aliases
  where
    step ::
      (Map TypeReference TypeReference, [(Path, Branch0 m -> Branch0 m)]) ->
      (Name, TypeReferenceId, TypeAlias Symbol Ann) ->
      Transaction (Map TypeReference TypeReference, [(Path, Branch0 m -> Branch0 m)])
    step (substs, updates) (name, oldRefId, oldAlias) = do
      let newBody = Type.updateDependencies substs oldAlias.body
          newAlias = oldAlias {TypeAlias.body = newBody}
          newRefId = Hashing.hashTypeAlias newAlias
          oldRef = Reference.fromId oldRefId
          newRef = Reference.fromId newRefId
          split = Path.splitFromName name
      Codebase.putTypeAlias codebase newRefId newAlias
      pure
        ( Map.insert oldRef newRef substs,
          updates
            ++ [ BranchUtil.makeAnnihilateTypeName split,
                 BranchUtil.makeAddTypeName split newRef
               ]
        )

-- | Sort alias dependents in dependency order so each appears after the
-- aliases it references. Dependents that don't reference any other
-- alias in the input are first.
aliasDependentsInOrder ::
  Map TypeReferenceId (Name, TypeAlias Symbol Ann) ->
  [(Name, TypeReferenceId, TypeAlias Symbol Ann)]
aliasDependentsInOrder aliases =
  reverse (snd (List.foldl' (visit Set.empty) (Set.empty, []) (Map.keys aliases)))
  where
    refsHere :: Set TypeReferenceId
    refsHere = Map.keysSet aliases

    visit ::
      Set TypeReferenceId ->
      (Set TypeReferenceId, [(Name, TypeReferenceId, TypeAlias Symbol Ann)]) ->
      TypeReferenceId ->
      (Set TypeReferenceId, [(Name, TypeReferenceId, TypeAlias Symbol Ann)])
    visit inProgress (done, acc) ref
      | Set.member ref done || Set.member ref inProgress = (done, acc)
      | otherwise =
          case Map.lookup ref aliases of
            Nothing -> (done, acc)
            Just (name, alias) ->
              let inProgress' = Set.insert ref inProgress
                  deps = Set.toList (Set.intersection refsHere (Set.mapMaybe Reference.toId (TypeAlias.dependencies alias)))
                  (done', acc') = List.foldl' (visit inProgress') (done, acc) deps
               in (Set.insert ref done', (name, ref, alias) : acc')

-- | Associate names with hydrated terms/types.
nameHydratedRefIds ::
  DefnsF (Map name) TermReferenceId TypeReferenceId ->
  Defns (Map TermReferenceId term) (Map TypeReferenceId typ) ->
  DefnsF (Map name) (TermReferenceId, term) (TypeReferenceId, typ)
nameHydratedRefIds =
  zipDefnsWith f f
  where
    f :: Map name Reference.Id -> Map Reference.Id defn -> Map name (Reference.Id, defn)
    f nameToRef refToDefn =
      Map.mapMaybe (\ref -> (ref,) <$> Map.lookup ref refToDefn) nameToRef

-- | Like 'nameHydratedRefIds', but takes the entire namespace as a first argument, which includes constructors.
nameHydratedRefIds2 ::
  forall name term typ.
  (Ord name) =>
  Defns (BiMultimap Referent name) (BiMultimap TypeReference name) ->
  Defns (Map TermReferenceId term) (Map TypeReferenceId typ) ->
  DefnsF (Map name) (TermReferenceId, term) (TypeReferenceId, typ)
nameHydratedRefIds2 =
  zipDefnsWith (f Referent.fromTermReferenceId) (f Reference.fromId)
  where
    f ::
      forall defn ref refId.
      (Ord ref) =>
      (refId -> ref) ->
      BiMultimap ref name ->
      Map refId defn ->
      Map name (refId, defn)
    f toRef defns =
      Map.foldlWithKey' (g toRef defns) Map.empty

    g ::
      forall defn ref refId.
      (Ord ref) =>
      (refId -> ref) ->
      BiMultimap ref name ->
      Map name (refId, defn) ->
      refId ->
      defn ->
      Map name (refId, defn)
    g toRef defns acc ref defn =
      Map.union (Map.fromSet (\_ -> (ref, defn)) names) acc
      where
        names :: Set name
        names =
          BiMultimap.lookupDom (toRef ref) defns

------------------------------------------------------------------------------------------------------------------------
-- Unique type guids

-- Make a unique type name to guid mapping from definitions, by looking up each decl individually. Maybe there will be
-- a more efficient way to accomplish this some day, but this is how it works for now.
makeUniqueTypeGuids :: Map Name TypeReference -> Transaction (Map Name Text)
makeUniqueTypeGuids types = do
  let step :: Map TypeReferenceId Text -> TypeReferenceId -> Transaction (Map TypeReferenceId Text)
      step acc refId = do
        -- Type aliases share the type namespace slot with decls but have
        -- no unique-type guid. Skip them.
        isAlias <- Operations.isTypeAliasReference (Reference.fromId refId)
        if isAlias
          then pure acc
          else do
            decl <- Operations.expectDeclByReference refId
            pure case decl.modifier of
              V2.Decl.Unique guid -> Map.insert refId guid acc
              V2.Decl.Structural -> acc

  uniqueTypeGuidsByRef <-
    Foldable.foldlM step Map.empty (foldMap toRefIds types)

  let refToUniqueTypeGuid :: TypeReference -> Maybe Text
      refToUniqueTypeGuid = \case
        ReferenceDerived refId -> Map.lookup refId uniqueTypeGuidsByRef
        ReferenceBuiltin _ -> Nothing

  pure (Map.mapMaybe refToUniqueTypeGuid types)
  where
    toRefIds :: TypeReference -> Set TypeReferenceId
    toRefIds = \case
      ReferenceDerived refId -> Set.singleton refId
      ReferenceBuiltin _ -> Set.empty

------------------------------------------------------------------------------------------------------------------------
-- Parsing and typechecking

-- TODO: find a better module for this function, as it's used in a couple places
parseAndTypecheck ::
  Pretty Pretty.ColorText ->
  Parser.ParsingEnv Transaction ->
  Cli (Maybe (TypecheckedUnisonFile Symbol Ann))
parseAndTypecheck prettyUf parsingEnv = do
  env <- ask
  let stringUf = Text.unpack $ Pretty.toPlain 80 prettyUf
  Debug.whenDebug Debug.Update do
    liftIO do
      putStrLn "--- Scratch ---"
      putStrLn stringUf
  Cli.runTransaction do
    Parsers.parseFile "<update>" stringUf parsingEnv >>= \case
      Left _ -> pure Nothing
      Right uf -> do
        typecheckingEnv <-
          computeTypecheckingEnvironment (FileParsers.ShouldUseTndr'Yes parsingEnv) env.codebase [] uf
        pure (Result.result (FileParsers.synthesizeFile typecheckingEnv uf))
