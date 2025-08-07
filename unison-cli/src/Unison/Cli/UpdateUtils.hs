-- | This module contains functionality that is common to the general idea of "updating" a term in Unison, which is when
-- we reassign a name from one hash to another and then see if all dependents still typecheck.
--
-- This occurs in the `pull`, `merge`, `update`, and `upgrade` commands.
module Unison.Cli.UpdateUtils
  ( -- * Getting dependents in a namespace
    getNamespaceDependentsOf,
    getNamespaceDependentsOf2,

    -- * Hydrating definitions
    hydrateRefs,
    nameHydratedRefIds,

    -- * Parsing and typechecking
    parseAndTypecheck,
  )
where

import Control.Monad.Reader (ask)
import Data.Bifoldable (bifoldMap)
import Data.Bitraversable (bitraverse)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import U.Codebase.Reference (TermReferenceId, TypeReferenceId)
import U.Codebase.Sqlite.Operations qualified as Operations
import Unison.Cli.Monad (Cli, Env (..))
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.TypeCheck (computeTypecheckingEnvironment)
import Unison.Debug qualified as Debug
import Unison.FileParsers qualified as FileParsers
import Unison.Hash (Hash)
import Unison.Name (Name)
import Unison.Names (Names)
import Unison.Names qualified as Names
import Unison.Parser.Ann (Ann)
import Unison.Parsers qualified as Parsers
import Unison.Prelude
import Unison.Reference (Reference, TypeReference)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Result qualified as Result
import Unison.Sqlite (Transaction)
import Unison.Symbol (Symbol)
import Unison.Syntax.Parser qualified as Parser
import Unison.UnisonFile (TypecheckedUnisonFile)
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Defns (Defns (..), DefnsF, zipDefnsWith)
import Unison.Util.Map qualified as Map (thenInsertPair)
import Unison.Util.Pretty (Pretty)
import Unison.Util.Pretty qualified as Pretty
import Unison.Util.Relation (Relation)
import Unison.Util.Relation qualified as Relation
import Unison.Util.Set qualified as Set
import Prelude hiding (unzip, zip, zipWith)

------------------------------------------------------------------------------------------------------------------------
-- Getting dependents in a namespace

-- | Given a namespace and a set of dependencies, return the subset of the namespace that consists of only the
-- (transitive) dependents of the dependencies.
getNamespaceDependentsOf ::
  Names ->
  Set Reference ->
  Transaction (DefnsF (Relation Name) TermReferenceId TypeReferenceId)
getNamespaceDependentsOf names dependencies = do
  dependents <- Operations.transitiveDependentsWithinScope (Names.referenceIds names) dependencies
  pure (bimap (foldMap nameTerm) (foldMap nameType) dependents)
  where
    nameTerm :: TermReferenceId -> Relation Name TermReferenceId
    nameTerm ref =
      Relation.fromManyDom (Relation.lookupRan (Referent.fromTermReferenceId ref) (Names.terms names)) ref

    nameType :: TypeReferenceId -> Relation Name TypeReferenceId
    nameType ref =
      Relation.fromManyDom (Relation.lookupRan (Reference.fromId ref) (Names.types names)) ref

-- | Given a namespace and a set of dependencies, return the subset of the namespace that consists of only the
-- (transitive) dependents of the dependencies.
getNamespaceDependentsOf2 ::
  Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
  Set Reference ->
  Transaction (DefnsF (Map Name) TermReferenceId TypeReferenceId)
getNamespaceDependentsOf2 defns dependencies = do
  let toTermScope = Set.mapMaybe Referent.toReferenceId . BiMultimap.dom
  let toTypeScope = Set.mapMaybe Reference.toId . BiMultimap.dom
  let scope = bifoldMap toTermScope toTypeScope defns
  Operations.transitiveDependentsWithinScope scope dependencies
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

-- -- | Given a namespace and a set of dependencies, return the subset of the namespace that consists of only the
-- -- (transitive) dependents of the dependencies.
-- getNamespaceDependentsOf3 ::
--   Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
--   DefnsF Set TermReference TypeReference ->
--   Transaction (DefnsF Set TermReferenceId TypeReferenceId)
-- getNamespaceDependentsOf3 defns dependencies = do
--   let toTermScope = Set.mapMaybe Referent.toReferenceId . BiMultimap.dom
--   let toTypeScope = Set.mapMaybe Reference.toId . BiMultimap.dom
--   let scope = bifoldMap toTermScope toTypeScope defns
--   Operations.transitiveDependentsWithinScope scope (bifold dependencies)

------------------------------------------------------------------------------------------------------------------------
-- Hydrating definitions

-- | Hydrate term/type references to actual terms/types.
hydrateRefs ::
  (Monad m) =>
  (Hash -> m [term]) ->
  (Hash -> m [typ]) ->
  DefnsF Set TermReferenceId TypeReferenceId ->
  m (Defns (Map TermReferenceId term) (Map TypeReferenceId typ))
hydrateRefs getTermComponent getTypeComponent =
  bitraverse (hydrateRefs1 getTermComponent) (hydrateRefs1 getTypeComponent)

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

------------------------------------------------------------------------------------------------------------------------
-- Parsing and typechecking

-- TODO: find a better module for this function, as it's used in a couple places
parseAndTypecheck ::
  Pretty Pretty.ColorText ->
  Parser.ParsingEnv Transaction ->
  Cli (Maybe (TypecheckedUnisonFile Symbol Ann))
parseAndTypecheck prettyUf parsingEnv = do
  env <- ask
  let stringUf = Pretty.toPlain 80 prettyUf
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
