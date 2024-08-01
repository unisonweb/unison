-- | This module contains functionality that is common to the general idea of "updating" a term in Unison, which is when
-- we reassign a name from one hash to another and then see if all dependents still typecheck.
--
-- This occurs in the `pull`, `merge`, `update`, and `upgrade` commands.
module Unison.Cli.UpdateUtils
  ( -- * Loading definitions
    loadNamespaceDefinitions,

    -- * Getting dependents in a namespace
    getNamespaceDependentsOf,
    getNamespaceDependentsOf2,

    -- * Narrowing definitions
    narrowDefns,

    -- * Hydrating definitions
    hydrateDefns,

    -- * Rendering definitions
    renderDefnsForUnisonFile,

    -- * Parsing and typechecking
    parseAndTypecheck,
  )
where

import Control.Lens (mapped, _1)
import Control.Monad.Reader (ask)
import Control.Monad.Writer (Writer)
import Control.Monad.Writer qualified as Writer
import Data.Bifoldable (bifoldMap)
import Data.Bitraversable (bitraverse)
import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.List.NonEmpty qualified as List.NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as Set.NonEmpty
import U.Codebase.Branch qualified as V2
import U.Codebase.Causal qualified
import U.Codebase.Reference (TermReferenceId, TypeReferenceId)
import U.Codebase.Referent qualified as V2
import U.Codebase.Sqlite.Operations qualified as Operations
import Unison.Builtin.Decls qualified as Builtin.Decls
import Unison.Cli.Monad (Cli, Env (..))
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.TypeCheck (computeTypecheckingEnvironment)
import Unison.ConstructorReference (ConstructorReference, GConstructorReference (..))
import Unison.DataDeclaration (Decl)
import Unison.Debug qualified as Debug
import Unison.FileParsers qualified as FileParsers
import Unison.Hash (Hash)
import Unison.HashQualified qualified as HQ
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Merge.DeclNameLookup (DeclNameLookup (..), expectConstructorNames)
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment)
import Unison.NameSegment qualified as NameSegment
import Unison.Parser.Ann (Ann)
import Unison.Parsers qualified as Parsers
import Unison.Prelude
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl (..))
import Unison.Reference (Reference, TypeReference)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Result qualified as Result
import Unison.Sqlite (Transaction)
import Unison.Symbol (Symbol)
import Unison.Syntax.DeclPrinter (AccessorName)
import Unison.Syntax.DeclPrinter qualified as DeclPrinter
import Unison.Syntax.Parser qualified as Parser
import Unison.Syntax.TermPrinter qualified as TermPrinter
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Typechecker qualified as Typechecker
import Unison.UnisonFile (TypecheckedUnisonFile)
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Conflicted (Conflicted (..))
import Unison.Util.Defn (Defn (..))
import Unison.Util.Defns (Defns (..), DefnsF, DefnsF2)
import Unison.Util.Nametree (Nametree (..), traverseNametreeWithName, unflattenNametrees)
import Unison.Util.Pretty (ColorText, Pretty)
import Unison.Util.Pretty qualified as Pretty
import Unison.Util.Relation (Relation)
import Unison.Util.Relation qualified as Relation
import Unison.Util.Set qualified as Set
import Unison.Var (Var)
import Prelude hiding (unzip, zip, zipWith)
import Unison.Names (Names)
import qualified Unison.Names as Names

------------------------------------------------------------------------------------------------------------------------
-- Loading definitions

-- Load all "namespace definitions" of a branch, which are all terms and type declarations *except* those defined
-- in the "lib" namespace.
--
-- Fails if there is a conflicted name.
loadNamespaceDefinitions ::
  forall m.
  (Monad m) =>
  (V2.Referent -> m Referent) ->
  V2.Branch m ->
  m
    ( Either
        (Defn (Conflicted Name Referent) (Conflicted Name TypeReference))
        (Nametree (DefnsF (Map NameSegment) Referent TypeReference))
    )
loadNamespaceDefinitions referent2to1 =
  fmap assertNamespaceHasNoConflictedNames . go (Map.delete NameSegment.libSegment)
  where
    go ::
      (forall x. Map NameSegment x -> Map NameSegment x) ->
      V2.Branch m ->
      m (Nametree (DefnsF2 (Map NameSegment) NESet Referent TypeReference))
    go f branch = do
      terms <- for branch.terms (fmap (Set.NonEmpty.fromList . List.NonEmpty.fromList) . traverse referent2to1 . Map.keys)
      let types = Map.map (Set.NonEmpty.unsafeFromSet . Map.keysSet) branch.types
      children <-
        for (f branch.children) \childCausal -> do
          child <- childCausal.value
          go id child
      pure Nametree {value = Defns {terms, types}, children}

-- | Assert that there are no unconflicted names in a namespace.
assertNamespaceHasNoConflictedNames ::
  Nametree (DefnsF2 (Map NameSegment) NESet Referent TypeReference) ->
  Either
    (Defn (Conflicted Name Referent) (Conflicted Name TypeReference))
    (Nametree (DefnsF (Map NameSegment) Referent TypeReference))
assertNamespaceHasNoConflictedNames =
  traverseNametreeWithName \segments defns -> do
    let toName segment =
          Name.fromReverseSegments (segment List.NonEmpty.:| segments)
    terms <-
      defns.terms & Map.traverseWithKey \segment ->
        assertUnconflicted (TermDefn . Conflicted (toName segment))
    types <-
      defns.types & Map.traverseWithKey \segment ->
        assertUnconflicted (TypeDefn . Conflicted (toName segment))
    pure Defns {terms, types}
  where
    assertUnconflicted :: (NESet ref -> x) -> NESet ref -> Either x ref
    assertUnconflicted conflicted refs
      | Set.NonEmpty.size refs == 1 = Right (Set.NonEmpty.findMin refs)
      | otherwise = Left (conflicted refs)

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

------------------------------------------------------------------------------------------------------------------------
-- Narrowing definitions

-- | "Narrow" a namespace that may contain conflicted names, resulting in either a failure (if we find a conflicted
-- name), or the narrowed nametree without conflicted names.
narrowDefns ::
  forall term typ.
  (Ord term, Ord typ) =>
  DefnsF (Relation Name) term typ ->
  Either
    ( Defn
        (Conflicted Name term)
        (Conflicted Name typ)
    )
    (Nametree (DefnsF (Map NameSegment) term typ))
narrowDefns =
  fmap unflattenNametrees
    . bitraverse
      (go (\name -> TermDefn . Conflicted name))
      (go (\name -> TypeDefn . Conflicted name))
  where
    go :: forall ref x. (Ord ref) => (Name -> NESet ref -> x) -> Relation Name ref -> Either x (Map Name ref)
    go conflicted =
      Map.traverseWithKey unconflicted . Relation.domain
      where
        unconflicted :: Name -> Set ref -> Either x ref
        unconflicted name refs0
          | Set.NonEmpty.size refs == 1 = Right (Set.NonEmpty.findMin refs)
          | otherwise = Left (conflicted name refs)
          where
            refs = Set.NonEmpty.unsafeFromSet refs0

------------------------------------------------------------------------------------------------------------------------
-- Hydrating definitions

-- | Hydrate term/type references to actual terms/types.
hydrateDefns ::
  forall m name term typ.
  (Monad m, Ord name) =>
  (Hash -> m [term]) ->
  (Hash -> m [typ]) ->
  DefnsF (Map name) TermReferenceId TypeReferenceId ->
  m (DefnsF (Map name) (TermReferenceId, term) (TypeReferenceId, typ))
hydrateDefns getTermComponent getTypeComponent = do
  bitraverse hydrateTerms hydrateTypes
  where
    hydrateTerms :: Map name TermReferenceId -> m (Map name (TermReferenceId, term))
    hydrateTerms terms =
      hydrateDefns_ getTermComponent terms \_ -> (,)

    hydrateTypes :: Map name TypeReferenceId -> m (Map name (TypeReferenceId, typ))
    hydrateTypes types =
      hydrateDefns_ getTypeComponent types \_ -> (,)

hydrateDefns_ ::
  forall a b name m.
  (Monad m, Ord name) =>
  (Hash -> m [a]) ->
  Map name Reference.Id ->
  (name -> Reference.Id -> a -> b) ->
  m (Map name b)
hydrateDefns_ getComponent defns modify =
  Foldable.foldlM f Map.empty (foldMap (Set.singleton . Reference.idToHash) defns)
  where
    f :: Map name b -> Hash -> m (Map name b)
    f acc hash =
      List.foldl' g acc . Reference.componentFor hash <$> getComponent hash

    g :: Map name b -> (Reference.Id, a) -> Map name b
    g acc (ref, thing) =
      Set.foldl' (h ref thing) acc (BiMultimap.lookupDom ref defns2)

    h :: Reference.Id -> a -> Map name b -> name -> Map name b
    h ref thing acc name =
      Map.insert name (modify name ref thing) acc

    defns2 :: BiMultimap Reference.Id name
    defns2 =
      BiMultimap.fromRange defns

------------------------------------------------------------------------------------------------------------------------
-- Rendering definitions

-- | Render definitions destined for a Unison file.
--
-- This first renders the types (discovering which record accessors will be generated upon parsing), then renders the
-- terms (being careful not to render any record accessors, since those would cause duplicate binding errors upon
-- parsing).
renderDefnsForUnisonFile ::
  forall a v.
  (Var v, Monoid a) =>
  DeclNameLookup ->
  PrettyPrintEnvDecl ->
  DefnsF (Map Name) (Term v a, Type v a) (TypeReferenceId, Decl v a) ->
  DefnsF (Map Name) (Pretty ColorText) (Pretty ColorText)
renderDefnsForUnisonFile declNameLookup ppe defns =
  let (types, accessorNames) = Writer.runWriter (Map.traverseWithKey renderType defns.types)
   in Defns
        { terms = Map.mapMaybeWithKey (renderTerm accessorNames) defns.terms,
          types
        }
  where
    renderType :: Name -> (TypeReferenceId, Decl v a) -> Writer (Set AccessorName) (Pretty ColorText)
    renderType name (ref, typ) =
      fmap Pretty.syntaxToColor $
        DeclPrinter.prettyDeclW
          -- Sort of a hack; since the decl printer looks in the PPE for names of constructors,
          -- we just delete all term names out and add back the constructors...
          -- probably no need to wipe out the suffixified side but we do it anyway
          (setPpedToConstructorNames declNameLookup name ref ppe)
          (Reference.fromId ref)
          (HQ.NameOnly name)
          typ

    renderTerm :: Set Name -> Name -> (Term v a, Type v a) -> Maybe (Pretty ColorText)
    renderTerm accessorNames name (term, typ) = do
      guard (not (Set.member name accessorNames))
      let hqName = HQ.NameOnly name
      let rendered
            | Typechecker.isEqual (Builtin.Decls.testResultListType mempty) typ =
                "test> " <> TermPrinter.prettyBindingWithoutTypeSignature ppe.suffixifiedPPE hqName term
            | otherwise = TermPrinter.prettyBinding ppe.suffixifiedPPE hqName term
      Just (Pretty.syntaxToColor rendered)

setPpedToConstructorNames :: DeclNameLookup -> Name -> TypeReferenceId -> PrettyPrintEnvDecl -> PrettyPrintEnvDecl
setPpedToConstructorNames declNameLookup name ref =
  set (#unsuffixifiedPPE . #termNames) referentNames
    . set (#suffixifiedPPE . #termNames) referentNames
  where
    constructorNameMap :: Map ConstructorReference Name
    constructorNameMap =
      Map.fromList
        ( name
            & expectConstructorNames declNameLookup
            & List.zip [0 ..]
            & over (mapped . _1) (ConstructorReference (Reference.fromId ref))
        )

    referentNames :: Referent -> [(HQ'.HashQualified Name, HQ'.HashQualified Name)]
    referentNames = \case
      Referent.Con conRef _ ->
        case Map.lookup conRef constructorNameMap of
          Nothing -> []
          Just conName -> let hqConName = HQ'.NameOnly conName in [(hqConName, hqConName)]
      Referent.Ref _ -> []

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
