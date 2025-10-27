module Unison.Codebase.Editor.HandleInput.Delete
  ( handleDelete,
  )
where

import Control.Lens
import Control.Monad.Reader (ask)
import Data.Bifoldable (bifoldMap)
import Data.Containers.ListUtils qualified as List (nubOrd)
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as Set.NonEmpty
import Data.Text qualified as Text
import Text.Builder qualified
import U.Codebase.Sqlite.Operations qualified as Operations
import U.Codebase.Sqlite.ProjectBranch (ProjectBranch (..))
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.ProjectUtils qualified as ProjectUtils
import Unison.Cli.UpdateUtils (hydrateRefs, makeUniqueTypeGuids, nameHydratedRefIds2)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (Branch, Branch0)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.BranchUtil qualified as BranchUtil
import Unison.Codebase.Editor.HandleInput.Branch qualified as HandleInput.Branch
import Unison.Codebase.Editor.Input
import Unison.Codebase.Editor.Output
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Path qualified as Path
import Unison.ConstructorReference (ConstructorReference, GConstructorReference (..))
import Unison.ConstructorType (ConstructorType)
import Unison.DataDeclaration (Decl)
import Unison.DataDeclaration.ConstructorId (ConstructorId)
import Unison.DeclCoherencyCheck qualified as DeclCoherencyCheck
import Unison.DeclNameLookup (DeclNameLookup)
import Unison.DeclNameLookup qualified as DeclNameLookup
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Name (Name)
import Unison.NameSegment (NameSegment)
import Unison.NameSegment qualified as NameSegment
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), projectBranchNameToValidProjectBranchNameText)
import Unison.Reference (Reference' (..), TermReference, TermReferenceId, TypeReference, TypeReferenceId)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Symbol (Symbol)
import Unison.Syntax.FilePrinter (renderDefnsForUnisonFile)
import Unison.Syntax.HashQualifiedPrime qualified as HQ'
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.UnconflictedLocalDefnsView (UnconflictedLocalDefnsView (..))
import Unison.Util.Alphabetical (sortAlphabeticallyOn)
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Defn (Defn (..))
import Unison.Util.Defns (Defns (..), DefnsF, defnsAreEmpty, zipDefnsWith)
import Unison.Util.Defns qualified as Defns
import Unison.Util.Map qualified as Map
import Unison.Util.Pretty (ColorText, Pretty)
import Unison.Util.Pretty qualified as Pretty
import Unison.Util.Relation (Relation)
import Unison.Util.Relation qualified as Relation
import Unison.Util.Set qualified as Set
import Witch (unsafeFrom)

-- Note: we de-dupe input because we might be feeding in from numbered arg that have duplicates, e.g. if the previous
-- output was
--
-- I deleted these types:
--
--   1. foo
--
-- I deleted these terms:
--
--   2. foo

handleDelete :: Bool -> DeleteTarget -> [HQ'.HashQualified Name] -> Cli ()
handleDelete False {- force? -} which (List.nubOrd -> targetNames) = do
  env <- ask

  projectAndBranch <- Cli.getCurrentProjectAndBranch

  when (projectAndBranch.branch.isUpdate || projectAndBranch.branch.isUpgrade) do
    Cli.returnEarly
      if projectAndBranch.branch.isUpdate
        then Output.CantDoThatDuring "an update" "update"
        else Output.CantDoThatDuring "an upgrade" "upgrade"

  currentNamespace <- Cli.getCurrentProjectRoot
  let currentNamespace0 = Branch.head currentNamespace
  let currentNamespaceSansLib0 = Branch.deleteLibdeps currentNamespace0

  unconflictedView :: UnconflictedLocalDefnsView <-
    Branch.asUnconflicted currentNamespaceSansLib0 & onLeft (Cli.returnEarly . Output.ConflictedDefn)

  -- Assert that the namespace doesn't have any incoherent decls
  declNameLookup <-
    Cli.runTransactionWithRollback \rollback -> do
      Codebase.getBranchDeclNameLookup env.codebase (Branch.namespaceHash currentNamespace) unconflictedView
        & onLeftM
          ( rollback
              . Output.IncoherentDeclDuringDelete which
              . DeclCoherencyCheck.asOneRandomIncoherentDeclReason
          )

  -- Identify the term and types identified by the provided names.
  target :: Defns (BiMultimap TermReference Name) (BiMultimap TypeReference Name) <-
    resolveTarget which targetNames unconflictedView.defns

  let targetIds :: DefnsF Set TermReferenceId TypeReferenceId
      targetIds =
        let f = Set.mapMaybe Reference.toId . BiMultimap.dom in bimap f f target

  -- Identify the references whose last name(s) would go away if the delete goes through.
  let nameless :: DefnsF Set TermReference TypeReference
      nameless =
        resolveReferencesToDelete (Branch.deepDefns currentNamespace0) target

  -- Now there are two possibilities:
  --
  --   1. There is at least one direct dependent of `nameless` that is itself not in `target`, i.e. if the delete
  --      goes through then there'd be something with a nameless dependency.
  --
  --   2. There are no direct dependents of `nameless` that are not in `target`, i.e. we're deleting stuff that
  --      wouldn't leave behind anything with a nameless dependency, so the delete can proceed.
  result ::
    Either
      ( Map Name Text,
        Defns (Map TermReferenceId (Term Symbol Ann, Type Symbol Ann)) (Map TypeReferenceId (Decl Symbol Ann))
      )
      (Map TypeReference ConstructorType) <-
    Cli.runTransaction do
      let scope = Branch.deepDefnsIds currentNamespaceSansLib0
      dependents <- Operations.directDependentsWithinScope scope nameless
      if defnsAreEmpty (zipDefnsWith Set.difference Set.difference dependents targetIds)
        then do
          -- (2)
          declTypes <- Map.fromSetA (Codebase.getDeclType env.codebase) (BiMultimap.dom target.types)
          pure (Right declTypes)
        else do
          -- (1)
          transitiveDependents <- Operations.transitiveDependentsWithinScope scope nameless
          uniqueTypeGuidsByName <- makeUniqueTypeGuids (BiMultimap.range unconflictedView.defns.types)
          hydratedDependents <- hydrateRefs env.codebase transitiveDependents
          pure (Left (uniqueTypeGuidsByName, hydratedDependents))

  declTypes <-
    result & onLeft \(uniqueTypeGuidsByName, dependentsOfNameless) -> do
      -- Identify the name of everything being deleted: the target names, plus the names of all target types' constructors
      let targetPlusConstructorsNames :: DefnsF Set Name Name
          targetPlusConstructorsNames =
            let typesNames = BiMultimap.ran target.types
                constructorNames = Set.fromList . DeclNameLookup.expectConstructorNames declNameLookup
             in fold
                  [ Defns (BiMultimap.ran target.terms) typesNames,
                    Defns.fromTerms (foldMap constructorNames typesNames)
                  ]

      let -- Make the underlying namespace of the new branch: start with the current branch, delete the targets (including
          -- constructors of targeted types), delete the transitive dependents of now-nameless dependencies (since those
          -- are going in the scratch file)
          nextNamespaceDefns :: Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)
          nextNamespaceDefns =
            zipDefnsWith
              ( \defns target ->
                  defns
                    & BiMultimap.withoutRan target
                    & BiMultimap.filterDom \case
                      Referent.Ref (ReferenceDerived termRef) -> Map.notMember termRef dependentsOfNameless.terms
                      Referent.Con (ConstructorReference (ReferenceDerived typeRef) _) _ ->
                        Map.notMember typeRef dependentsOfNameless.types
                      _ -> True -- keep builtins, they obvioulsy aren't / can't be in dependents
              )
              ( let dependentTypes = Map.mapKeysMonotonic Reference.fromId dependentsOfNameless.types
                 in \defns target ->
                      defns
                        & BiMultimap.withoutRan target
                        & BiMultimap.withoutDomMap dependentTypes
              )
              unconflictedView.defns
              targetPlusConstructorsNames

      let nextNamespace :: Branch IO
          nextNamespace =
            nextNamespaceDefns
              & bimap BiMultimap.range BiMultimap.range
              & Branch.fromUnconflictedDefns
              & Branch.setLibdeps (Branch.getAt0 (Path.singleton NameSegment.libSegment) currentNamespace0)
              & (`Branch.cons` currentNamespace)

      -- A failed delete makes an "update" branch, since it behaves like an update branch in every way. We even name the
      -- branch update-* so it doesn't feel like a weird new thing.
      (_updateBranchId, updateBranchName) <-
        HandleInput.Branch.createBranch
          ("update " <> into @Text (ProjectAndBranch projectAndBranch.project.name projectAndBranch.branch.name))
          ( HandleInput.Branch.CreateFrom'Update
              (projectAndBranch.branch, Branch.headHash currentNamespace, uniqueTypeGuidsByName)
              nextNamespace
          )
          projectAndBranch.project
          ( ProjectUtils.findTemporaryBranchName
              projectAndBranch.project.projectId
              ( ("update-" <> projectBranchNameToValidProjectBranchNameText projectAndBranch.branch.name)
                  & Text.Builder.run
                  & unsafeFrom @Text
              )
          )

      scratchFilePath <- fst <$> Cli.expectLatestFile
      #latestFile ?= (scratchFilePath, True)

      let prettyUnisonFile :: Pretty ColorText
          prettyUnisonFile =
            "-- The definitions below depend on the deleted definitions."
              <> Pretty.newline
              <> "-- Please fix the errors and run `update`."
              <> Pretty.newline
              <> Pretty.newline
              <> renderDefns dependents.types
              <> renderDefns dependents.terms
            where
              dependents :: DefnsF (Map Name) (Pretty ColorText) (Pretty ColorText)
              dependents =
                renderDefnsForUnisonFile
                  declNameLookup
                  (Branch.toPrettyPrintEnvDecl 10 currentNamespace0)
                  Set.empty
                  ( dependentsOfNameless
                      & nameHydratedRefIds2 unconflictedView.defns
                      & over (#terms . mapped) snd
                  )

              renderDefns :: Map Name (Pretty ColorText) -> Pretty ColorText
              renderDefns =
                foldMap (\(_, defn) -> defn <> Pretty.newline <> Pretty.newline)
                  . sortAlphabeticallyOn fst
                  . Map.toList

      liftIO $ env.writeSource (Text.pack scratchFilePath) (Pretty.toPlain 80 prettyUnisonFile) True

      Cli.returnEarly (Output.DeleteFailure scratchFilePath projectAndBranch.branch.name updateBranchName)

  -- Identify the delete actions to apply to the current branch. This is just the delete target, plus constructors.
  let deleteActions :: [(Path.Absolute, Branch0 m -> Branch0 m)]
      deleteActions =
        target
          & addConstructorsToTarget declNameLookup declTypes
          & makeDeleteActions

  let command =
        case which of
          DeleteTarget'TermOrType -> "delete"
          DeleteTarget'Term -> "delete.term"
          DeleteTarget'Type -> "delete.type"

  let description =
        Text.unwords (command : map HQ'.toText targetNames)

  Cli.stepManyAt projectAndBranch.branch description deleteActions

  Cli.respondNumbered (DeletedDefinitions (bimap BiMultimap.ran BiMultimap.ran target))
-- A force-delete is muuch simpler: don't care if we leave nameless dependencies, just nuke things from the namespace.
-- You also have to use this version when deleting a constructor, which doesn't happen often, but could (e.g. if you
-- have an incoherent decl due to extra constructor alias).
handleDelete True {- force? -} which targetNames = do
  projectAndBranch <- Cli.getCurrentProjectAndBranch

  currentNamespace <- Cli.getCurrentProjectRoot
  let currentNamespace0 = Branch.head currentNamespace
  let currentNamespaceSansLib0 = Branch.deleteLibdeps currentNamespace0

  -- Identify the terms, types, and constructors identified by the provided names.
  let target :: DefnsF (Relation Name) Referent TypeReference
      target =
        currentNamespaceSansLib0
          & Branch.deepDefns
          & bimap Relation.swap Relation.swap
          & resolveTargetInConflicted which targetNames

  -- Identify the delete actions to apply to the current branch. This is just the delete target, plus constructors.
  let deleteActions :: [(Path.Absolute, Branch0 m -> Branch0 m)]
      deleteActions =
        makeDeleteActionsForConflicted target

  let command =
        case which of
          DeleteTarget'TermOrType -> "delete.force"
          DeleteTarget'Term -> "delete.term.force"
          DeleteTarget'Type -> "delete.type.force"

  let description =
        Text.unwords (command : map HQ'.toText targetNames)

  Cli.stepManyAt projectAndBranch.branch description deleteActions

  Cli.respondNumbered (DeletedDefinitions (bimap Relation.dom Relation.dom target))

resolveTarget ::
  DeleteTarget ->
  [HQ'.HashQualified Name] ->
  Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
  Cli (Defns (BiMultimap TermReference Name) (BiMultimap TypeReference Name))
resolveTarget target targetNames defns =
  targetNames
    & traverse
      ( \name ->
          case target of
            DeleteTarget'Term -> do
              let (matchingConstructors, matchingTerms) = toMatchingReferents name
              when (Map.null matchingTerms) do
                Cli.returnEarly
                  case Set.NonEmpty.nonEmptySet (Map.keysSet matchingConstructors) of
                    Nothing -> Output.TermAndOrTypeNameNotFound (Just (TermDefn ())) name
                    Just names -> Output.CantDeleteConstructor names
              pure Defns {terms = BiMultimap.fromRange matchingTerms, types = BiMultimap.empty}
            DeleteTarget'Type -> do
              let matchingTypes = toMatchingTypeReferences name
              when (BiMultimap.isEmpty matchingTypes) do
                Cli.returnEarly (Output.TermAndOrTypeNameNotFound (Just (TypeDefn ())) name)
              pure Defns {terms = BiMultimap.empty, types = matchingTypes}
            DeleteTarget'TermOrType -> do
              let (matchingConstructors, matchingTerms) = toMatchingReferents name
                  matchingTypes = toMatchingTypeReferences name
              when (Map.null matchingTerms && BiMultimap.isEmpty matchingTypes) do
                Cli.returnEarly
                  case Set.NonEmpty.nonEmptySet (Map.keysSet matchingConstructors) of
                    Nothing -> Output.TermAndOrTypeNameNotFound Nothing name
                    Just names -> Output.CantDeleteConstructor names
              pure Defns {terms = BiMultimap.fromRange matchingTerms, types = matchingTypes}
      )
    & fmap
      ( List.foldl'
          (zipDefnsWith BiMultimap.unsafeUnion BiMultimap.unsafeUnion)
          (Defns BiMultimap.empty BiMultimap.empty)
      )
  where
    toMatchingReferents ::
      HQ'.HashQualified Name ->
      (Map Name ConstructorReference, Map Name TermReference)
    toMatchingReferents name =
      defns.terms
        & HQ'.filterUnconflictedBySuffix Referent.toShortHash name
        & BiMultimap.range
        & Map.foldlWithKey'
          ( \(constructors, terms) name -> \case
              Referent.Con ref _ -> let !constructors1 = Map.insert name ref constructors in (constructors1, terms)
              Referent.Ref ref -> let !terms1 = Map.insert name ref terms in (constructors, terms1)
          )
          (Map.empty, Map.empty)

    toMatchingTypeReferences :: HQ'.HashQualified Name -> BiMultimap TypeReference Name
    toMatchingTypeReferences name =
      HQ'.filterUnconflictedBySuffix Reference.toShortHash name defns.types

-- Like `resolveTarget`, but takes a whole (potentially conflicted) namespace, rather than an unconflicted namespace.
-- This is used in `delete.force`, which works on a namespace with conflicted names, can delete individual
-- constructors, and doesn't care if the name doesn't resolve to anything.
resolveTargetInConflicted ::
  DeleteTarget ->
  [HQ'.HashQualified Name] ->
  Defns (Relation Name Referent) (Relation Name TypeReference) ->
  Defns (Relation Name Referent) (Relation Name TypeReference)
resolveTargetInConflicted target targetNames defns =
  targetNames & foldMap \name ->
    case target of
      DeleteTarget'Term -> Defns.fromTerms (toMatchingReferents name)
      DeleteTarget'Type -> Defns.fromTypes (toMatchingTypeReferences name)
      DeleteTarget'TermOrType ->
        Defns
          { terms = toMatchingReferents name,
            types = toMatchingTypeReferences name
          }
  where
    toMatchingReferents :: HQ'.HashQualified Name -> Relation Name Referent
    toMatchingReferents name =
      HQ'.filterBySuffix Referent.toShortHash name defns.terms

    toMatchingTypeReferences :: HQ'.HashQualified Name -> Relation Name TypeReference
    toMatchingTypeReferences name =
      HQ'.filterBySuffix Reference.toShortHash name defns.types

resolveReferencesToDelete ::
  Defns (Relation Referent Name) (Relation TypeReference Name) ->
  Defns (BiMultimap TermReference Name) (BiMultimap TypeReference Name) ->
  DefnsF Set TermReference TypeReference
resolveReferencesToDelete =
  Defns.zipDefnsWith (f Referent.fromTermReference) (f id)
  where
    f :: (Ord ref, Ord ref') => (ref -> ref') -> Relation ref' Name -> BiMultimap ref Name -> Set ref
    f toRef defns =
      Map.foldlWithKey' (g toRef defns) Set.empty . BiMultimap.domain

    g :: (Ord ref, Ord ref') => (ref -> ref') -> Relation ref' Name -> Set ref -> ref -> NESet Name -> Set ref
    g toRef defns acc ref namesToDelete
      | Set.size (Relation.lookupDom (toRef ref) defns) > Set.NonEmpty.size namesToDelete = acc
      | otherwise = Set.insert ref acc

addConstructorsToTarget ::
  DeclNameLookup ->
  Map TypeReference ConstructorType ->
  Defns (BiMultimap TermReference Name) (BiMultimap TypeReference Name) ->
  Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)
addConstructorsToTarget declNameLookup declTypes targetDefns =
  Defns
    { terms = BiMultimap.unsafeUnion termsToDelete constructorsToDelete,
      types = targetDefns.types
    }
  where
    termsToDelete :: BiMultimap Referent Name
    termsToDelete =
      BiMultimap.mapDomMonotonic Referent.fromTermReference targetDefns.terms

    constructorsToDelete :: BiMultimap Referent Name
    constructorsToDelete =
      targetDefns.types
        & BiMultimap.range
        & Map.foldlWithKey' f BiMultimap.empty
      where
        f :: BiMultimap Referent Name -> Name -> TypeReference -> BiMultimap Referent Name
        f acc typeName typeRef =
          declNameLookup.declToConstructors
            & Map.lookup typeName
            & maybe acc (ifoldl' (g typeRef) acc)

        g :: TypeReference -> Int -> BiMultimap Referent Name -> Name -> BiMultimap Referent Name
        g typeRef i acc constructorName =
          BiMultimap.insert
            ( Referent.Con
                (ConstructorReference typeRef (unsafeFrom @Int @ConstructorId i))
                (declTypes Map.! typeRef)
            )
            constructorName
            acc

makeDeleteActions ::
  Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
  [(Path.Absolute, Branch0 m -> Branch0 m)]
makeDeleteActions =
  toList @Seq . bifoldMap termActions typeActions
  where
    termActions :: BiMultimap Referent Name -> Seq (Path.Absolute, Branch0 m -> Branch0 m)
    termActions =
      Seq.fromList . map termAction . Map.toList . BiMultimap.range

    termAction :: (Name, Referent) -> (Path.Absolute, Branch0 m -> Branch0 m)
    termAction (name, ref) =
      BranchUtil.makeDeleteTermName (nameToSplitAbsolute name) ref

    typeActions :: BiMultimap TypeReference Name -> Seq (Path.Absolute, Branch0 m -> Branch0 m)
    typeActions =
      Seq.fromList . map typeAction . Map.toList . BiMultimap.range

    typeAction :: (Name, TermReference) -> (Path.Absolute, Branch0 m -> Branch0 m)
    typeAction (name, ref) =
      BranchUtil.makeDeleteTypeName (nameToSplitAbsolute name) ref

    nameToSplitAbsolute :: Name -> (Path.Absolute, NameSegment)
    nameToSplitAbsolute =
      over _1 Path.Absolute . Path.splitFromName

makeDeleteActionsForConflicted ::
  DefnsF (Relation Name) Referent TypeReference ->
  [(Path.Absolute, Branch0 m -> Branch0 m)]
makeDeleteActionsForConflicted =
  toList @Seq . bifoldMap termActions typeActions
  where
    termActions :: Relation Name Referent -> Seq (Path.Absolute, Branch0 m -> Branch0 m)
    termActions =
      Seq.fromList . map termAction . Relation.toList

    termAction :: (Name, Referent) -> (Path.Absolute, Branch0 m -> Branch0 m)
    termAction (name, ref) =
      BranchUtil.makeDeleteTermName (nameToSplitAbsolute name) ref

    typeActions :: Relation Name TypeReference -> Seq (Path.Absolute, Branch0 m -> Branch0 m)
    typeActions =
      Seq.fromList . map typeAction . Relation.toList

    typeAction :: (Name, TermReference) -> (Path.Absolute, Branch0 m -> Branch0 m)
    typeAction (name, ref) =
      BranchUtil.makeDeleteTypeName (nameToSplitAbsolute name) ref

    nameToSplitAbsolute :: Name -> (Path.Absolute, NameSegment)
    nameToSplitAbsolute =
      over _1 Path.Absolute . Path.splitFromName
