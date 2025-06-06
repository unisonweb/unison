-- | @update@ input handler.
module Unison.Codebase.Editor.HandleInput.Update2
  ( handleUpdate2,

    -- * Misc helpers to be organized later
    typecheckedUnisonFileToBranchUpdates,
  )
where

import Control.Lens (mapped, (.=))
import Control.Monad.Reader.Class (ask)
import Data.Bifoldable (bifoldMap)
import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import Text.Builder qualified
import U.Codebase.Decl qualified as V2.Decl
import U.Codebase.Reference (Reference, Reference' (..), TermReferenceId)
import U.Codebase.Sqlite.Operations qualified as Operations
import U.Codebase.Sqlite.Project qualified as Sqlite
import U.Codebase.Sqlite.ProjectBranch qualified as Sqlite
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Cli.Monad (Cli, Env (..))
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.Pretty qualified as Pretty
import Unison.Cli.ProjectUtils qualified as ProjectUtils
import Unison.Cli.UpdateUtils (getNamespaceDependentsOf2, hydrateDefns, parseAndTypecheck)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (Branch0)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.BranchUtil qualified as BranchUtil
import Unison.Codebase.Editor.HandleInput.Branch qualified as HandleInput.Branch
import Unison.Codebase.Editor.HandleInput.DeleteBranch qualified as DeleteBranch
import Unison.Codebase.Editor.HandleInput.Merge2 qualified as Merge
import Unison.Codebase.Editor.Output (Output)
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Path (Path)
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ProjectPath (ProjectPathG (..))
import Unison.Codebase.SqliteCodebase.Operations qualified as Operations
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.DataDeclaration (Decl)
import Unison.DataDeclaration qualified as Decl
import Unison.DeclNameLookup (DeclNameLookup (..))
import Unison.Merge qualified as Merge
import Unison.Name (Name)
import Unison.NameSegment qualified as NameSegment
import Unison.Names (Names)
import Unison.Names qualified as Names
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl)
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.PrettyPrintEnvDecl.Names qualified as PPED
import Unison.Project (ProjectAndBranch (..), projectBranchNameToValidProjectBranchNameText)
import Unison.Reference (TypeReference, TypeReferenceId)
import Unison.Reference qualified as Reference (fromId)
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Sqlite (Transaction)
import Unison.Symbol (Symbol)
import Unison.Syntax.FilePrinter (renderDefnsForUnisonFile)
import Unison.Syntax.Name qualified as Name
import Unison.UnisonFile qualified as UF
import Unison.UnisonFile.Names qualified as UF
import Unison.UnisonFile.Type (TypecheckedUnisonFile)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Defns (Defns (..), DefnsF, defnsAreEmpty)
import Unison.Util.Monoid qualified as Monoid
import Unison.Util.Pretty (ColorText, Pretty)
import Unison.Util.Pretty qualified as Pretty
import Unison.Util.Relation qualified as Relation
import Unison.Var (Var)
import Unison.WatchKind qualified as WK
import Witch (unsafeFrom)

useUpdateV2 :: Bool
useUpdateV2 =
  isJust (unsafePerformIO (lookupEnv "UNISON_USE_UPDATE_V2"))
{-# NOINLINE useUpdateV2 #-}

handleUpdate2 :: Cli ()
handleUpdate2 = do
  env <- ask
  tuf <- Cli.expectLatestTypecheckedFile
  let termAndDeclNames = getTermAndDeclNames tuf
  pp <- Cli.getCurrentProjectPath
  let projectId = pp.project.projectId
  currentBranch <- Cli.getCurrentBranch
  let currentBranch0 = Branch.head currentBranch
  let currentBranch0ExcludingLibdeps = Branch.deleteLibdeps currentBranch0
  let namesIncludingLibdeps = Branch.toNames currentBranch0

  -- Assert that the namespace doesn't have any conflicted names, and get whether we are on an "update" branch already
  unconflictedView <-
    Branch.asUnconflicted currentBranch0ExcludingLibdeps
      & onLeft (Cli.returnEarly . Output.ConflictedDefn "update")

  -- Assert that the namespace doesn't have any incoherent decls
  (declNameLookup, onUpdateBranchAlready) <-
    Cli.runTransactionWithRollback \rollback -> do
      declNameLookup <-
        Codebase.getBranchDeclNameLookup env.codebase (Branch.namespaceHash currentBranch) unconflictedView
          & onLeftM (rollback . Output.IncoherentDeclDuringUpdate)
      onUpdateBranchAlready <- Queries.projectBranchIsUpdateBranch projectId pp.branch.branchId
      pure (declNameLookup, onUpdateBranchAlready)

  let fileTermNamespaceBindings :: Set Name
      fileTermNamespaceBindings =
        Set.map Name.unsafeParseVar (UF.termNamespaceBindings tuf)

  let fileTypeNamespaceBindings :: Set Name
      fileTypeNamespaceBindings =
        Set.map Name.unsafeParseVar (UF.typeNamespaceBindings tuf)

  finalOutput <-
    Cli.label \done ->
      Cli.withRespondRegion \respondRegion -> do
        respondRegion $
          Output.Literal (Pretty.wrap "Okay, I'm searching the branch for code that needs to be updated...")

        (dependents, hydratedDependents) <-
          Cli.runTransaction do
            -- Get all dependents of things being updated
            dependents0 <-
              getNamespaceDependentsOf2
                unconflictedView.defns
                (getExistingReferencesNamed termAndDeclNames (Branch.toNames currentBranch0ExcludingLibdeps))

            -- Throw away the dependents that are shadowed by the file itself
            let dependents1 :: DefnsF (Map Name) TermReferenceId TypeReferenceId
                dependents1 =
                  bimap
                    (`Map.withoutKeys` fileTermNamespaceBindings)
                    (`Map.withoutKeys` fileTypeNamespaceBindings)
                    dependents0

            -- Hydrate the dependents for rendering
            hydratedDependents <-
              hydrateDefns
                (Codebase.unsafeGetTermComponent env.codebase)
                Operations.expectDeclComponent
                dependents1

            pure (dependents1, hydratedDependents)

        secondTuf <- do
          case defnsAreEmpty dependents of
            -- If there are no dependents of the updates, then just use the already-typechecked file.
            True -> pure tuf
            False -> do
              respondRegion (Output.Literal (Pretty.wrap "That's done. Now I'm making sure everything typechecks..."))

              let prettyUnisonFile =
                    let ppe = makePPE 10 namesIncludingLibdeps (UF.typecheckedToNames tuf) dependents
                     in makePrettyUnisonFile
                          (Pretty.prettyUnisonFile ppe (UF.discardTypes tuf))
                          ( renderDefnsForUnisonFile
                              declNameLookup
                              ppe
                              Set.empty
                              (over (#terms . mapped) snd hydratedDependents)
                          )

              parsingEnv <- Cli.makeParsingEnv pp namesIncludingLibdeps

              secondTuf <-
                parseAndTypecheck prettyUnisonFile parsingEnv & onNothingM do
                  if useUpdateV2
                    then do
                      let dependentRefs :: DefnsF Set TermReferenceId TypeReferenceId
                          dependentRefs =
                            bimap (Map.elems >>> Set.fromList) (Map.elems >>> Set.fromList) dependents

                      let namespaceWithoutDependents :: Branch0 IO
                          namespaceWithoutDependents =
                            let keepType :: TypeReference -> Bool
                                keepType = \case
                                  ReferenceBuiltin _ -> True
                                  ReferenceDerived refId -> not (Set.member refId dependentRefs.types)
                                keepTerm :: Referent -> Bool
                                keepTerm = \case
                                  Referent.Con (ConstructorReference ref _) _ -> keepType ref
                                  Referent.Ref ref ->
                                    case ref of
                                      ReferenceBuiltin _ -> True
                                      ReferenceDerived refId -> not (Set.member refId dependentRefs.terms)
                             in unconflictedView.defns
                                  & bimap
                                    ( BiMultimap.range
                                        >>> (`Map.withoutKeys` fileTermNamespaceBindings)
                                        >>> Map.filter keepTerm
                                    )
                                    ( BiMultimap.range
                                        >>> (`Map.withoutKeys` fileTypeNamespaceBindings)
                                        >>> Map.filter keepType
                                    )
                                  & Branch.fromUnconflictedDefns
                                  & Branch.setLibdeps
                                    ( currentBranch0
                                        & Branch.getAt0 (Path.singleton NameSegment.libSegment)
                                    )

                      let nextNamespace =
                            Branch.cons namespaceWithoutDependents currentBranch

                      if onUpdateBranchAlready
                        then do
                          Cli.updateProjectBranchRoot_ pp.branch "update" (const nextNamespace)
                        else do
                          uniqueTypeGuidsByName <-
                            Cli.runTransaction (makeUniqueTypeGuids (BiMultimap.range unconflictedView.defns.types))

                          (_temporaryBranchId, _temporaryBranchName) <-
                            HandleInput.Branch.createBranch
                              ("update " <> into @Text (ProjectAndBranch pp.project.name pp.branch.name))
                              ( HandleInput.Branch.CreateFrom'Update
                                  (pp.branch, Branch.headHash currentBranch, uniqueTypeGuidsByName)
                                  nextNamespace
                              )
                              pp.project
                              ( ProjectUtils.findTemporaryBranchName
                                  projectId
                                  ( ("update-" <> projectBranchNameToValidProjectBranchNameText pp.branch.name)
                                      & Text.Builder.run
                                      & unsafeFrom @Text
                                  )
                              )
                          pure ()

                      scratchFilePath <- fst <$> Cli.expectLatestFile
                      liftIO $ env.writeSource (Text.pack scratchFilePath) (Text.pack $ Pretty.toPlain 80 prettyUnisonFile) True
                      done Output.UpdateTypecheckingFailure
                    else do
                      scratchFilePath <- fst <$> Cli.expectLatestFile
                      liftIO $ env.writeSource (Text.pack scratchFilePath) (Text.pack $ Pretty.toPlain 80 prettyUnisonFile) True
                      done Output.UpdateTypecheckingFailure

              respondRegion (Output.Literal (Pretty.wrap "Everything typechecks, so I'm saving the results..."))

              pure secondTuf

        path <- Cli.getCurrentProjectPath
        branchUpdates <-
          Cli.runTransactionWithRollback \abort -> do
            Codebase.addDefsToCodebase env.codebase secondTuf
            typecheckedUnisonFileToBranchUpdates
              abort
              (\typeName -> Right (Map.lookup typeName declNameLookup.declToConstructors))
              secondTuf
        Cli.stepAt "update" (path, Branch.batchUpdates branchUpdates)
        #latestTypecheckedFile .= Nothing

        -- Special case: we are running a successful `update` on an update branch that has a parent (an update branch
        -- only won't have a parent if the parent has been deleted for some reason).
        case (onUpdateBranchAlready, pp.branch.parentBranchId) of
          (True, Just parentBranchId) -> do
            -- Switch to the parent branch
            parentBranch <-
              Cli.runTransaction do
                Queries.expectProjectBranch projectId parentBranchId
            Cli.switchProject (ProjectAndBranch projectId parentBranch.branchId)

            -- Merge the update branch into the parent branch. This isn't guaranteed to succeed, but it probably will.

            Merge.doMergeLocalBranch
              Merge.TwoWay
                { alice = ProjectAndBranch pp.project parentBranch,
                  bob = ProjectAndBranch pp.project pp.branch
                }

            -- If the merge succeeded, delete the update branch. We may want to try to delete it even if the merge
            -- fails, because otherwise the user will have to manually clean it up, which isn't as nice as a successful
            -- `update` on an update branch. However, it's very likely that the merge is simply a fast-forward.

            DeleteBranch.doDeleteProjectBranch (ProjectAndBranch pp.project pp.branch)
          _ -> pure ()

        pure Output.Success

  Cli.respond finalOutput

-- Make a unique type name to guid mapping from definitions, by looking up each decl individually. Maybe there will be
-- a more efficient way to accomplish this some day, but this is how it works for now.
makeUniqueTypeGuids :: Map Name TypeReference -> Transaction (Map Name Text)
makeUniqueTypeGuids types = do
  let step :: Map TypeReferenceId Text -> TypeReferenceId -> Transaction (Map TypeReferenceId Text)
      step acc refId = do
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

makePrettyUnisonFile :: Pretty ColorText -> DefnsF (Map Name) (Pretty ColorText) (Pretty ColorText) -> Pretty ColorText
makePrettyUnisonFile originalFile dependents =
  originalFile
    <> Pretty.newline
    <> Pretty.newline
    <> "-- The definitions below no longer typecheck with the changes above."
    <> Pretty.newline
    <> "-- Please fix the errors and try `update` again."
    <> Pretty.newline
    <> Pretty.newline
    <> ( dependents
           & inAlphabeticalOrder
           & let f = foldMap (\defn -> defn <> Pretty.newline <> Pretty.newline) in bifoldMap f f
       )
  where
    inAlphabeticalOrder :: DefnsF (Map Name) a b -> DefnsF [] a b
    inAlphabeticalOrder =
      bimap f f
      where
        f = map snd . List.sortOn (Name.toText . fst) . Map.toList

-- @typecheckedUnisonFileToBranchUpdates getConstructors file@ returns a list of branch updates (suitable for passing
-- along to `batchUpdates` or some "step at" combinator) that corresponds to using all of the contents of @file@.
-- `getConstructors` returns the full constructor names of a decl, e.g. "Maybe" -> ["Maybe.Nothing", "Maybe.Just"]
--
-- For example, if the file contains
--
--     foo.bar.baz = <#foo>
--
-- then the returned updates will look like
--
--     [ ("foo.bar", insert-term("baz",<#foo>)) ]
typecheckedUnisonFileToBranchUpdates ::
  (forall void. Output -> Transaction void) ->
  -- | Returns 'Nothing' if the decl isn't in namesExcludingLibdeps,
  -- in which case we know the decl is new and do not need to generate
  -- delete actions for it.
  (Name -> Either Output (Maybe [Name])) ->
  TypecheckedUnisonFile Symbol Ann ->
  Transaction [(Path, Branch0 m -> Branch0 m)]
typecheckedUnisonFileToBranchUpdates abort getConstructors tuf = do
  declUpdates <- makeDeclUpdates abort
  pure $ declUpdates ++ termUpdates
  where
    makeDeclUpdates :: forall m. (forall void. Output -> Transaction void) -> Transaction [(Path, Branch0 m -> Branch0 m)]
    makeDeclUpdates abort = do
      dataDeclUpdates <- Monoid.foldMapM makeDataDeclUpdates (Map.toList $ UF.dataDeclarationsId' tuf)
      effectDeclUpdates <- Monoid.foldMapM makeEffectDeclUpdates (Map.toList $ UF.effectDeclarationsId' tuf)
      pure $ dataDeclUpdates <> effectDeclUpdates
      where
        makeDataDeclUpdates (symbol, (typeRefId, dataDecl)) = makeDeclUpdates (symbol, (typeRefId, Right dataDecl))
        makeEffectDeclUpdates (symbol, (typeRefId, effectDecl)) = makeDeclUpdates (symbol, (typeRefId, Left effectDecl))

        makeDeclUpdates :: (Symbol, (TypeReferenceId, Decl Symbol Ann)) -> Transaction [(Path, Branch0 m -> Branch0 m)]
        makeDeclUpdates (symbol, (typeRefId, decl)) = do
          -- some decls will be deleted, we want to delete their
          -- constructors as well
          deleteConstructorActions <-
            ( maybe [] (map (BranchUtil.makeAnnihilateTermName . Path.splitFromName))
                <$> getConstructors (Name.unsafeParseVar symbol)
              )
              & onLeft abort
          let deleteTypeAction = BranchUtil.makeAnnihilateTypeName split
              split = splitVar symbol
              insertTypeAction = BranchUtil.makeAddTypeName split (Reference.fromId typeRefId)
              insertTypeConstructorActions =
                let referentIdsWithNames = zip (Decl.constructorVars (Decl.asDataDecl decl)) (Decl.declConstructorReferents typeRefId decl)
                 in map
                      ( \(sym, rid) ->
                          let splitConName = splitVar sym
                           in BranchUtil.makeAddTermName splitConName (Reference.fromId <$> rid)
                      )
                      referentIdsWithNames
              deleteStuff = deleteTypeAction : deleteConstructorActions
              addStuff = insertTypeAction : insertTypeConstructorActions
          pure $ deleteStuff ++ addStuff

    termUpdates :: [(Path, Branch0 m -> Branch0 m)]
    termUpdates =
      tuf
        & UF.hashTermsId
        & Map.toList
        & foldMap \(var, (_, ref, wk, _, _)) ->
          if WK.watchKindShouldBeStoredInDatabase wk
            then
              let split = splitVar var
               in [ BranchUtil.makeAnnihilateTermName split,
                    BranchUtil.makeAddTermName split (Referent.fromTermReferenceId ref)
                  ]
            else []

    splitVar :: Symbol -> Path.Split Path
    splitVar = Path.splitFromName . Name.unsafeParseVar

-- | get references from `names` that have the same names as in `defns`
-- For constructors, we get the type reference.
getExistingReferencesNamed :: DefnsF Set Name Name -> Names -> Set Reference
getExistingReferencesNamed defns names =
  bifoldMap fromTerms fromTypes defns
  where
    fromTerms :: Set Name -> Set Reference
    fromTerms =
      foldMap \name ->
        Set.map Referent.toReference (Relation.lookupDom name (Names.terms names))

    fromTypes :: Set Name -> Set TypeReference
    fromTypes =
      foldMap \name ->
        Relation.lookupDom name (Names.types names)

-- @getTermAndDeclNames file@ returns the names of the terms and decls defined in a typechecked Unison file.
getTermAndDeclNames :: (Var v) => TypecheckedUnisonFile v a -> DefnsF Set Name Name
getTermAndDeclNames tuf =
  Defns (terms <> effectCtors <> dataCtors) (effects <> datas)
  where
    terms =
      UF.hashTermsId tuf
        & Map.foldMapWithKey \var (_, _, wk, _, _) ->
          if WK.watchKindShouldBeStoredInDatabase wk
            then Set.singleton (Name.unsafeParseVar var)
            else Set.empty
    effects = keysToNames $ UF.effectDeclarationsId' tuf
    datas = keysToNames $ UF.dataDeclarationsId' tuf
    effectCtors = foldMap ctorsToNames $ fmap (Decl.toDataDecl . snd) $ UF.effectDeclarationsId' tuf
    dataCtors = foldMap ctorsToNames $ fmap snd $ UF.dataDeclarationsId' tuf
    keysToNames = Set.map Name.unsafeParseVar . Map.keysSet
    ctorsToNames = Set.fromList . map Name.unsafeParseVar . Decl.constructorVars

-- The big picture behind PPE building, though there are many details:
--
--   * We are updating old references to new references by rendering old references as names that are then parsed
--     back to resolve to new references (the world's weirdest implementation of AST substitution).
--
--   * We have to render names that refer to definitions in the file with a different suffixification strategy
--     (namely, "suffixify by name") than names that refer to things in the codebase.
--
--     This is because you *may* refer to aliases that share a suffix by that suffix for definitions in the
--     codebase, but not in the file.
--
--     For example, the following file will fail to parse:
--
--       one.foo = 10
--       two.foo = 10
--       hey = foo + foo -- "Which foo do you mean? There are two."
--
--     However, the following file will not fail to parse, if `one.foo` and `two.foo` are aliases in the codebase:
--
--       hey = foo + foo
makePPE ::
  Int ->
  Names ->
  Names ->
  DefnsF (Map Name) TermReferenceId TypeReferenceId ->
  PrettyPrintEnvDecl
makePPE hashLen namespaceNames initialFileNames dependents =
  PPED.addFallback
    ( let names = initialFileNames <> Names.fromUnconflictedReferenceIds dependents
       in PPED.makePPED (PPE.namer names) (PPE.suffixifyByName (Names.shadowing names namespaceNames))
    )
    ( PPED.makePPED
        (PPE.hqNamer hashLen namespaceNames)
        -- We don't want to over-suffixify for a reference in the namespace. For example, say we have "foo.bar" in the
        -- namespace and "oink.bar" in the file. "bar" may be a unique suffix among the namespace names, but would be
        -- ambiguous in the context of namespace + file names.
        --
        -- So, we use `shadowing`, which starts with the LHS names (the namespace), and adds to it names from the
        -- RHS (the initial file names, i.e. what was originally saved) that don't already exist in the LHS.
        (PPE.suffixifyByHash (Names.shadowing namespaceNames initialFileNames))
    )
