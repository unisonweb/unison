-- | @update@ input handler.
module Unison.Codebase.Editor.HandleInput.Update2
  ( handleUpdate2,

    -- * Misc helpers to be organized later
    typecheckedUnisonFileToBranchUpdates,
  )
where

import Control.Lens (mapped, (.=), (?=))
import Control.Monad.Reader.Class (ask)
import Data.Bifoldable (bifoldMap)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import Text.Builder qualified
import U.Codebase.Reference (TermReferenceId)
import U.Codebase.Sqlite.Project qualified as Sqlite
import U.Codebase.Sqlite.ProjectBranch qualified as Sqlite
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Cli.Monad (Cli, Env (..))
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.Pretty qualified as Pretty
import Unison.Cli.ProjectUtils qualified as ProjectUtils
import Unison.Cli.UpdateUtils (getNamespaceDependentsOf, hydrateRefs, makeUniqueTypeGuids, nameHydratedRefIds, parseAndTypecheck, subtractDependents)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (Branch, Branch0)
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
import Unison.DataDeclaration (Decl)
import Unison.DataDeclaration qualified as Decl
import Unison.DeclCoherencyCheck qualified as DeclCoherencyCheck
import Unison.DeclNameLookup (DeclNameLookup (..))
import Unison.Merge qualified as Merge
import Unison.Name (Name)
import Unison.NameSegment qualified as NameSegment
import Unison.Names (Names (Names))
import Unison.Names qualified as Names
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl)
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Project (ProjectAndBranch (..), projectBranchNameToValidProjectBranchNameText)
import Unison.Reference (TypeReferenceId)
import Unison.Reference qualified as Reference (fromId)
import Unison.Referent qualified as Referent
import Unison.Sqlite (Transaction)
import Unison.Symbol (Symbol)
import Unison.Syntax.FilePrinter (renderDefnsForUnisonFile)
import Unison.Syntax.Name qualified as Name
import Unison.UnconflictedLocalDefnsView (UnconflictedLocalDefnsView (..))
import Unison.UnisonFile qualified as UF
import Unison.UnisonFile.Names qualified as UF
import Unison.UnisonFile.Type (TypecheckedUnisonFile)
import Unison.Util.Alphabetical (sortAlphabeticallyOn)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Defns (Defns (..), DefnsF, defnsAreEmpty)
import Unison.Util.Monoid qualified as Monoid
import Unison.Util.Pretty (ColorText, Pretty)
import Unison.Util.Pretty qualified as Pretty
import Unison.Util.Relation qualified as Relation
import Unison.WatchKind qualified as WK
import Witch (unsafeFrom)

useUpdateV2 :: Bool
useUpdateV2 =
  not . isJust . unsafePerformIO $ lookupEnv "UNISON_USE_UPDATE_V1"
{-# NOINLINE useUpdateV2 #-}

handleUpdate2 :: Cli ()
handleUpdate2 = do
  env <- ask
  tuf <- Cli.expectLatestTypecheckedFile
  pp <- Cli.getCurrentProjectPath
  let projectId = pp.project.projectId
  currentBranch <- Cli.getCurrentBranch
  let currentBranch0 = Branch.head currentBranch
  let namesIncludingLibdeps = Branch.toNames currentBranch0

  -- Assert that the namespace doesn't have any conflicted names
  unconflictedView <-
    Branch.asUnconflicted currentBranch0
      & onLeft (Cli.returnEarly . Output.ConflictedDefn)

  -- Assert that the namespace doesn't have any incoherent decls
  declNameLookup <-
    Cli.runTransactionWithRollback \rollback -> do
      Codebase.getBranchDeclNameLookup env.codebase (Branch.namespaceHash currentBranch) unconflictedView
        & onLeftM (rollback . Output.IncoherentDeclDuringUpdate . DeclCoherencyCheck.asOneRandomIncoherentDeclReason)

  let namespaceBindings :: DefnsF Set Name Name
      namespaceBindings =
        bimap (Set.map Name.unsafeParseVar) (Set.map Name.unsafeParseVar) (UF.namespaceBindings tuf)

  finalOutput <-
    Cli.label \done ->
      Cli.withRespondRegion \respondRegion -> do
        respondRegion $
          Output.Literal (Pretty.wrap "Okay, I'm searching the branch for code that needs to be updated...")

        (dependents, dependentsRefs, hydratedDependents) <-
          Cli.runTransaction do
            -- Get all dependents of things being updated
            dependents0 <-
              getNamespaceDependentsOf
                unconflictedView.defns
                ( Names.references
                    Names
                      { terms = Relation.restrictDom namespaceBindings.terms unconflictedView.names.terms,
                        types = Relation.restrictDom namespaceBindings.types unconflictedView.names.types
                      }
                )

            -- Throw away the dependents that are shadowed by the file itself
            let dependents1 :: DefnsF (Map Name) TermReferenceId TypeReferenceId
                dependents1 =
                  bimap
                    (`Map.withoutKeys` namespaceBindings.terms)
                    (`Map.withoutKeys` namespaceBindings.types)
                    dependents0

            let dependentsRefs :: DefnsF Set TermReferenceId TypeReferenceId
                dependentsRefs =
                  bimap (Set.fromList . Map.elems) (Set.fromList . Map.elems) dependents1

            -- Hydrate the dependents for rendering
            hydratedDependents0 <-
              hydrateRefs (Codebase.unsafeGetTermComponent env.codebase) Operations.expectDeclComponent dependentsRefs

            let hydratedDependents1 =
                  nameHydratedRefIds dependents1 hydratedDependents0

            pure (dependents1, dependentsRefs, hydratedDependents1)

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
                      let nextNamespace :: Branch IO
                          nextNamespace =
                            unconflictedView.defns
                              & bimap
                                (BiMultimap.range >>> (`Map.withoutKeys` namespaceBindings.terms))
                                (BiMultimap.range >>> (`Map.withoutKeys` namespaceBindings.types))
                              & subtractDependents dependentsRefs
                              & Branch.fromUnconflictedDefns
                              & Branch.setLibdeps (Branch.getAt0 (Path.singleton NameSegment.libSegment) currentBranch0)
                              & (`Branch.cons` currentBranch)

                      if pp.branch.isUpdate || pp.branch.isUpgrade
                        then do
                          Cli.updateProjectBranchRoot_ pp.branch "update" (const nextNamespace)
                          scratchFilePath <- fst <$> Cli.expectLatestFile
                          liftIO $ env.writeSource (Text.pack scratchFilePath) (Text.pack $ Pretty.toPlain 80 prettyUnisonFile) True
                          done Output.UpdateTypecheckingFailure
                        else do
                          uniqueTypeGuidsByName <-
                            Cli.runTransaction (makeUniqueTypeGuids (BiMultimap.range unconflictedView.defns.types))

                          (_updateBranchId, updateBranchName) <-
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
                          scratchFilePath <- fst <$> Cli.expectLatestFile
                          #latestFile ?= (scratchFilePath, True)
                          liftIO $ env.writeSource (Text.pack scratchFilePath) (Text.pack $ Pretty.toPlain 80 prettyUnisonFile) True
                          done (Output.UpdateTypecheckingFailure2 scratchFilePath pp.branch.name updateBranchName)
                    else do
                      scratchFilePath <- fst <$> Cli.expectLatestFile
                      #latestFile ?= (scratchFilePath, True)
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

        -- Special case: we are running a successful `update` on an update/upgrade branch that has a parent (such
        -- branches won't have a parent only if the parent has been deleted for some reason).
        case (pp.branch.isUpdate || pp.branch.isUpgrade, pp.branch.parentBranchId) of
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

            -- If the merge succeeded, delete the current (update or upgrade) branch. We may want to try to delete it
            -- even if the merge fails, because otherwise the user will have to manually clean it up, which isn't as
            -- nice as a successful `update` on an update branch. However, it's very likely that the merge is simply a
            -- fast-forward.

            DeleteBranch.doDeleteProjectBranch (ProjectAndBranch pp.project pp.branch)
          _ -> pure ()

        pure Output.Success

  Cli.respond finalOutput

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
        f = map snd . sortAlphabeticallyOn fst . Map.toList

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
