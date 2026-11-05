-- | @update@ input handler.
module Unison.Codebase.Editor.HandleInput.Update2
  ( handleUpdate2,

    -- * Misc helpers to be organized later
    typecheckedUnisonFileToBranchUpdates,
  )
where

import Control.Lens (mapped, (.=), (?=), _3)
import Control.Monad.Reader.Class (ask)
import Data.Bifoldable (bifoldMap)
import Data.Map qualified as Map
import Data.Map.Merge.Strict qualified as Map
import Data.Set qualified as Set
import Data.Set.NonEmpty qualified as Set.NonEmpty
import Data.Text qualified as Text
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import TextBuilder qualified
import U.Codebase.Reference (TermReferenceId)
import U.Codebase.Sqlite.Project qualified as Sqlite
import U.Codebase.Sqlite.ProjectBranch qualified as Sqlite
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.ABT qualified as ABT
import Unison.Cli.Monad (Cli, Env (..))
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.Pretty qualified as Pretty
import Unison.Cli.ProjectUtils qualified as ProjectUtils
import Unison.Cli.ScratchFile qualified as ScratchFile
import Unison.Cli.TypeCheck (computeTypecheckingEnvironment)
import Unison.Cli.UpdateUtils (getNamespaceDependentsOf, hydrateRefs, makeUniqueTypeGuids, nameHydratedRefIds, parseAndTypecheck, subtractDependents)
import Unison.Codebase (Codebase)
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
import Unison.DataDeclaration (Decl)
import Unison.DataDeclaration qualified as Decl
import Unison.DeclCoherencyCheck qualified as DeclCoherencyCheck
import Unison.DeclNameLookup (DeclNameLookup (..))
import Unison.FileParsers qualified as FileParsers
import Unison.Hash (Hash)
import Unison.Merge qualified as Merge
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment qualified as NameSegment
import Unison.Names (Names (Names))
import Unison.Names qualified as Names
import Unison.Parser.Ann (Ann)
import Unison.Parsers qualified as Parsers
import Unison.Prelude
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl)
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Project (ProjectAndBranch (..), projectBranchNameToValidProjectBranchNameText)
import Unison.Reference (TermReference, TypeReference, TypeReferenceId)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Result qualified as Result
import Unison.Sqlite (Transaction)
import Unison.Symbol (Symbol)
import Unison.Syntax.FilePrinter (renderDefnsForUnisonFile)
import Unison.Syntax.Name qualified as Name
import Unison.Syntax.Parser qualified as Parser
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.Type (Type)
import Unison.Type qualified as Type
import Unison.Typechecker qualified as Typechecker
import Unison.Typechecker.TypeLookup qualified as TL
import Unison.UnconflictedLocalDefnsView (UnconflictedLocalDefnsView (..))
import Unison.UnisonFile qualified as UF
import Unison.UnisonFile.Names qualified as UF
import Unison.UnisonFile.Type (TypecheckedUnisonFile)
import Unison.Util.Alphabetical (sortAlphabeticallyOn)
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Defns (Defns (..), DefnsF, defnsAreEmpty, zipDefnsWith)
import Unison.Util.Monoid qualified as Monoid
import Unison.Util.Pretty (ColorText, Pretty)
import Unison.Util.Pretty qualified as Pretty
import Unison.Util.Relation (Relation)
import Unison.Util.Relation qualified as Relation
import Unison.Util.Set qualified as Set
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

  -- Of all the namespace bindings in the latest typechecked Unison file, keep the ones that don't correspond to "no
  -- change" (i.e. the thing was just `edit`-ed and untouched). We also reject the update entirely if it touches
  -- anything in lib.*.
  let addedOrUpdatedNamespaceBindings0 :: Defns (Set Name, Map Name Bool) (Set Name, Map Name Bool)
      addedOrUpdatedNamespaceBindings0 =
        let f :: (Eq ref1) => Relation Name ref1 -> (ref2 -> ref1) -> Name -> ref2 -> (Set Name, Bool)
            f libdeps toRef name fileRef
              | Name.beginsWithSegment name NameSegment.libSegment,
                maybe True (/= toRef fileRef) (Set.asSingleton (Relation.lookupDom name libdeps)) =
                  (Set.singleton name, False)
              | otherwise = (Set.empty, False)
            g :: (Eq ref1) => (ref2 -> ref1) -> name -> ref1 -> ref2 -> Maybe Bool
            g toRef _ codebaseRef fileRef
              | codebaseRef == toRef fileRef = Nothing
              | otherwise = Just True
            h :: (Eq ref1) => Relation Name ref1 -> (ref2 -> ref1) -> BiMultimap ref1 Name -> Map Symbol ref2 -> (Set Name, Map Name Bool)
            h libdeps toRef codebaseDefns fileDefns =
              Map.mergeA
                Map.dropMissing
                (Map.traverseMissing (f libdeps toRef))
                (Map.zipWithMaybeMatched (g toRef))
                (BiMultimap.range codebaseDefns)
                (Map.mapKeys Name.unsafeParseVar fileDefns)
         in zipDefnsWith
              (h namesIncludingLibdeps.terms Referent.fromId)
              (h namesIncludingLibdeps.types Reference.fromId)
              unconflictedView.defns
              (UF.namespaceBindingsMap tuf)

  let thingsInLibBeingAddedOrUpdated :: Set Name
      thingsInLibBeingAddedOrUpdated =
        bifoldMap fst fst addedOrUpdatedNamespaceBindings0

  whenJust (Set.NonEmpty.nonEmptySet thingsInLibBeingAddedOrUpdated) \things ->
    Cli.returnEarly (Output.CantUpdateLib things)

  let addedOrUpdatedNamespaceBindings :: DefnsF Set Name Name
      addedOrUpdatedNamespaceBindings =
        bimap (Map.keysSet . snd) (Map.keysSet . snd) addedOrUpdatedNamespaceBindings0

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
                      { terms = Relation.restrictDom addedOrUpdatedNamespaceBindings.terms unconflictedView.names.terms,
                        types = Relation.restrictDom addedOrUpdatedNamespaceBindings.types unconflictedView.names.types
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
              hydrateRefs env.codebase dependentsRefs

            let hydratedDependents1 =
                  nameHydratedRefIds dependents1 hydratedDependents0

            pure (dependents1, dependentsRefs, hydratedDependents1)

        -- How the file and its dependents are printed for typechecking, by the frontier and the slow path alike
        let ppe = makePPE 10 namesIncludingLibdeps (UF.typecheckedToNames tuf) dependents

        -- Typechecks the file with every dependent type and the named dependent terms printed into it. The other
        -- dependents are referred to by their old references, with the types in @retyped@ where they are given.
        let typecheckDependents :: Map TermReference (Type Symbol Ann) -> Set Name -> Cli (Maybe (TypecheckedUnisonFile Symbol Ann))
            typecheckDependents retyped names = do
              parsingEnv <- Cli.makeParsingEnv pp namesIncludingLibdeps
              let prettyUnisonFile =
                    makePrettyUnisonFile
                      (Pretty.prettyUnisonFile ppe (UF.discardTypes tuf))
                      ( renderDefnsForUnisonFile
                          declNameLookup
                          ppe
                          Set.empty
                          (Defns (snd <$> Map.restrictKeys hydratedDependents.terms names) hydratedDependents.types)
                      )
              parseAndTypecheckRetyped retyped prettyUnisonFile parsingEnv

        -- Dependents that use no type that changed take their new references without being typechecked again
        propagated <-
          if defnsAreEmpty dependents || typecheckDependentsAlways
            then pure Nothing
            else runMaybeT do
              propagation <-
                MaybeT . Cli.runTransaction $
                  planPropagation
                    env.codebase
                    unconflictedView.defns
                    tuf
                    (bimap (Map.keysSet . Map.filter id . snd) (Map.keysSet . Map.filter id . snd) addedOrUpdatedNamespaceBindings0)
                    hydratedDependents

              -- The types that depend on an updated type get new references too, which printing them into the file
              -- and parsing it gives them, as the slow path does
              declFile <-
                if Map.null propagation.updatedTypes && Map.null hydratedDependents.types
                  then pure (Just tuf)
                  else lift (typecheckDependents Map.empty Set.empty)

              case declFile of
                Nothing -> pure Propagated'Failed
                Just declFile -> do
                  let declRefs :: Map Name TypeReferenceId
                      declRefs =
                        Map.fromList $
                          [(Name.unsafeParseVar var, ref) | (var, (ref, _)) <- Map.toList (UF.dataDeclarationsId' declFile)]
                            ++ [(Name.unsafeParseVar var, ref) | (var, (ref, _)) <- Map.toList (UF.effectDeclarationsId' declFile)]
                  let (typeUpdates, divergedTypes) =
                        typeUpdatesOf
                          unconflictedView.defns.types
                          declRefs
                          (Map.elems propagation.updatedTypes ++ map fst (Map.elems hydratedDependents.types))
                  let retype = retypeWith typeUpdates

                  -- A term whose type is the same once it names the new types keeps its type
                  let changed =
                        Set.fromList
                          [oldRef | (oldRef, newType, oldType) <- propagation.updatedTermTypes, Just newType /= fmap retype oldType]

                  -- A dependent has to be typechecked again if it builds or matches on a value of a type that changed, or
                  -- mentions a type whose names went separate ways
                  let seeds =
                        propagation.printed
                          <> Map.keysSet
                            ( Map.filter
                                ( \(_, (term, typ)) ->
                                    not (Set.disjoint (Term.constructorDependencies term) (Map.keysSet typeUpdates))
                                      || not (Set.disjoint (Term.typeDependencies term <> Type.dependencies typ) divergedTypes)
                                )
                                hydratedDependents.terms
                            )
                  let retyped =
                        if Map.null typeUpdates
                          then Map.empty
                          else Map.fromList [(Reference.fromId ref, retype typ) | (ref, (_, typ)) <- Map.elems hydratedDependents.terms]

                  frontier <-
                    if Set.null changed && Set.null seeds
                      then pure (Frontier'Typechecked Map.empty)
                      else do
                        lift . respondRegion . Output.Literal . Pretty.wrap $
                          if Set.null changed && Map.null typeUpdates && Set.null divergedTypes
                            then "That's done. Some of that code needs a closer look, so I'm typechecking those parts again..."
                            else "That's done. Some of that code uses a type that changed, so I'm typechecking those parts again..."
                        lift $
                          typecheckFrontier
                            (\names -> fmap (termsNamed names) <$> typecheckDependents retyped names)
                            retype
                            hydratedDependents.terms
                            seeds
                            changed
                  case frontier of
                    Frontier'GaveUp -> hoistMaybe Nothing
                    Frontier'Failed -> pure Propagated'Failed
                    Frontier'Typechecked retypechecked -> do
                      file <-
                        hoistMaybe $
                          substituteDependents tuf declFile propagation.updated typeUpdates retypechecked hydratedDependents.terms
                      lift . respondRegion . Output.Literal . Pretty.wrap $
                        if not (Map.null retypechecked)
                          then "Everything typechecks, so I'm saving the results..."
                          else
                            if Map.null typeUpdates
                              then "That's done. Nothing that code uses changed its type, so I'm saving the results without typechecking it again..."
                              else "That's done. None of that code looks inside the types that changed, so I'm saving the results without typechecking it again..."
                      pure (Propagated'File file)

        -- A dependent that fails to typecheck against the types the others keep fails in the whole file too
        let frontierFailed = case propagated of
              Just Propagated'Failed -> True
              _ -> False

        secondTuf <- do
          case defnsAreEmpty dependents of
            -- If there are no dependents of the updates, then just use the already-typechecked file.
            True -> pure tuf
            False | Just (Propagated'File file) <- propagated -> pure file
            False -> do
              unless frontierFailed do
                respondRegion (Output.Literal (Pretty.wrap "That's done. Now I'm making sure everything typechecks..."))

              let prettyUnisonFile =
                    makePrettyUnisonFile
                      (Pretty.prettyUnisonFile ppe (UF.discardTypes tuf))
                      ( renderDefnsForUnisonFile
                          declNameLookup
                          ppe
                          Set.empty
                          (over (#terms . mapped) snd hydratedDependents)
                      )

              typechecked <-
                if frontierFailed
                  then pure Nothing
                  else do
                    parsingEnv <- Cli.makeParsingEnv pp namesIncludingLibdeps
                    parseAndTypecheck prettyUnisonFile parsingEnv

              secondTuf <-
                pure typechecked & onNothingM do
                  if useUpdateV2
                    then do
                      let nextNamespace :: Branch IO
                          nextNamespace =
                            unconflictedView.defns
                              & bimap
                                ( BiMultimap.range
                                    >>> ( `Map.withoutKeys`
                                            -- Toss (by name):
                                            --   1. the terms in the file
                                            --   2. the old constructors of the types in the file
                                            ( Map.foldlWithKey'
                                                ( \acc typeName isUpdate ->
                                                    if isUpdate
                                                      then
                                                        foldr
                                                          Set.insert
                                                          acc
                                                          ( Map.findWithDefault
                                                              [] -- impossible, could error
                                                              typeName
                                                              declNameLookup.declToConstructors
                                                          )
                                                      else acc
                                                )
                                                namespaceBindings.terms
                                                (snd addedOrUpdatedNamespaceBindings0.types)
                                            )
                                        )
                                )
                                (BiMultimap.range >>> (`Map.withoutKeys` namespaceBindings.types))
                              & subtractDependents dependentsRefs
                              & Branch.fromUnconflictedDefns
                              & Branch.setLibdeps (Branch.getAt0 (Path.singleton NameSegment.libSegment) currentBranch0)
                              & (`Branch.cons` currentBranch)

                      if pp.branch.isUpdate || pp.branch.isUpgrade || pp.branch.isMerge
                        then do
                          Cli.updateProjectBranchRoot_ pp.branch "update" (const nextNamespace)
                          scratchFilePath <- fst <$> Cli.expectLatestFile
                          liftIO $ env.writeSource (Text.pack scratchFilePath) (Pretty.toPlain 80 prettyUnisonFile) True
                          done
                            ( Output.UpdateTypecheckingFailure
                                Output.ScratchFileHandoff
                                  { action = "update",
                                    scratchFile = scratchFilePath,
                                    temporaryBranch = pp.branch.name,
                                    baseBranch = pp.branch.name,
                                    project = pp.project.name,
                                    loadFirst = isNothing env.watchState
                                  }
                            )
                        else do
                          uniqueTypeGuidsByName <-
                            Cli.runTransaction (makeUniqueTypeGuids (BiMultimap.range unconflictedView.defns.types))

                          (_updateBranchId, temporaryBranch) <-
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
                                      & TextBuilder.toText
                                      & unsafeFrom @Text
                                  )
                              )
                          scratchFilePath <-
                            ScratchFile.writeActionScratchFile
                              "update"
                              [projectBranchNameToValidProjectBranchNameText pp.branch.name & TextBuilder.toText]
                              ( into @Text (ProjectAndBranch pp.project.name pp.branch.name)
                                  : into @Text (Branch.headHash currentBranch)
                                  : Set.toAscList (Set.map Name.toText (namespaceBindings.terms <> namespaceBindings.types))
                              )
                              (Pretty.toPlain 80 prettyUnisonFile)
                          done
                            ( Output.UpdateTypecheckingFailure2
                                Output.ScratchFileHandoff
                                  { action = "update",
                                    scratchFile = scratchFilePath,
                                    temporaryBranch,
                                    baseBranch = pp.branch.name,
                                    project = pp.project.name,
                                    loadFirst = isNothing env.watchState
                                  }
                            )
                    else do
                      scratchFilePath <- fst <$> Cli.expectLatestFile
                      #latestFile ?= (scratchFilePath, True)
                      liftIO $ env.writeSource (Text.pack scratchFilePath) (Pretty.toPlain 80 prettyUnisonFile) True
                      done
                        ( Output.UpdateTypecheckingFailure
                            Output.ScratchFileHandoff
                              { action = "update",
                                scratchFile = scratchFilePath,
                                temporaryBranch = pp.branch.name,
                                baseBranch = pp.branch.name,
                                project = pp.project.name,
                                loadFirst = isNothing env.watchState
                              }
                        )

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

        -- Special case: we are running a successful `update` on a merge/update/upgrade branch that has a parent (such
        -- branches won't have a parent only if the parent has been deleted for some reason).
        case (pp.branch.isUpdate || pp.branch.isUpgrade || pp.branch.isMerge, pp.branch.parentBranchId) of
          (True, Just parentBranchId) -> do
            -- Switch to the parent branch
            parentBranch <-
              Cli.runTransaction do
                Queries.expectProjectBranch projectId parentBranchId
            Cli.switchProject (ProjectAndBranch projectId parentBranch.branchId)

            -- Merge into the parent branch. This isn't guaranteed to succeed, but it probably will.

            Merge.doMergeLocalBranch
              Merge.TwoWay
                { alice = ProjectAndBranch pp.project parentBranch,
                  bob = ProjectAndBranch pp.project pp.branch
                }

            -- If the merge succeeded, delete the current branch. We may want to try to delete it even if the merge
            -- fails, because otherwise the user will have to manually clean it up, which isn't as nice as a successful
            -- `update` on an update branch. However, it's very likely that the merge is simply a fast-forward.

            DeleteBranch.doDeleteProjectBranch (ProjectAndBranch pp.project pp.branch)
          _ -> pure ()

        pure Output.Success

  Cli.respond finalOutput

-- | Set @UNISON_UPDATE_TYPECHECK_DEPENDENTS@ to typecheck an update's dependents again even when no type changed, the
-- way `update` always did.
typecheckDependentsAlways :: Bool
typecheckDependentsAlways =
  isJust . unsafePerformIO $ lookupEnv "UNISON_UPDATE_TYPECHECK_DEPENDENTS"
{-# NOINLINE typecheckDependentsAlways #-}

-- | The outcome of propagating an update to its dependents: the file to save, or a dependent that doesn't typecheck.
data Propagated
  = Propagated'File (TypecheckedUnisonFile Symbol Ann)
  | Propagated'Failed

-- | How 'typecheckFrontier' ends.
data Frontier
  = Frontier'Typechecked (Map Name (Term Symbol Ann, Type Symbol Ann))
  | Frontier'Failed
  | Frontier'GaveUp

-- | What 'planPropagation' finds about the updates: each updated term's old reference with the variable that names it
-- in the file (unless it has an alias), each one's old reference with its new type and its old one, each updated type's
-- old reference, and the dependents that can only be printed.
data Propagation = Propagation
  { updated :: Map TermReferenceId Symbol,
    updatedTermTypes :: [(TermReferenceId, Type Symbol Ann, Maybe (Type Symbol Ann))],
    updatedTypes :: Map Name TypeReferenceId,
    printed :: Set Name
  }

-- | @planPropagation codebase namespace file updates dependents@ checks that an update can reach its dependents without
-- printing them all into the file and typechecking them again (#5790).
--
-- A dependent that uses no term whose type changed, and doesn't build or match on a value of a type that changed, keeps
-- its own type (with the new references of the types in it), and only its references change: to an updated term they
-- become that term in the file, to another dependent, that dependent, and to an updated type, its new reference. Only
-- the other dependents are typechecked again ('typecheckFrontier'), and 'substituteDependents' then hashes them all with
-- the file, cycles included. That gives the result that printing the dependents into the file and parsing it back
-- gives, with most of the typechecking skipped.
--
-- An alias is left to the printer where its choice of name matters, by printing the dependents it affects (@printed@).
-- A reference to an updated term with an alias follows whichever name is printed, which may not be the updated one. A
-- recursive term printed under two names refers to one of them, so the other leaves its cycle. A term that binds a
-- local variable named like something it refers to gets that name resolved by the printer, where a substitution would
-- capture it. An aliased dependent that isn't recursive needs none of this: both names print the same term.
--
-- Returns 'Nothing' when the slow path is needed: an updated type has parameters, or changes between data and ability
-- (either could change its kind, which a substitution can't check).
--
-- @updates@ holds the names the file updates (not the ones it adds).
planPropagation ::
  Codebase IO Symbol Ann ->
  Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
  TypecheckedUnisonFile Symbol Ann ->
  DefnsF Set Name Name ->
  DefnsF (Map Name) (TermReferenceId, (Term Symbol Ann, Type Symbol Ann)) (TypeReferenceId, Decl Symbol Ann) ->
  Transaction (Maybe Propagation)
planPropagation codebase namespace file updates dependents = runMaybeT do
  let aliased :: TermReferenceId -> Bool
      aliased ref =
        Set.size (BiMultimap.lookupDom (Referent.fromTermReferenceId ref) namespace.terms) > 1

  let fileTerms :: Map Name (Symbol, Type Symbol Ann)
      fileTerms =
        Map.fromList
          [ (Name.unsafeParseVar var, (var, typ))
          | (var, (_, _, Nothing, _, typ)) <- Map.toList (UF.hashTermsId file)
          ]

  let fileDecls :: Map Name (Decl Symbol Ann)
      fileDecls =
        Map.fromList $
          [(Name.unsafeParseVar var, Right decl) | (var, (_, decl)) <- Map.toList (UF.dataDeclarationsId' file)]
            ++ [(Name.unsafeParseVar var, Left decl) | (var, (_, decl)) <- Map.toList (UF.effectDeclarationsId' file)]

  -- Each updated type's old reference, when its kind can't have changed
  updatedTypes <-
    for (Set.toList updates.types) \name -> do
      oldRef <-
        case BiMultimap.lookupRan name namespace.types of
          Just (Reference.DerivedId ref) -> pure ref
          _ -> hoistMaybe Nothing
      newDecl <- hoistMaybe (Map.lookup name fileDecls)
      oldDecl <- MaybeT (Codebase.getTypeDeclaration codebase oldRef)
      guard (null (Decl.bound (Decl.asDataDecl oldDecl)) && null (Decl.bound (Decl.asDataDecl newDecl)))
      guard (isLeft oldDecl == isLeft newDecl)
      pure (name, oldRef)

  -- Each updated term's old reference, with the variable that names it in the file, its new type and its old one
  let updatedTypeRefs = Set.fromList [Reference.fromId oldRef | (_, oldRef) <- updatedTypes]
  updatedTerms <-
    fmap catMaybes . for (Set.toList updates.terms) $ \name ->
      case BiMultimap.lookupRan name namespace.terms of
        -- A constructor of an updated type is updated with its type
        Just referent@(Referent.Con _ _) -> do
          guard (Set.member (Referent.toReference referent) updatedTypeRefs)
          pure Nothing
        Just (Referent.Ref (Reference.DerivedId oldRef)) -> do
          (var, typ) <- hoistMaybe (Map.lookup name fileTerms)
          oldType <- lift (Codebase.getTypeOfTerm codebase (Reference.fromId oldRef))
          pure (Just (oldRef, var, typ, oldType))
        _ -> hoistMaybe Nothing

  let updated :: Map TermReferenceId Symbol
      updated = Map.fromList [(oldRef, var) | (oldRef, var, _, _) <- updatedTerms, not (aliased oldRef)]

  let aliasedUpdates :: Set TermReferenceId
      aliasedUpdates = Set.fromList [oldRef | (oldRef, _, _, _) <- updatedTerms, aliased oldRef]

  -- The components holding a recursive dependent with an alias
  let aliasedCycles :: Set Hash
      aliasedCycles =
        Set.fromList
          [ Reference.idToHash ref
          | (ref, (term, _)) <- Map.elems dependents.terms,
            aliased ref,
            Set.member (Reference.idToHash ref) (Set.map Reference.idToHash (termRefsIn term))
          ]

  let replacements = substitutions updated dependents.terms
  let captured = foldMap (capturedIn replacements . fst . snd) dependents.terms

  let mustPrint :: TermReferenceId -> Term Symbol Ann -> Bool
      mustPrint ref term =
        let refs = termRefsIn term
         in not (Set.disjoint refs aliasedUpdates)
              || not (Set.disjoint (Set.map Reference.idToHash refs) aliasedCycles)
              || not (Set.null (capturedIn replacements term))
              || Set.member ref captured

  pure
    Propagation
      { updated,
        updatedTermTypes = [(oldRef, typ, oldType) | (oldRef, _, typ, oldType) <- updatedTerms],
        updatedTypes = Map.fromList updatedTypes,
        printed = Map.keysSet (Map.filter (\(ref, (term, _)) -> mustPrint ref term) dependents.terms)
      }

-- | What each term reference becomes in the file: an updated term or a dependent, as a variable.
substitutions ::
  Map TermReferenceId Symbol ->
  Map Name (TermReferenceId, (Term Symbol Ann, Type Symbol Ann)) ->
  Map TermReferenceId Symbol
substitutions updated dependents =
  updated <> Map.fromList [(ref, Name.toVar name) | (name, (ref, _)) <- Map.toList dependents]

-- | The term references a term makes.
termRefsIn :: Term Symbol Ann -> Set TermReferenceId
termRefsIn term =
  Set.fromList [ref | Term.Ref' (Reference.DerivedId ref) <- ABT.subterms term]

-- | The references in a term whose substitution would be captured by a local variable of the same name.
capturedIn :: Map TermReferenceId Symbol -> Term Symbol Ann -> Set TermReferenceId
capturedIn replacements term =
  Set.filter (maybe False (`Set.member` binders term) . (`Map.lookup` replacements)) (termRefsIn term)
  where
    binders :: Term Symbol Ann -> Set Symbol
    binders subterm =
      case ABT.out subterm of
        ABT.Var _ -> Set.empty
        ABT.Cycle body -> binders body
        ABT.Abs var body -> Set.insert var (binders body)
        ABT.Tm body -> foldMap binders body

-- | @typeUpdatesOf namespaceTypes declRefs oldRefs@ works out where each old type reference goes, given the new reference
-- of each name the file defines. The names of a type usually go together; when they don't (one of two aliases is
-- updated, or a recursive type printed under two names leaves its cycle) a reference to it follows whichever name is
-- printed, so it can't be substituted and is returned in the second part instead.
typeUpdatesOf ::
  BiMultimap TypeReference Name ->
  Map Name TypeReferenceId ->
  [TypeReferenceId] ->
  (Map TypeReference TypeReference, Set TypeReference)
typeUpdatesOf namespaceTypes declRefs oldRefs =
  ( Map.fromList
      [ (old, new)
      | (old, targets) <- destinations,
        Just new <- [Set.asSingleton targets],
        new /= old
      ],
    Set.fromList [old | (old, targets) <- destinations, Set.size targets > 1]
  )
  where
    destinations :: [(TypeReference, Set TypeReference)]
    destinations =
      [ (old, Set.map (\name -> maybe old Reference.fromId (Map.lookup name declRefs)) (BiMultimap.lookupDom old namespaceTypes))
      | old <- map Reference.fromId oldRefs
      ]

-- | @typecheckFrontier typecheck retype dependents seeds changed@ typechecks again the dependents in @seeds@ and the
-- ones that refer to a term whose type changed, then the ones that refer to one of those whose type changed in turn,
-- and so on: the type-preserving frontier of #904. Each round typechecks everything found so far, since the next round
-- may refer to it. A type is compared with the old one as @retype@ gives it, with the new references of updated types.
--
-- A dependent outside the frontier keeps its type, so the frontier can be typechecked against its old reference, given
-- that type, and 'substituteDependents' then replaces the reference. Dependents are printed with their signatures, so
-- the frontier usually ends one round past the update.
--
-- It fails when a dependent doesn't typecheck, and gives up (for the slow path to run instead) when the frontier is
-- still growing after ten rounds, or a round doesn't give back every dependent it was asked about.
typecheckFrontier ::
  (Monad m) =>
  (Set Name -> m (Maybe (Map Name (Term Symbol Ann, Type Symbol Ann)))) ->
  (Type Symbol Ann -> Type Symbol Ann) ->
  Map Name (TermReferenceId, (Term Symbol Ann, Type Symbol Ann)) ->
  Set Name ->
  Set TermReferenceId ->
  m Frontier
typecheckFrontier typecheck retype dependents seeds =
  go (10 :: Int) Set.empty Map.empty
  where
    go rounds frontier typechecked changed
      | Set.null next = pure (Frontier'Typechecked typechecked)
      | rounds == 0 = pure Frontier'GaveUp
      | otherwise =
          typecheck frontier' >>= \case
            Nothing -> pure Frontier'Failed
            Just typechecked'
              | Map.keysSet typechecked' == frontier' -> go (rounds - 1) frontier' typechecked' (changed <> changedIn typechecked')
              | otherwise -> pure Frontier'GaveUp
      where
        next =
          Map.keysSet $
            Map.filterWithKey
              (\name (_, (term, _)) -> Set.notMember name frontier && (Set.member name seeds || refersTo changed term))
              dependents
        frontier' = frontier <> next

    changedIn :: Map Name (Term Symbol Ann, Type Symbol Ann) -> Set TermReferenceId
    changedIn typechecked =
      Set.fromList
        [ ref
        | (name, (_, typ)) <- Map.toList typechecked,
          Just (ref, (_, oldType)) <- [Map.lookup name dependents],
          typ /= retype oldType
        ]

    refersTo :: Set TermReferenceId -> Term Symbol Ann -> Bool
    refersTo refs term =
      not (Set.disjoint refs (termRefsIn term))

-- | A stored type with every updated type's new reference in place of its old one. An ability's new reference can sort
-- differently from its old one, and a list of abilities is kept sorted, so the list is sorted again.
retypeWith :: Map TypeReference TypeReference -> Type Symbol Ann -> Type Symbol Ann
retypeWith typeUpdates
  | Map.null typeUpdates = id
  | otherwise = Type.cleanupAbilityLists . Type.updateDependencies typeUpdates

-- | The named terms in a typechecked file, with their types. A test in the codebase prints as a test watch, so those
-- count too.
termsNamed :: Set Name -> TypecheckedUnisonFile Symbol Ann -> Map Name (Term Symbol Ann, Type Symbol Ann)
termsNamed names typechecked =
  Map.fromList
    [ (name, (term, typ))
    | (var, _, term, typ) <- concat (UF.topLevelComponents typechecked),
      let name = Name.unsafeParseVar var,
      Set.member name names
    ]

-- | 'parseAndTypecheck', with the types of some terms in the codebase given rather than looked up: the dependents
-- outside the frontier, whose types name the updated types' new references.
parseAndTypecheckRetyped ::
  Map TermReference (Type Symbol Ann) ->
  Pretty ColorText ->
  Parser.ParsingEnv Transaction ->
  Cli (Maybe (TypecheckedUnisonFile Symbol Ann))
parseAndTypecheckRetyped retyped prettyUnisonFile parsingEnv
  | Map.null retyped = parseAndTypecheck prettyUnisonFile parsingEnv
  | otherwise = do
      env <- ask
      Cli.runTransaction do
        Parsers.parseFile "<update>" (Text.unpack (Pretty.toPlain 80 prettyUnisonFile)) parsingEnv >>= \case
          Left _ -> pure Nothing
          Right unisonFile -> do
            typecheckingEnv <-
              computeTypecheckingEnvironment (FileParsers.ShouldUseTndr'Yes parsingEnv) env.codebase [] unisonFile
            let typeLookup = typecheckingEnv.typeLookup
            let retypedEnv =
                  typecheckingEnv {Typechecker.typeLookup = typeLookup {TL.typeOfTerms = Map.union retyped typeLookup.typeOfTerms}}
            pure (Result.result (FileParsers.synthesizeFile retypedEnv unisonFile))

-- | @substituteDependents file declFile updated typeUpdates typechecked dependents@ gives every dependent its new
-- references and hashes it with the file and the types in @declFile@: the ones in @typechecked@ as they typechecked,
-- and the rest as they are stored. A term link is left alone, as printing and parsing leaves it: the parser resolves it
-- against the namespace and the file's constructors, not the file's terms (a dependent with a link to a constructor of
-- an updated type is typechecked again instead). A type link takes the type's new reference, as the parser resolves it
-- to the type printed into the file.
--
-- Returns 'Nothing' when a substitution would still be captured by a local variable, which only a term in the file
-- itself can do: 'planPropagation' has the dependents where that could happen printed.
substituteDependents ::
  TypecheckedUnisonFile Symbol Ann ->
  TypecheckedUnisonFile Symbol Ann ->
  Map TermReferenceId Symbol ->
  Map TypeReference TypeReference ->
  Map Name (Term Symbol Ann, Type Symbol Ann) ->
  Map Name (TermReferenceId, (Term Symbol Ann, Type Symbol Ann)) ->
  Maybe (TypecheckedUnisonFile Symbol Ann)
substituteDependents file declFile updated typeUpdates typechecked dependents = do
  let replacements = substitutions updated dependents

  let replaceTerms :: Term Symbol Ann -> Term Symbol Ann
      replaceTerms =
        ABT.visitPure \case
          term@(Term.Ref' (Reference.DerivedId ref)) -> Term.var (ABT.annotation term) <$> Map.lookup ref replacements
          _ -> Nothing

  -- Annotations and type links inside a term name types, which take their new references, as the parser resolves them
  -- to the types printed into the file
  let replaceTypes :: Term Symbol Ann -> Term Symbol Ann
      replaceTypes =
        ABT.rebuildUp \case
          Term.Ann term typ -> Term.Ann term (Type.updateDependencies typeUpdates typ)
          Term.TypeLink ref -> Term.TypeLink (Map.findWithDefault ref ref typeUpdates)
          other -> other

  let dependentBindings :: [[(Symbol, Ann, Term Symbol Ann, Type Symbol Ann)]]
      dependentBindings =
        [ [(Name.toVar name, ABT.annotation term, term, retypeWith typeUpdates typ)]
        | (name, (_, stored)) <- Map.toList dependents,
          let (term, typ) = Map.findWithDefault stored name typechecked
        ]

  let everyTerm =
        [term | (_, _, term, _) <- concat dependentBindings ++ concat (UF.topLevelComponents' file)]
          ++ [term | (_, terms) <- UF.watchComponents file, (_, _, term, _) <- terms]
  guard (all (Set.null . capturedIn replacements) everyTerm)

  let replaceIn = map (over _3 (replaceTerms . replaceTypes))
  pure $
    UF.typecheckedUnisonFile
      (UF.dataDeclarationsId' declFile)
      (UF.effectDeclarationsId' declFile)
      (map replaceIn (UF.topLevelComponents' file ++ dependentBindings))
      (map (second replaceIn) (UF.watchComponents file))

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
    <> renderDefns dependents.types
    <> renderDefns dependents.terms
  where
    renderDefns :: Map Name (Pretty ColorText) -> Pretty ColorText
    renderDefns =
      foldMap (\(_, defn) -> defn <> Pretty.newline <> Pretty.newline)
        . sortAlphabeticallyOn fst
        . Map.toList

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
  PPED.leftBiased
    [ let names = initialFileNames <> Names.fromUnconflictedReferenceIds dependents
       in PPED.makePPED (PPE.namer names) (PPE.suffixifyByName (Names.shadowing names namespaceNames)),
      PPED.makePPED
        (PPE.hqNamer hashLen namespaceNames)
        -- We don't want to over-suffixify for a reference in the namespace. For example, say we have "foo.bar" in the
        -- namespace and "oink.bar" in the file. "bar" may be a unique suffix among the namespace names, but would be
        -- ambiguous in the context of namespace + file names.
        --
        -- So, we use `shadowing`, which starts with the LHS names (the namespace), and adds to it names from the
        -- RHS (the initial file names, i.e. what was originally saved) that don't already exist in the LHS.
        (PPE.suffixifyByHash (Names.shadowing namespaceNames initialFileNames))
    ]
