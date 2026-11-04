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
import Unison.Merge qualified as Merge
import Unison.Name (Name)
import Unison.Name qualified as Name
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
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Sqlite (Transaction)
import Unison.Symbol (Symbol)
import Unison.Syntax.FilePrinter (renderDefnsForUnisonFile)
import Unison.Syntax.Name qualified as Name
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.Type (Type)
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

        -- Typechecks the file with the named dependents printed into it, and returns what they typechecked to.
        let typecheckDependents :: Set Name -> Cli (Maybe (Map Name (Term Symbol Ann, Type Symbol Ann)))
            typecheckDependents names = do
              parsingEnv <- Cli.makeParsingEnv pp namesIncludingLibdeps
              let prettyUnisonFile =
                    makePrettyUnisonFile
                      (Pretty.prettyUnisonFile ppe (UF.discardTypes tuf))
                      ( renderDefnsForUnisonFile
                          declNameLookup
                          ppe
                          Set.empty
                          (Defns (snd <$> Map.restrictKeys hydratedDependents.terms names) Map.empty)
                      )
              parseAndTypecheck prettyUnisonFile parsingEnv <&> fmap \typechecked ->
                Map.fromList
                  [ (name, (term, typ))
                  | (var, _, term, typ) <- concat (UF.topLevelComponents' typechecked),
                    let name = Name.unsafeParseVar var,
                    Set.member name names
                  ]

        -- Dependents that use no type that changed take their new references without being typechecked again
        propagated <-
          if defnsAreEmpty dependents || typecheckDependentsAlways
            then pure Nothing
            else runMaybeT do
              propagation <-
                MaybeT . Cli.runTransaction $
                  planPropagation
                    env.codebase
                    unconflictedView.defns.terms
                    tuf
                    (bimap (Map.keysSet . Map.filter id . snd) (Map.keysSet . Map.filter id . snd) addedOrUpdatedNamespaceBindings0)
                    hydratedDependents
              frontier <-
                if Set.null propagation.changed
                  then pure (Frontier'Typechecked Map.empty)
                  else do
                    lift . respondRegion . Output.Literal $
                      Pretty.wrap "That's done. Some of that code uses a type that changed, so I'm typechecking those parts again..."
                    lift (typecheckFrontier typecheckDependents hydratedDependents.terms propagation.changed)
              case frontier of
                Frontier'GaveUp -> hoistMaybe Nothing
                Frontier'Failed -> pure Propagated'Failed
                Frontier'Typechecked retypechecked -> do
                  file <- hoistMaybe (substituteDependents tuf propagation.updated retypechecked hydratedDependents.terms)
                  lift . respondRegion . Output.Literal . Pretty.wrap $
                    if Map.null retypechecked
                      then "That's done. Nothing that code uses changed its type, so I'm saving the results without typechecking it again..."
                      else "Everything typechecks, so I'm saving the results..."
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

-- | What 'planPropagation' finds about the updated terms: each one's old reference with the variable that names it in
-- the file, and the old references of those whose type changed.
data Propagation = Propagation
  { updated :: Map TermReferenceId Symbol,
    changed :: Set TermReferenceId
  }

-- | @planPropagation codebase namespaceTerms file updates dependents@ checks that an update can reach its dependents
-- without printing them all into the file and typechecking them again (#5790).
--
-- A dependent that uses no type that changed keeps its own type, and only its references change: to an updated term
-- they become that term in the file, and to another dependent, that dependent. Only the dependents that do use a
-- changed type are typechecked again ('typecheckFrontier'), and 'substituteDependents' then hashes them all with the
-- file, cycles included. That gives the result that printing the dependents into the file and parsing it back gives,
-- with most of the typechecking skipped.
--
-- Returns 'Nothing' when the slow path is needed: a type declaration is updated or depends on an update, or a name
-- involved is one of several aliases (the round trip can split an alias from its cycle, and this doesn't try to match
-- it).
--
-- @updates@ holds the names the file updates (not the ones it adds).
planPropagation ::
  Codebase IO Symbol Ann ->
  BiMultimap Referent Name ->
  TypecheckedUnisonFile Symbol Ann ->
  DefnsF Set Name Name ->
  DefnsF (Map Name) (TermReferenceId, (Term Symbol Ann, Type Symbol Ann)) (TypeReferenceId, Decl Symbol Ann) ->
  Transaction (Maybe Propagation)
planPropagation codebase namespaceTerms file updates dependents
  | not (Set.null updates.types) || not (Map.null dependents.types) = pure Nothing
  | otherwise = runMaybeT do
      let onlyName :: TermReferenceId -> Name -> Bool
          onlyName ref name =
            BiMultimap.lookupDom (Referent.fromTermReferenceId ref) namespaceTerms == Set.singleton name

      let fileTerms :: Map Name (Symbol, Type Symbol Ann)
          fileTerms =
            Map.fromList
              [ (Name.unsafeParseVar var, (var, typ))
              | (var, (_, _, Nothing, _, typ)) <- Map.toList (UF.hashTermsId file)
              ]

      -- Each updated term's old reference, with the variable that names it in the file and its new type
      updatedTerms <-
        for (Set.toList updates.terms) \name -> do
          oldRef <-
            case BiMultimap.lookupRan name namespaceTerms of
              Just (Referent.Ref (Reference.DerivedId ref)) -> pure ref
              _ -> hoistMaybe Nothing
          guard (onlyName oldRef name)
          (var, typ) <- hoistMaybe (Map.lookup name fileTerms)
          pure (oldRef, (var, typ))

      for_ (Map.toList dependents.terms) \(name, (ref, _)) -> guard (onlyName ref name)

      changedTypes <-
        lift $ for updatedTerms \(oldRef, (_, newType)) -> do
          oldType <- Codebase.getTypeOfTerm codebase (Reference.fromId oldRef)
          pure [oldRef | oldType /= Just newType]

      pure
        Propagation
          { updated = Map.fromList (map (second fst) updatedTerms),
            changed = Set.fromList (concat changedTypes)
          }

-- | @typecheckFrontier typecheck dependents changed@ typechecks again the dependents that refer to a term whose type
-- changed, then the ones that refer to one of those whose type changed in turn, and so on: the type-preserving frontier
-- of #904. Each round typechecks everything found so far, since the next round may refer to it.
--
-- A dependent outside the frontier keeps its type, so the frontier can be typechecked against its old reference, which
-- 'substituteDependents' then replaces. Dependents are printed with their signatures, so the frontier usually ends one
-- round past the update.
--
-- It fails when a dependent doesn't typecheck, and gives up (for the slow path to run instead) when the frontier is
-- still growing after ten rounds, or a round doesn't give back every dependent it was asked about.
typecheckFrontier ::
  (Monad m) =>
  (Set Name -> m (Maybe (Map Name (Term Symbol Ann, Type Symbol Ann)))) ->
  Map Name (TermReferenceId, (Term Symbol Ann, Type Symbol Ann)) ->
  Set TermReferenceId ->
  m Frontier
typecheckFrontier typecheck dependents =
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
        next = Map.keysSet (Map.filterWithKey (\name (_, (term, _)) -> Set.notMember name frontier && refersTo changed term) dependents)
        frontier' = frontier <> next

    changedIn :: Map Name (Term Symbol Ann, Type Symbol Ann) -> Set TermReferenceId
    changedIn typechecked =
      Set.fromList
        [ ref
        | (name, (_, typ)) <- Map.toList typechecked,
          Just (ref, (_, oldType)) <- [Map.lookup name dependents],
          typ /= oldType
        ]

    refersTo :: Set TermReferenceId -> Term Symbol Ann -> Bool
    refersTo refs term =
      or [Set.member ref refs | Term.Ref' (Reference.DerivedId ref) <- ABT.subterms term]

-- | @substituteDependents file updated typechecked dependents@ gives every dependent its new references and hashes it
-- with the file: the ones in @typechecked@ as they typechecked, and the rest as they are stored. Term and type links are
-- left alone, as printing and parsing leaves them (it resolves them against the namespace, not the file).
--
-- Returns 'Nothing' when a substituted name is also bound inside a term, where a plain substitution would capture it.
substituteDependents ::
  TypecheckedUnisonFile Symbol Ann ->
  Map TermReferenceId Symbol ->
  Map Name (Term Symbol Ann, Type Symbol Ann) ->
  Map Name (TermReferenceId, (Term Symbol Ann, Type Symbol Ann)) ->
  Maybe (TypecheckedUnisonFile Symbol Ann)
substituteDependents file updated typechecked dependents = do
  let replacements :: Map TermReferenceId Symbol
      replacements =
        updated <> Map.fromList [(ref, Name.toVar name) | (name, (ref, _)) <- Map.toList dependents]

  let replace :: Term Symbol Ann -> Term Symbol Ann
      replace =
        ABT.visitPure \case
          term@(Term.Ref' (Reference.DerivedId ref)) -> Term.var (ABT.annotation term) <$> Map.lookup ref replacements
          _ -> Nothing

  let dependentBindings :: [[(Symbol, Ann, Term Symbol Ann, Type Symbol Ann)]]
      dependentBindings =
        [ [(Name.toVar name, ABT.annotation term, term, typ)]
        | (name, (_, stored)) <- Map.toList dependents,
          let (term, typ) = Map.findWithDefault stored name typechecked
        ]

  let replacementVars = Set.fromList (Map.elems replacements)
  let everyTerm =
        [term | (_, _, term, _) <- concat dependentBindings ++ concat (UF.topLevelComponents' file)]
          ++ [term | (_, terms) <- UF.watchComponents file, (_, _, term, _) <- terms]
  guard (all (Set.disjoint replacementVars . binders) everyTerm)

  let replaceIn = map (over _3 replace)
  pure $
    UF.typecheckedUnisonFile
      (UF.dataDeclarationsId' file)
      (UF.effectDeclarationsId' file)
      (map replaceIn (UF.topLevelComponents' file ++ dependentBindings))
      (map (second replaceIn) (UF.watchComponents file))
  where
    binders :: Term Symbol Ann -> Set Symbol
    binders term =
      case ABT.out term of
        ABT.Var _ -> Set.empty
        ABT.Cycle body -> binders body
        ABT.Abs var body -> Set.insert var (binders body)
        ABT.Tm body -> foldMap binders body

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
