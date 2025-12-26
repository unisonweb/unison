-- | @diff.update@ input handler - shows a preview of what `update` would change.
module Unison.Codebase.Editor.HandleInput.DiffUpdate
  ( handleDiffUpdate,
  )
where

import Control.Monad.Reader.Class (ask)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import U.Codebase.Reference (TermReferenceId, TypeReferenceId)
import Unison.Cli.Monad (Cli, Env (..))
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.UpdateUtils (getNamespaceDependentsOf, hydrateRefs)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.Editor.Output qualified as Output
import Unison.DataDeclaration (Decl, DeclOrBuiltin)
import Unison.DeclCoherencyCheck qualified as DeclCoherencyCheck
import Unison.Name (Name)
import Unison.Names (Names (Names))
import Unison.Names qualified as Names
import Unison.OrBuiltin (OrBuiltin (..))
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.Symbol (Symbol)
import Unison.Syntax.Name qualified as Name
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.UnconflictedLocalDefnsView (UnconflictedLocalDefnsView (..))
import Unison.UnisonFile qualified as UF
import Unison.UnisonFile.Names qualified as UF
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Defns (Defns (..), DefnsF)
import Unison.Util.Relation qualified as Relation

handleDiffUpdate :: Cli ()
handleDiffUpdate = do
  env <- ask
  tuf <- Cli.expectLatestTypecheckedFile
  currentBranch <- Cli.getCurrentBranch
  let currentBranch0 = Branch.head currentBranch
  let namesIncludingLibdeps = Branch.toNames currentBranch0

  -- Assert that the namespace doesn't have any conflicted names
  unconflictedView <-
    Branch.asUnconflicted currentBranch0
      & onLeft (Cli.returnEarly . Output.ConflictedDefn)

  -- Assert that the namespace doesn't have any incoherent decls
  _declNameLookup <-
    Cli.runTransactionWithRollback \rollback -> do
      Codebase.getBranchDeclNameLookup env.codebase (Branch.namespaceHash currentBranch) unconflictedView
        & onLeftM (rollback . Output.IncoherentDeclDuringUpdate . DeclCoherencyCheck.asOneRandomIncoherentDeclReason)

  -- Get namespace bindings from the file (terms and types being added/updated)
  let namespaceBindings :: DefnsF Set Name Name
      namespaceBindings =
        bimap (Set.map Name.unsafeParseVar) (Set.map Name.unsafeParseVar) (UF.namespaceBindings tuf)

  -- Compute new vs updated definitions
  let existingTermNames = BiMultimap.ran unconflictedView.defns.terms
  let existingTypeNames = BiMultimap.ran unconflictedView.defns.types

  let newTermNames = Set.difference namespaceBindings.terms existingTermNames
  let newTypeNames = Set.difference namespaceBindings.types existingTypeNames
  let updatedTermNames = Set.intersection namespaceBindings.terms existingTermNames
  let updatedTypeNames = Set.intersection namespaceBindings.types existingTypeNames

  -- Get dependents that would need retypechecking
  dependents <-
    Cli.runTransaction do
      dependents0 <-
        getNamespaceDependentsOf
          unconflictedView.defns
          ( Names.references
              Names
                { terms = Relation.restrictDom namespaceBindings.terms unconflictedView.names.terms,
                  types = Relation.restrictDom namespaceBindings.types unconflictedView.names.types
                }
          )

      -- Remove dependents that are also being updated directly by the file,
      -- since they'll already appear in the "updated definitions" section
      let dependents1 :: DefnsF (Map Name) TermReferenceId TypeReferenceId
          dependents1 =
            bimap
              (`Map.withoutKeys` namespaceBindings.terms)
              (`Map.withoutKeys` namespaceBindings.types)
              dependents0

      pure dependents1

  -- Get the terms (body + type) for new and updated terms from the typechecked file
  -- hashTermsId returns: (ann, TermReferenceId, Maybe WatchKind, Term v a, Type v a)
  let fileTerms :: Map Name (Term Symbol Ann, Type Symbol Ann)
      fileTerms =
        Map.fromList
          [ (Name.unsafeParseVar var, (term, typ))
            | (var, (_, _, _, term, typ)) <- Map.toList (UF.hashTermsId tuf)
          ]

  let newTerms :: Map Name (Term Symbol Ann, Type Symbol Ann)
      newTerms = Map.restrictKeys fileTerms newTermNames

  -- Terms from the file that are updates to existing codebase definitions
  let updatedFileTerms :: Map Name (Term Symbol Ann, Type Symbol Ann)
      updatedFileTerms = Map.restrictKeys fileTerms updatedTermNames

  -- Get the old terms from the codebase for updated definitions
  -- First, get the term reference IDs for the updated names
  let updatedTermRefIds :: Map Name TermReferenceId
      updatedTermRefIds =
        Map.fromList
          [ (name, refId)
            | name <- Set.toList updatedTermNames,
              Just referent <- [Map.lookup name (BiMultimap.range unconflictedView.defns.terms)],
              Just refId <- [Referent.toTermReferenceId referent]
          ]

  -- Fetch the old terms from the codebase
  oldTerms <- Cli.runTransaction do
    let refIdSet = Set.fromList (Map.elems updatedTermRefIds)
    hydratedTerms <- hydrateRefs env.codebase (Defns refIdSet Set.empty)
    pure hydratedTerms.terms

  -- Intersect old and new terms to find updated definitions
  let updatedTerms :: Map Name ((Term Symbol Ann, Type Symbol Ann), (Term Symbol Ann, Type Symbol Ann))
      updatedTerms =
        Map.mapMaybe id $
          Map.intersectionWith
            ( \refId newTerm ->
                case Map.lookup refId oldTerms of
                  Just oldTerm -> Just (oldTerm, newTerm)
                  Nothing -> Nothing
            )
            updatedTermRefIds
            updatedFileTerms

  -- Get type declarations from the file (including reference IDs)
  let fileDataDecls :: Map Name (DeclOrBuiltin Symbol Ann)
      fileDataDecls =
        Map.fromList
          [ (Name.unsafeParseVar var, NotBuiltin (Right decl))
            | (var, (_, decl)) <- Map.toList (UF.dataDeclarationsId' tuf)
          ]

  let fileEffectDecls :: Map Name (DeclOrBuiltin Symbol Ann)
      fileEffectDecls =
        Map.fromList
          [ (Name.unsafeParseVar var, NotBuiltin (Left decl))
            | (var, (_, decl)) <- Map.toList (UF.effectDeclarationsId' tuf)
          ]

  let fileTypeDecls :: Map Name (DeclOrBuiltin Symbol Ann)
      fileTypeDecls = Map.union fileDataDecls fileEffectDecls

  -- File types with their reference IDs (for updated types rendering)
  let fileTypeDeclsWithRefIds :: Map Name (TypeReferenceId, Decl Symbol Ann)
      fileTypeDeclsWithRefIds =
        Map.fromList $
          [ (Name.unsafeParseVar var, (refId, Right decl))
            | (var, (refId, decl)) <- Map.toList (UF.dataDeclarationsId' tuf)
          ]
            ++ [ (Name.unsafeParseVar var, (refId, Left decl))
                 | (var, (refId, decl)) <- Map.toList (UF.effectDeclarationsId' tuf)
               ]

  let newTypes :: Map Name (DeclOrBuiltin Symbol Ann)
      newTypes = Map.restrictKeys fileTypeDecls newTypeNames

  -- Types from the file that are updates to existing codebase definitions
  let updatedFileTypes :: Map Name (TypeReferenceId, Decl Symbol Ann)
      updatedFileTypes = Map.restrictKeys fileTypeDeclsWithRefIds updatedTypeNames

  -- Get the old types from the codebase for updated definitions
  -- First, get the type reference IDs for the updated names
  let updatedTypeRefIds :: Map Name TypeReferenceId
      updatedTypeRefIds =
        Map.fromList
          [ (name, refId)
            | name <- Set.toList updatedTypeNames,
              Just typeRef <- [Map.lookup name (BiMultimap.range unconflictedView.defns.types)],
              Just refId <- [Reference.toId typeRef]
          ]

  -- Fetch the old types from the codebase
  oldTypes <- Cli.runTransaction do
    let refIdSet = Set.fromList (Map.elems updatedTypeRefIds)
    hydratedTypes <- hydrateRefs env.codebase (Defns Set.empty refIdSet)
    pure hydratedTypes.types

  -- Intersect old and new types to find updated definitions
  -- Result: Map Name ((old refId, old decl), (new refId, new decl))
  let updatedTypes :: Map Name ((TypeReferenceId, Decl Symbol Ann), (TypeReferenceId, Decl Symbol Ann))
      updatedTypes =
        Map.mapMaybe id $
          Map.intersectionWith
            ( \oldRefId (newRefId, newDecl) ->
                case Map.lookup oldRefId oldTypes of
                  Just oldDecl -> Just ((oldRefId, oldDecl), (newRefId, newDecl))
                  Nothing -> Nothing
            )
            updatedTypeRefIds
            updatedFileTypes

  -- Build the PPEs:
  -- - ppedNew: for new definitions (file names shadowing namespace names)
  -- - ppedOld: for old definitions (just namespace names, so old refs resolve properly)
  let fileNames = UF.typecheckedToNames tuf
  let allNames = fileNames `Names.shadowing` namesIncludingLibdeps
  let ppedNew =
        PPED.makePPED
          (PPE.hqNamer 10 allNames)
          (PPE.suffixifyByHash allNames)
  let ppedOld =
        PPED.makePPED
          (PPE.hqNamer 10 namesIncludingLibdeps)
          (PPE.suffixifyByHash namesIncludingLibdeps)

  -- Respond with the diff
  Cli.respond $
    Output.ShowUpdateDiff
      ppedNew
      ppedOld
      Defns {terms = newTerms, types = newTypes}
      Defns {terms = updatedTerms, types = updatedTypes}
      dependents
