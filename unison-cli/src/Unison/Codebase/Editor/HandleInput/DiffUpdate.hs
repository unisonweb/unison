-- | @diff.update@ input handler - shows a preview of what `update` would change.
module Unison.Codebase.Editor.HandleInput.DiffUpdate
  ( handleDiffUpdate,
  )
where

import Control.Monad.Reader.Class (ask)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import U.Codebase.Reference (TermReferenceId, TypeReferenceId)
import Unison.Builtin qualified as Builtin
import Unison.Cli.Monad (Cli, Env (..))
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.UpdateUtils (getNamespaceDependentsOf, hydrateRefs)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.BuiltinAnnotation (builtinAnnotation)
import Unison.Codebase.Editor.Output qualified as Output
import Unison.DataDeclaration (DeclOrBuiltin)
import Unison.DeclCoherencyCheck qualified as DeclCoherencyCheck
import Unison.Name (Name)
import Unison.Names (Names (Names))
import Unison.Names qualified as Names
import Unison.OrBuiltin (OrBuiltin (..))
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Reference (TermReference, TypeReference)
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
import Unison.Util.Set qualified as Set

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

      -- Throw away the dependents that are shadowed by the file itself
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

  let newFileTerms :: Map Name (Term Symbol Ann, Type Symbol Ann)
      newFileTerms = Map.restrictKeys fileTerms updatedTermNames

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

  -- Combine old and new terms for the updated definitions
  let updatedTerms :: Map Name ((Term Symbol Ann, Type Symbol Ann), (Term Symbol Ann, Type Symbol Ann))
      updatedTerms =
        Map.mapMaybe id $
          Map.intersectionWith
            (\refId newTerm ->
              case Map.lookup refId oldTerms of
                Just oldTerm -> Just (oldTerm, newTerm)
                Nothing -> Nothing
            )
            updatedTermRefIds
            newFileTerms

  -- Get type declarations from the file
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

  let newTypes :: Map Name (DeclOrBuiltin Symbol Ann)
      newTypes = Map.restrictKeys fileTypeDecls newTypeNames

  let updatedTypes :: Map Name (DeclOrBuiltin Symbol Ann)
      updatedTypes = Map.restrictKeys fileTypeDecls updatedTypeNames

  -- Build the PPE using namespace names
  let pped =
        PPED.makePPED
          (PPE.hqNamer 10 namesIncludingLibdeps)
          (PPE.suffixifyByHash namesIncludingLibdeps)
  let ppe = PPED.suffixifiedPPE pped

  -- Respond with the diff
  Cli.respond $
    Output.ShowUpdateDiff
      ppe
      Defns {terms = newTerms, types = newTypes}
      Defns {terms = updatedTerms, types = updatedTypes}
      dependents
