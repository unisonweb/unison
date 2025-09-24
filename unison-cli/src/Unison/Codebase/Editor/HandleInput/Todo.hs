-- | @todo@ input handler
module Unison.Codebase.Editor.HandleInput.Todo
  ( handleTodo,
  )
where

import Control.Monad.Reader (ask)
import Data.Set qualified as Set
import U.Codebase.HashTags (BranchHash (..))
import U.Codebase.Sqlite.Operations qualified as Operations
import Unison.Builtin qualified as Builtin
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.Causal qualified as Causal
import Unison.Codebase.Editor.HandleInput.Merge2 (hasDefnsInLib)
import Unison.Codebase.Editor.Output
import Unison.DeclCoherencyCheck (checkAllDeclCoherency)
import Unison.Hash (HashFor (..))
import Unison.Names qualified as Names
import Unison.Prelude
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Reference (TermReference)
import Unison.Syntax.Name qualified as Name
import Unison.Util.Defns (Defns (..))
import Unison.Util.Set qualified as Set
import qualified Unison.Util.Defns as Defns

handleTodo :: Cli ()
handleTodo = do
  env <- ask

  -- For now, we don't go through any great trouble to seek out the root of the project branch. Just assume the current
  -- namespace is the root, which will be the case unless the user uses `deprecated.cd`.
  currentCausal <- Cli.getCurrentBranch
  let currentNamespace = Branch.head currentCausal
  let currentNamespaceWithoutLibdeps = Branch.deleteLibdeps currentNamespace

  (defnsInLib, dependentsOfTodo, directDependencies, hashLen, incoherentDeclReasons) <-
    Cli.runTransaction do
      -- We call a shared `hasDefnsLib` helper even though we could easily duplicate the logic with the branch in hand
      defnsInLib <- do
        branch <-
          currentCausal
            & Branch._history
            & Causal.valueHash
            & coerce @_ @BranchHash
            & Operations.expectBranchByBranchHash
        hasDefnsInLib branch

      let todoReference :: TermReference
          todoReference =
            Set.asSingleton (Names.refTermsNamed Builtin.names (Name.unsafeParseText "todo"))
              & fromMaybe (error (reportBug "E260496" "No reference for builtin named 'todo'"))

      -- All type-and-term dependents of the `todo` builtin, but we know they're all terms.
      dependentsOfTodo <-
        Operations.directDependentsWithinScope
          (Defns.fromTerms (Branch.deepTermReferenceIds currentNamespaceWithoutLibdeps))
          (Defns.fromTerms (Set.singleton todoReference))

      directDependencies <-
        Operations.directDependenciesOfScope
          Builtin.isBuiltinType
          Defns
            { terms = Branch.deepTermReferenceIds currentNamespaceWithoutLibdeps,
              types = Branch.deepTypeReferenceIds currentNamespaceWithoutLibdeps
            }

      hashLen <- Codebase.hashLength

      incoherentDeclReasons <-
        -- First try the happy path of an unconflicted namespace, which is cached in the Branch object. `todo` doesn't
        -- require that the namespace is unconflicted, so we fall back on a more expensive computation in that case
        case Branch.asUnconflicted currentNamespace of
          Right unconflictedView ->
            Codebase.getBranchDeclNameLookup env.codebase (Branch.namespaceHash currentCausal) unconflictedView <&> \case
              Right _ -> Nothing
              Left reasons -> Just reasons
          _ -> do
            let currentNamesWithoutLibdeps = Branch.toNames currentNamespaceWithoutLibdeps
            numConstructors <-
              Codebase.getBranchDeclNumConstructors
                env.codebase
                (Branch.namespaceHash currentCausal)
                (Names.typeReferences currentNamesWithoutLibdeps)
            pure case checkAllDeclCoherency (Names.lenientToNametree currentNamesWithoutLibdeps) numConstructors of
              Right _ -> Nothing
              Left reasons -> Just reasons

      pure (defnsInLib, dependentsOfTodo.terms, directDependencies, hashLen, incoherentDeclReasons)

  let currentNames = Branch.toNames currentNamespace
  let ppe = PPED.makePPED (PPE.hqNamer 10 currentNames) (PPE.suffixifyByHash currentNames)

  Cli.respondNumbered $
    Output'Todo
      TodoOutput
        { defnsInLib,
          dependentsOfTodo,
          directDependenciesWithoutNames =
            Defns
              { terms = Set.difference directDependencies.terms (Branch.deepTermReferences currentNamespace),
                types = Set.difference directDependencies.types (Branch.deepTypeReferences currentNamespace)
              },
          hashLen,
          incoherentDeclReasons,
          nameConflicts = Names.conflicts (Branch.toNames currentNamespaceWithoutLibdeps),
          ppe
        }
