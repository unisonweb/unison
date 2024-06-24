-- | @todo@ input handler
module Unison.Codebase.Editor.HandleInput.Todo
  ( handleTodo,
  )
where

import Data.Set qualified as Set
import U.Codebase.Sqlite.Operations qualified as Operations
import Unison.Builtin qualified as Builtin
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.PrettyPrintUtils qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.Editor.Output
import Unison.Names qualified as Names
import Unison.Prelude
import Unison.Reference (TermReference)
import Unison.Syntax.Name qualified as Name
import Unison.Util.Defns (Defns (..))
import Unison.Util.Set qualified as Set

handleTodo :: Cli ()
handleTodo = do
  -- For now, we don't go through any great trouble to seek out the root of the project branch. Just assume the current
  -- namespace is the root, which will be the case unless the user uses `deprecated.cd`.
  currentNamespace <- Cli.getCurrentBranch0
  let currentNamespaceWithoutLibdeps = Branch.deleteLibdeps currentNamespace

  (dependentsOfTodo, directDependencies, hashLen) <-
    Cli.runTransaction do
      let todoReference :: TermReference
          todoReference =
            Set.asSingleton (Names.refTermsNamed Builtin.names (Name.unsafeParseText "todo"))
              & fromMaybe (error (reportBug "E260496" "No reference for builtin named 'todo'"))

      -- All type-and-term dependents of the `todo` builtin, but we know they're all terms.
      dependentsOfTodo <-
        Operations.dependentsWithinScope
          (Branch.deepTermReferenceIds currentNamespaceWithoutLibdeps)
          (Set.singleton todoReference)

      directDependencies <-
        Operations.directDependenciesOfScope
          Defns
            { terms = Branch.deepTermReferenceIds currentNamespaceWithoutLibdeps,
              types = Branch.deepTypeReferenceIds currentNamespaceWithoutLibdeps
            }

      hashLen <- Codebase.hashLength

      pure (dependentsOfTodo.terms, directDependencies, hashLen)

  ppe <- Cli.currentPrettyPrintEnvDecl

  Cli.respondNumbered $
    Output'Todo
      TodoOutput
        { hashLen,
          dependentsOfTodo,
          directDependenciesWithoutNames =
            Defns
              { terms = Set.difference directDependencies.terms (Branch.deepTermReferences currentNamespace),
                types = Set.difference directDependencies.types (Branch.deepTypeReferences currentNamespace)
              },
          nameConflicts = Names.conflicts (Branch.toNames currentNamespaceWithoutLibdeps),
          ppe
        }
