-- | @todo@ input handler
module Unison.Codebase.Editor.HandleInput.Todo
  ( handleTodo,
  )
where

import Data.Set qualified as Set
import U.Codebase.Sqlite.Operations qualified as Operations
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.PrettyPrintUtils qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.Editor.Output
import Unison.Names qualified as Names
import Unison.Util.Defns (Defns (..))

handleTodo :: Cli ()
handleTodo = do
  -- For now, we don't go through any great trouble to seek out the root of the project branch. Just assume the current
  -- namespace is the root, which will be the case unless the user uses `deprecated.cd`.
  currentNamespace <- Cli.getCurrentBranch0
  let currentNamespaceWithoutLibdeps = Branch.deleteLibdeps currentNamespace

  (hashLen, directDependencies) <-
    Cli.runTransaction do
      hashLen <- Codebase.hashLength
      directDependencies <-
        Operations.directDependenciesOfScope
          Defns
            { terms = Branch.deepTermReferenceIds currentNamespaceWithoutLibdeps,
              types = Branch.deepTypeReferenceIds currentNamespaceWithoutLibdeps
            }
      pure (hashLen, directDependencies)

  ppe <- Cli.currentPrettyPrintEnvDecl

  Cli.respondNumbered $
    Output'Todo
      TodoOutput
        { hashLen,
          dependentsOfTodo = Set.empty,
          directDependenciesWithoutNames =
            Defns
              { terms = Set.difference directDependencies.terms (Branch.deepTermReferences currentNamespace),
                types = Set.difference directDependencies.types (Branch.deepTypeReferences currentNamespace)
              },
          nameConflicts = Names.conflicts (Branch.toNames currentNamespaceWithoutLibdeps),
          ppe
        }
