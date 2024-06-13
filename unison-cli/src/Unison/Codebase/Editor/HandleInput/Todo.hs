-- | @todo@ input handler
module Unison.Codebase.Editor.HandleInput.Todo
  ( handleTodo,
  )
where

import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.PrettyPrintUtils qualified as Cli
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.Editor.Output
import Unison.Codebase.Editor.TodoOutput qualified as TO
import Unison.Names qualified as Names

handleTodo :: Cli ()
handleTodo = do
  branch0 <- Cli.getCurrentBranch0
  let names0 = Branch.toNames branch0
  let todo =
        TO.TodoOutput
          { nameConflicts = Names.conflicts names0
          }
  pped <- Cli.currentPrettyPrintEnvDecl
  Cli.respondNumbered $ TodoOutput pped todo
