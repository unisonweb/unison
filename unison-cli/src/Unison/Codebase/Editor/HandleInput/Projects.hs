-- | @projects@ input handler
module Unison.Codebase.Editor.HandleInput.Projects
  ( handleProjects,
  )
where

import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Codebase.Editor.Output qualified as Output

handleProjects :: Cli ()
handleProjects = do
  projects <- Cli.runTransaction Queries.loadAllProjects
  Cli.respondNumbered (Output.ListProjects projects)
