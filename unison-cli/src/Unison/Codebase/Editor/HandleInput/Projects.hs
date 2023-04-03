-- | @projects@ input handler
module Unison.Codebase.Editor.HandleInput.Projects
  ( handleProjects,
  )
where

import qualified U.Codebase.Sqlite.Queries as Queries
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import qualified Unison.Codebase.Editor.Output as Output

handleProjects :: Cli ()
handleProjects = do
  projects <- Cli.runTransaction Queries.loadAllProjects
  Cli.respondNumbered (Output.ListProjects projects)
