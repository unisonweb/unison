-- | Utilities that have to do with constructing pretty-print environments, given stateful information in the Cli monad
-- state/environment, such as the current path.
module Unison.Cli.PrettyPrintUtils
  ( currentProjectPPED,
    prettyPrintEnvDecl,
    currentPrettyPrintEnvDecl,
  )
where

import Unison.Cli.LoopCache (LoopCache (..))
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Path (Path)
import Unison.Codebase.Path qualified as Path
import Unison.NamesWithHistory (NamesWithHistory (..))
import Unison.Prelude
import Unison.PrettyPrintEnvDecl qualified as PPE hiding (biasTo)
import Unison.PrettyPrintEnvDecl.Names qualified as PPE
import Unison.Server.Backend qualified as Backend

prettyPrintEnvDecl :: NamesWithHistory -> Cli PPE.PrettyPrintEnvDecl
prettyPrintEnvDecl ns =
  Cli.runTransaction Codebase.hashLength <&> (`PPE.fromNamesDecl` ns)

-- | Get a pretty print env decl for the current names at the current path.
currentPrettyPrintEnvDecl :: (Path -> Backend.NameScoping) -> Cli PPE.PrettyPrintEnvDecl
currentPrettyPrintEnvDecl scoping = do
  root' <- Cli.getRootBranch
  currentPath <- Cli.getCurrentPath
  hqLen <- Cli.runTransaction Codebase.hashLength
  pure $ Backend.getCurrentPrettyNames hqLen (scoping (Path.unabsolute currentPath)) root'

currentProjectPPED :: Cli PPE.PrettyPrintEnvDecl
currentProjectPPED = do
  projectBranchPPEDNamesWithoutTransitiveLibs <$> Cli.getLoopCache
