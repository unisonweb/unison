-- | Utilities that have to do with constructing pretty-print environments, given stateful information in the Cli monad
-- state/environment, such as the current path.
module Unison.Cli.PrettyPrintUtils
  ( prettyPrintEnvDecl,
    currentPrettyPrintEnvDecl,
  )
where

import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import qualified Unison.Cli.MonadUtils as Cli
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Path (Path)
import qualified Unison.Codebase.Path as Path
import Unison.NamesWithHistory (NamesWithHistory (..))
import Unison.Prelude
import qualified Unison.PrettyPrintEnvDecl as PPE hiding (biasTo)
import qualified Unison.PrettyPrintEnvDecl.Names as PPE
import qualified Unison.Server.Backend as Backend

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
