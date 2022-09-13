-- | Utilities that have to do with constructing pretty-print environments, given stateful information in the Cli monad
-- state/environment, such as the current path.
module Unison.Cli.PrettyPrintUtils
  ( prettyPrintEnvDecl,
    currentPrettyPrintEnvDecl,
  )
where

import Control.Lens
import Control.Monad.Reader (ask)
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

prettyPrintEnvDecl :: NamesWithHistory -> Cli r PPE.PrettyPrintEnvDecl
prettyPrintEnvDecl ns = do
  Cli.Env {codebase} <- ask
  liftIO (Codebase.hashLength codebase) <&> (`PPE.fromNamesDecl` ns)

-- | Get a pretty print env decl for the current names at the current path.
currentPrettyPrintEnvDecl :: (Path -> Backend.NameScoping) -> Cli r PPE.PrettyPrintEnvDecl
currentPrettyPrintEnvDecl scoping = do
  Cli.Env {codebase} <- ask
  root' <- Cli.getRootBranch
  currentPath <- Cli.getCurrentPath
  hqLen <- liftIO (Codebase.hashLength codebase)
  pure $ Backend.getCurrentPrettyNames hqLen (scoping (Path.unabsolute currentPath)) root'
