-- | Utilities that have to do with constructing pretty-print environments, given stateful information in the Cli monad
-- state/environment, such as the current path.
module Unison.Cli.PrettyPrintUtils
  ( prettyPrintEnvDecl,
  )
where

import Control.Lens
import Control.Monad.Reader (ask)
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import qualified Unison.Codebase as Codebase
import Unison.NamesWithHistory (NamesWithHistory (..))
import Unison.Prelude
import qualified Unison.PrettyPrintEnvDecl as PPE hiding (biasTo)
import qualified Unison.PrettyPrintEnvDecl.Names as PPE

prettyPrintEnvDecl :: NamesWithHistory -> Cli r PPE.PrettyPrintEnvDecl
prettyPrintEnvDecl ns = do
  Cli.Env {codebase} <- ask
  liftIO (Codebase.hashLength codebase) <&> (`PPE.fromNamesDecl` ns)
