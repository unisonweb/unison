module Unison.CommandLine.Server.Impl (application) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Proxy
import Servant.Server
import qualified Servant.Server as Servant
import Unison.Cli.Monad
import qualified Unison.Cli.Monad as Cli
import Unison.Codebase.Branch (Branch)
import qualified Unison.Codebase.Branch as Branch
import Unison.Codebase.Editor.HandleInput (handleInput)
import qualified Unison.CommandLine as CommandLine
import qualified Unison.CommandLine.InputPatterns as IP
import Unison.CommandLine.Server.API
import Unison.CommandLine.Server.Types
import qualified Unison.Util.Pretty as Pretty
import UnliftIO.STM

runCommand :: RunCommandRequest -> CommandLineServer RunCommandResponse
runCommand RunCommandRequest {namespace, args, command} = do
  let numberedArgs = []
  rootBranchVar <- asks rootVar
  let getRoot = Branch.head <$> (atomically $ readTMVar rootVar)
  input <-
    liftIO (CommandLine.parseInput getRoot namespace numberedArgs IP.patternMap (command : args)) >>= \case
      Left err -> throwError (err400 {errBody = BSC.pack $ Pretty.toPlain 120 err})
      Right input -> pure input
  RunCommandResponse <$> cliToResponse (handleInput (Right input))

server :: ServerT CommandLineAPI CommandLineServer
server = runCommand

application :: Cli.Env -> TMVar (Branch IO) -> Application
application =
  Servant.serve cliAPI $ hoistServer cliAPI toServerHandler server
  where
    cliAPI :: Proxy CommandLineAPI
    cliAPI = Proxy
    toServerHandler :: forall x. CommandLineServer x -> Servant.Handler x
    toServerHandler cls =
      liftIO (runCli _ _ (runExceptT cls)) >>= \case
        (rt, ls) -> case rt of
          Success response -> _
          Continue response -> _
          HaltRepl -> throwError err500
