module Unison.CommandLine.Server.Impl (application) where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Proxy
import Servant.Server
import qualified Servant.Server as Servant
import qualified Unison.Cli.Monad as Cli
import Unison.Codebase.Branch (Branch)
import qualified Unison.Codebase.Branch as Branch
import Unison.Codebase.Editor.HandleInput (handleInput)
import qualified Unison.CommandLine as CommandLine
import qualified Unison.CommandLine.InputPatterns as IP
import Unison.CommandLine.Server.API
import Unison.CommandLine.Server.Types
import qualified Unison.CommandLine.Server.Types as CliServer
import qualified Unison.Server.CodebaseServer as CodebaseServer
import qualified Unison.Util.Pretty as Pretty
import UnliftIO.STM

runCommand :: RunCommandRequest -> CommandLineServer RunCommandResponse
runCommand RunCommandRequest {namespace, args, command} = do
  let numberedArgs = []
  rootBranchVar <- asks rootVar
  let getRoot = Branch.head <$> (atomically $ readTMVar rootBranchVar)
  input <-
    liftIO (CommandLine.parseInput getRoot namespace numberedArgs IP.patternMap (command : args)) >>= \case
      Left err -> throwError (err400 {errBody = BSC.pack $ Pretty.toPlain 120 err})
      Right input -> pure input
  RunCommandResponse . either id id <$> liftCli (handleInput (Right input))

server :: ServerT CommandLineAPI CommandLineServer
server = runCommand

application ::
  ( Maybe CodebaseServer.BaseUrl ->
    Cli.Env
  ) ->
  TMVar (Branch IO) ->
  CodebaseServer.BaseUrl ->
  Application
application buildEnv rootBranchVar baseUrl =
  Servant.serve cliAPI $ hoistServer cliAPI (toServerHandler cliEnv) server
  where
    cliAPI :: Proxy CommandLineAPI
    cliAPI = Proxy
    cliEnv = buildEnv (Just baseUrl)
    toServerHandler :: Cli.Env -> (forall x. CommandLineServer x -> Servant.Handler x)
    toServerHandler cliEnv cls = runReaderT cls (CliServer.Env rootBranchVar cliEnv)

--  TODO: Add some server logging.
-- notifier :: Output -> IO ()
-- notifier _msg = pure ()
-- numberedNotifier :: Output.NumberedOutput -> IO Output.NumberedArgs
-- numberedNotifier _msg = pure mempty
